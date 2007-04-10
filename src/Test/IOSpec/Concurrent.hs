{-#  OPTIONS -fglasgow-exts -fno-warn-missing-fields  #-}
-- | A pure specification of basic concurrency operations.

module Test.IOSpec.Concurrent
   (
   -- * The IOConc monad
     IOConc
   , runIOConc
   -- * Supported functions
   , ThreadId
   , MVar
   , newEmptyMVar
   , takeMVar
   , putMVar
   , forkIO
   -- * Schedulers
   , Scheduler(..)
   , streamSched
   , roundRobin
   )
   where 

import Data.Dynamic
import Data.Maybe (fromJust)
import Data.List (nub)
import Control.Monad.State
import qualified Data.Stream as Stream

-- The IOConc data type and its instances
newtype ThreadId  = ThreadId Int deriving (Eq, Show)
type Data         = Dynamic
type Loc          = Int

data IOConc a = 
     NewEmptyMVar (Loc -> IOConc a) 
  |  TakeMVar Loc (Data -> IOConc a) 
  |  PutMVar Loc Data (IOConc a)
  |  forall b . Fork  (IOConc b) (ThreadId -> IOConc  a)
  |  Return a 

instance Functor IOConc where 
  fmap f (Return x) = Return (f x)
  fmap f (NewEmptyMVar io) = NewEmptyMVar (\l -> fmap f (io l))
  fmap f (TakeMVar l io) = TakeMVar l (\d -> fmap f (io d))
  fmap f (PutMVar l d io) = PutMVar l d (fmap f io)
  fmap f (Fork l io)      = Fork l (\tid -> fmap f (io tid))

instance Monad IOConc where
  return = Return
  (Return x) >>= g       = g x
  (NewEmptyMVar f) >>= g = NewEmptyMVar (\l -> f l >>= g)
  (TakeMVar l f) >>= g   = TakeMVar l (\d -> f d >>= g)
  PutMVar c d f >>= g    = PutMVar c d (f >>= g)
  Fork p1 p2 >>= g       = Fork p1 (\tid -> p2 tid >>= g)

-- | An 'MVar' is a shared, mutable variable.
newtype MVar a = MVar Loc

-- | The 'newEmptyMVar' function creates a new 'MVar' that is initially empty.
newEmptyMVar        :: IOConc (MVar a)
newEmptyMVar        = NewEmptyMVar (Return . MVar)
 
-- | The 'takeMVar' function removes the value stored in an
-- 'MVar'. If the 'MVar' is empty, the thread is blocked.
takeMVar            :: Typeable a => MVar a -> IOConc a
takeMVar (MVar l)   = TakeMVar l (Return . unsafeFromDynamic)

-- | The 'putMVar' function fills an 'MVar' with a new value. If the
-- 'MVar' is not empty, the thread is blocked.
putMVar             :: Typeable a => MVar a -> a -> IOConc ()
putMVar (MVar l) d  = PutMVar l (toDyn d) (Return ())

-- | The 'forkIO' function forks off a new thread.
forkIO              :: IOConc a -> IOConc ThreadId 
forkIO p            = Fork p Return

-- The scheduler and store

-- | A scheduler consists of a function that, given the number of
-- threads, returns the 'ThreadId' of the next scheduled thread,
-- together with a new scheduler.
newtype Scheduler = 
  Scheduler (Int -> (ThreadId, Scheduler))

data ThreadStatus = 
     forall b . Running (IOConc b) 
  |  Finished

type Heap = Loc -> Maybe Data

data Store   = Store    {  fresh :: Loc
                        ,  heap :: Heap
                        ,  nextTid :: ThreadId
                        ,  soup :: ThreadId -> ThreadStatus
                        ,  scheduler :: Scheduler
                        ,  deadThreads :: [ThreadId]
                        }

initStore :: Scheduler -> Store
initStore s   = Store  {   fresh    = 0 
                        ,  nextTid   = ThreadId 1
                        ,  scheduler = s
                        ,  deadThreads = []
                        }

-- | The 'runIOConc' function runs a concurrent computation with a given scheduler.
-- If a deadlock occurs, Nothing is returned.

runIOConc :: IOConc a -> Scheduler -> Maybe a
runIOConc io s = evalState (interleave io) (initStore s)

-- A single step

data Status a = Stop a | Step (IOConc a) | Blocked 

step ::  IOConc a -> State Store (Status a)
step (Return a) = return (Stop a)
step (NewEmptyMVar f)
  = do  loc <- alloc
        modifyHeap (update loc Nothing)
        return (Step (f loc))
step (TakeMVar l f)  
  = do  var <- lookupHeap l
        case var of
          Nothing   ->  return Blocked
          (Just d)  ->  do  emptyMVar l
                            return (Step (f d))
step (PutMVar l d p)   
  = do  var <- lookupHeap l
        case var of
          Nothing   ->  do  fillMVar l d
                            return (Step p)
          (Just d)  ->  return Blocked
step (Fork l r)        
  = do  tid <- freshThreadId
        extendSoup l tid
        return (Step (r tid))

emptyMVar :: Loc -> State Store ()
emptyMVar l = modifyHeap (update l Nothing)

fillMVar :: Loc -> Data -> State Store ()
fillMVar l d = modifyHeap (update l (Just d))

extendSoup :: IOConc a -> ThreadId -> State Store () 
extendSoup p tid = modifySoup (update tid (Running p))

-- Interleaving steps

data Process a = 
     Main (IOConc a)
  |  forall b . Aux (IOConc b)

interleave :: IOConc a -> State Store (Maybe a)
interleave main  
  = do  (tid,t) <- schedule main
        case t of
          Main p -> 
            do  x <- step p
                case x of
                  Stop r   ->  return (Just r)
                  Step p   ->  do resetDeadThreads
                                  interleave p
                  Blocked  ->  do isDeadlock <- detectDeadlock
                                  if isDeadlock 
                                    then return Nothing
                                    else interleave main
          Aux p -> 
            do  x <- step p
                case x of
                  Stop _   ->   do  resetDeadThreads
                                    finishThread tid
                                    interleave main
                  Step q   ->   do  resetDeadThreads
                                    extendSoup q tid
                                    interleave main
                  Blocked  ->   do  recordBlockedThread tid
                                    interleave main

schedule :: IOConc a -> State Store (ThreadId, Process a)
schedule main = do  (ThreadId tid) <- getNextThreadId
                    if tid == 0 
                      then return (ThreadId 0, Main main)
                      else do
                        tsoup <- gets soup
                        case tsoup (ThreadId tid) of
                          Finished ->  schedule main
                          Running p -> return (ThreadId tid, Aux p)
                          

getNextThreadId :: State Store ThreadId
getNextThreadId = do  Scheduler sch <- gets scheduler
                      (ThreadId n) <- gets nextTid
                      let (tid,s) = sch n
                      modifyScheduler (const s)
                      return tid


-- | Given a stream of integers, 'streamSched' builds a
-- scheduler. This is especially useful if you use QuickCheck and
-- generate a random stream; the resulting random scheduler will
-- hopefully cover a large number of interleavings.

streamSched :: Stream.Stream Int -> Scheduler
streamSched xs = 
  Scheduler (\k -> (ThreadId (Stream.head xs `mod` k), streamSched (Stream.tail xs)))


-- | A simple round-robin scheduler.
roundRobin :: Scheduler
roundRobin = streamSched (Stream.unfold (\k -> (k, k+1)) 0)

-- Utilities

freshThreadId :: State Store ThreadId
freshThreadId = do tid <- gets nextTid
                   modifyTid (\(ThreadId k) -> ThreadId (k + 1))
                   return tid

alloc :: State Store Loc 
alloc = do  loc <- gets fresh
            modifyFresh ((+) 1)
            return loc

lookupHeap :: Loc -> State Store (Maybe Data)
lookupHeap l = do  h <- gets heap
                   return (h l)

extendHeap :: Loc -> Data -> State Store ()
extendHeap l d  = modifyHeap (update l (Just d))

finishThread :: ThreadId -> State Store ()
finishThread tid = modifySoup (update tid Finished)

resetDeadThreads :: State Store ()
resetDeadThreads = modifyDeadThreads (const [])

recordBlockedThread :: ThreadId -> State Store ()
recordBlockedThread tid = modifyDeadThreads (tid :)

detectDeadlock :: State Store Bool
detectDeadlock = do deadTids <- gets deadThreads
                    (ThreadId nrThreads) <- gets nextTid
                    return (length (nub deadTids) == nrThreads - 1)
                       

update :: Eq a => a -> b -> (a -> b) -> (a -> b)
update l d h k
  | l == k       = d
  | otherwise    = h k

unsafeFromDynamic :: Typeable a => Dynamic -> a
unsafeFromDynamic = fromJust . fromDynamic

modifyHeap f            = do s <- get
                             put (s {heap = f (heap s)})

modifyScheduler f       = do s <- get
                             put (s {scheduler = f (scheduler s)})

modifyFresh f           = do s <- get
                             put (s {fresh = f (fresh s)})

modifyTid f             = do s <- get
                             put (s {nextTid = f (nextTid s)})
 
modifySoup f            = do s <- get
                             put (s {soup = f (soup s)})

modifyDeadThreads f     = do s <- get
                             put (s {deadThreads = f (deadThreads s)})