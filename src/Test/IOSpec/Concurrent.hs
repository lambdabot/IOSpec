-- | A pure specification of basic concurrency operations.

module Test.IOSpec.Concurrent
   (
   -- * The IOConc monad
     IOMVar
   -- * Supported functions
   , MVar
   , newEmptyMVar
   , takeMVar
   , putMVar
   )
   where 

import Data.Dynamic
import Data.Maybe (fromJust)
import Control.Monad.State
import Test.IOSpec.Types
import Test.IOSpec.VirtualMachine 
import qualified Data.Stream as Stream

-- The IOConc data type and its instances

data IOMVar a = 
     NewEmptyMVar (Loc -> a) 
  |  TakeMVar Loc (Data -> a) 
  |  PutMVar Loc Data a

instance Functor IOMVar where 
  fmap f (NewEmptyMVar io) = NewEmptyMVar (f . io)
  fmap f (TakeMVar l io) = TakeMVar l (f . io)
  fmap f (PutMVar l d io) = PutMVar l d (f io)

-- | An 'MVar' is a shared, mutable variable.
newtype MVar a = MVar Loc deriving Typeable

-- | The 'newEmptyMVar' function creates a new 'MVar' that is initially empty.
newEmptyMVar        :: (Typeable a, IOMVar :<: f) => IOSpec f (MVar a)
newEmptyMVar        = inject $ NewEmptyMVar (return . MVar)
 
-- | The 'takeMVar' function removes the value stored in an
-- 'MVar'. If the 'MVar' is empty, the thread is blocked.
takeMVar            :: (Typeable a, IOMVar :<: f) => MVar a -> IOSpec f a
takeMVar (MVar l)   = inject $ TakeMVar l (return . fromJust . fromDynamic)

-- | The 'putMVar' function fills an 'MVar' with a new value. If the
-- 'MVar' is not empty, the thread is blocked.
putMVar             :: (Typeable a, IOMVar :<: f) => MVar a -> a -> IOSpec f ()
putMVar (MVar l) d  = inject $ PutMVar l (toDyn d) (return ())

instance Executable IOMVar where
  step (NewEmptyMVar t) = do loc <- alloc
                             emptyMVar loc
                             return (Step (t loc))
  step (TakeMVar loc t) = do var <- lookupHeap loc
                             case var of
                               Nothing -> return Block
                               Just x -> do
                                 emptyMVar loc
                                 return (Step (t x))                                  
  step (PutMVar loc d t) = do var <- lookupHeap loc
                              case var of
                                Nothing -> do
                                  updateHeap loc (Just d)
                                  return (Step t)
                                Just x -> return Block

emptyMVar :: Loc -> VM ()
emptyMVar loc = updateHeap loc Nothing


-- runIOMVar :: IOMVar a -> Scheduler -> Maybe a
-- runIOMVar io s = evalState (interleave io) (initStore s)

-- -- A single step

-- data Status a = Stop a | Step (IOMVar a) | Blocked 

-- step ::  IOMVar a -> State Store (Status a)
-- step (Return a) = return (Stop a)
-- step (NewEmptyMVar f)
--   = do  loc <- alloc
--         modifyHeap (update loc Nothing)
--         return (Step (f loc))
-- step (TakeMVar l f)  
--   = do  var <- lookupHeap l
--         case var of
--           Nothing   ->  return Blocked
--           (Just d)  ->  do  emptyMVar l
--                             return (Step (f d))
-- step (PutMVar l d p)   
--   = do  var <- lookupHeap l
--         case var of
--           Nothing   ->  do  fillMVar l d
--                             return (Step p)
--           (Just d)  ->  return Blocked
-- step (Fork l r)        
--   = do  tid <- freshThreadId
--         extendSoup l tid
--         return (Step (r tid))

-- emptyMVar :: Loc -> State Store ()
-- emptyMVar l = modifyHeap (update l Nothing)

-- fillMVar :: Loc -> Data -> State Store ()
-- fillMVar l d = modifyHeap (update l (Just d))

-- extendSoup :: IOMVar a -> ThreadId -> State Store () 


-- -- Interleaving steps

-- data Process a = 
--      Main (IOMVar a)
--   |  forall b . Aux (IOMVar b)

-- interleave :: IOMVar a -> State Store (Maybe a)
-- interleave main  
--   = do  (tid,t) <- schedule main
--         case t of
--           Main p -> 
--             do  x <- step p
--                 case x of
--                   Stop r   ->  return (Just r)
--                   Step p   ->  do resetBlockedThreads
--                                   interleave p
--                   Blocked  ->  do isDeadlock <- detectDeadlock
--                                   if isDeadlock 
--                                     then return Nothing
--                                     else interleave main
--           Aux p -> 
--             do  x <- step p
--                 case x of
--                   Stop _   ->   do  resetBlockedThreads
--                                     finishThread tid
--                                     interleave main
--                   Step q   ->   do  resetBlockedThreads
--                                     extendSoup q tid
--                                     interleave main
--                   Blocked  ->   do  recordBlockedThread tid
--                                     interleave main

-- schedule :: IOMVar a -> State Store (ThreadId, Process a)
-- schedule main = do  (ThreadId tid) <- getNextThreadId
--                     if tid == 0 
--                       then return (ThreadId 0, Main main)
--                       else do
--                         tsoup <- gets soup
--                         case tsoup (ThreadId tid) of
--                           Finished ->  schedule main
--                           Running p -> return (ThreadId tid, Aux p)
                          

-- getNextThreadId :: State Store ThreadId
-- getNextThreadId = do  Scheduler sch <- gets scheduler
--                       (ThreadId n) <- gets nextTid
--                       let (tid,s) = sch n
--                       modifyScheduler (const s)
--                       return tid


-- -- | Given a stream of integers, 'streamSched' builds a
-- -- scheduler. This is especially useful if you use QuickCheck and
-- -- generate a random stream; the resulting random scheduler will
-- -- hopefully cover a large number of interleavings.

-- streamSched :: Stream.Stream Int -> Scheduler
-- streamSched xs = 
--   Scheduler (\k -> (ThreadId (Stream.head xs `mod` k), streamSched (Stream.tail xs)))


-- -- | A simple round-robin scheduler.
-- roundRobin :: Scheduler
-- roundRobin = streamSched (Stream.unfold (\k -> (k, k+1)) 0)

-- -- Utilities

-- freshThreadId :: State Store ThreadId
-- freshThreadId = do tid <- gets nextTid
--                    modifyTid (\(ThreadId k) -> ThreadId (k + 1))
--                    return tid

-- alloc :: State Store Loc 
-- alloc = do  loc <- gets fresh
--             modifyFresh ((+) 1)
--             return loc

-- lookupHeap :: Loc -> State Store (Maybe Data)
-- lookupHeap l = do  h <- gets heap
--                    return (h l)

-- extendHeap :: Loc -> Data -> State Store ()
-- extendHeap l d  = modifyHeap (update l (Just d))

-- finishThread :: ThreadId -> State Store ()
-- finishThread tid = modifySoup (update tid Finished)

-- resetBlockedThreads :: State Store ()
-- resetBlockedThreads = modifyBlockedThreads (const [])

-- recordBlockedThread :: ThreadId -> State Store ()
-- recordBlockedThread tid = do 
--   tids <- gets blockedThreads
--   if tid `elem` tids 
--     then return ()
--     else modifyBlockedThreads (tid :)

-- detectDeadlock :: State Store Bool
-- detectDeadlock = do blockedThreads <- liftM length (gets blockedThreads)                   
--                     (ThreadId nrThreads) <- gets nextTid
--                     threadSoup <- gets soup
--                     let allThreadIds = [ThreadId x | x <- [1 .. (nrThreads - 1)]]
--                     let finishedThreads = length $ filter isFinished (map threadSoup allThreadIds)
--                     return (blockedThreads + finishedThreads == nrThreads - 1)

-- isFinished :: ThreadStatus -> Bool
-- isFinished Finished = True
-- isFinished _        = False
