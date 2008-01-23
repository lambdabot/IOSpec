-- | The virtual machine on which the specifications execute.
module Test.IOSpec.VirtualMachine
  (
  -- * The Virtual Machine
   VM
  , Data
  , Loc
  , Scheduler
  , Store
  , ThreadId
  , initialStore
  -- * Primitive operations on the VM
  , alloc
  , emptyLoc
  , freshThreadId
  , finishThread
  , lookupHeap
  , mainTid
  , printChar
  , readChar
  , updateHeap
  , updateSoup
  -- * The observable effects on the VM
  , Effect (..)
  -- * Sample schedulers
  , roundRobin
  , singleThreaded
  -- * Executing code on the VM
  , Executable(..)
  , Step(..)
  , runIOSpec
  , evalIOSpec
  , execIOSpec
  )
  where

import Control.Monad.State
import Data.Dynamic
import Data.List
import qualified Data.Stream as Stream
import Test.IOSpec.Types
import Test.QuickCheck

type Data         = Dynamic
type Loc          = Int
type Heap         = Loc -> Maybe Data

newtype ThreadId  = ThreadId Int deriving (Eq, Show)

instance Arbitrary ThreadId where
  arbitrary                = liftM ThreadId arbitrary
  coarbitrary (ThreadId k) = coarbitrary k

newtype Scheduler =
  Scheduler (Int -> (Int, Scheduler))

instance Arbitrary Scheduler where
  arbitrary   = liftM streamSched arbitrary
  coarbitrary = internalError
    "Test.IOSpec: no definition of coarbitrary for Schedulers."

instance Show Scheduler where
  show _ = "Test.IOSpec.Scheduler"


data ThreadStatus =
     forall f b . Executable f => Running (IOSpec f b)
  |  Finished

type ThreadSoup = ThreadId -> ThreadStatus

data Store =
  Store { fresh :: Loc
        ,  heap :: Heap
        ,  nextTid :: ThreadId
        ,  blockedThreads :: [ThreadId]
        ,  finishedThreads :: [ThreadId]
        ,  scheduler :: Scheduler
        ,  threadSoup :: ThreadSoup
        }

initialStore :: Scheduler -> Store
initialStore sch =
  Store { fresh = 0
        , heap = internalError "Access of unallocated memory "
        , nextTid = ThreadId 1
        , blockedThreads = []
        , finishedThreads = []
        , scheduler = sch
        , threadSoup = internalError "Unknown thread scheduled"
        }

-- Auxiliary functions
modifyFresh :: (Loc -> Loc) -> VM ()
modifyFresh f           = do s <- get
                             put (s {fresh = f (fresh s)})

modifyHeap :: (Heap -> Heap) -> VM ()
modifyHeap f            = do s <- get
                             put (s {heap = f (heap s)})

modifyNextTid :: (ThreadId -> ThreadId) -> VM ()
modifyNextTid f         = do s <- get
                             put (s {nextTid = f (nextTid s)})

modifyScheduler :: (Scheduler -> Scheduler) -> VM ()
modifyScheduler f       = do s <- get
                             put (s {scheduler = f (scheduler s)})

modifyThreadSoup :: (ThreadSoup -> ThreadSoup) -> VM ()
modifyThreadSoup f = do s <- get
                        put (s {threadSoup = f (threadSoup s)})


-- | The 'VM' monad is essentially a state monad, modifying the
-- store. Besides returning pure values, various primitive effects
-- may occur, such as printing characters or failing with an error
-- message.
type VM a = StateT Store Effect a

-- | The 'alloc' function allocate a fresh location on the heap.
alloc :: VM Loc 
alloc = do  loc <- gets fresh
            modifyFresh ((+) 1)
            return loc

-- | The 'emptyLoc' function removes the data stored at a given
-- location. This corresponds, for instance, to emptying an 'MVar'.
emptyLoc :: Loc -> VM ()
emptyLoc l = modifyHeap (update l Nothing)

-- | The 'freshThreadId' function returns a previously unallocated 'ThreadId'.
freshThreadId :: VM ThreadId
freshThreadId = do
  t <- gets nextTid
  modifyNextTid (\(ThreadId n) -> ThreadId (n+1))
  return t

-- | The 'finishThread' function kills the thread with the specified
-- 'ThreadId'.
finishThread :: ThreadId -> VM ()
finishThread tid = modifyThreadSoup (update tid Finished)


-- | The 'lookupHeap' function returns the data stored at a given
-- heap location, if there is any.
lookupHeap :: Loc -> VM (Maybe Data)
lookupHeap l = do  h <- gets heap
                   return (h l)

-- | The 'mainTid' constant is the 'ThreadId' of the main process.
mainTid :: ThreadId
mainTid = ThreadId 0

-- | The 'readChar' and 'printChar' functions are the primitive
-- counterparts of 'getChar' and 'putChar' in the 'VM' monad.
readChar :: VM Char
readChar = StateT (\s -> (ReadChar (\c -> (Done (c,s)))))

printChar :: Char -> VM ()
printChar c = StateT (\s -> (Print c (Done ((),s))))

-- | The 'updateHeap' function overwrites a given location with
-- new data.
updateHeap :: Loc -> Data -> VM ()
updateHeap l d  = modifyHeap (update l (Just d))

-- | The 'updateSoup' function updates the process associated with a
-- given 'ThreadId'.
updateSoup :: Executable f => ThreadId -> IOSpec f a -> VM ()
updateSoup tid p = modifyThreadSoup (update tid (Running p))

update :: Eq a => a -> b -> (a -> b) -> (a -> b)
update l d h k
  | l == k       = d
  | otherwise    = h k

-- | The 'Effect' type contains all the primitive effects that are
-- observable on the virtual machine.
data Effect a = 
    Done a 
  | ReadChar (Char -> Effect a)
  | Print Char (Effect a)
  | Fail String 

instance Functor Effect where
  fmap f (Done x) = Done (f x)
  fmap f (ReadChar t) = ReadChar (\c -> fmap f (t c))
  fmap f (Print c t) = Print c (fmap f t)
  fmap _ (Fail msg) = Fail msg

instance Monad Effect where
  return = Done
  (Done x) >>= f = f x
  (ReadChar t) >>= f = ReadChar (\c -> t c >>= f)
  (Print c t) >>= f = Print c (t >>= f)
  (Fail msg) >>= _ = Fail msg

-- | The 'roundRobin' scheduler simple round-robin scheduler.
roundRobin :: Scheduler
roundRobin = streamSched (Stream.unfold (\k -> (ThreadId k, k+1)) 0)

-- | The 'singleThreaded' scheduler will never schedule forked
-- threads, always scheduling the main thread. Only use this
-- scheduler if your code is not concurrent.
singleThreaded :: Scheduler
singleThreaded = streamSched (Stream.repeat mainTid)

streamSched :: Stream.Stream ThreadId -> Scheduler
streamSched (Stream.Cons (ThreadId x) xs) = 
  Scheduler (\k -> (ThreadId (x `mod` k), streamSched xs))


-- | The 'Executable' type class captures all the different types of
-- operations that can be executed in the 'VM' monad.
class Functor f => Executable f where
  step :: f a -> VM (Step a)

data Step a = Step a | Block

instance (Executable f, Executable g) => Executable (f :+: g) where 
  step (Inl x) = step x
  step (Inr y) = step y

-- The 'execVM' function essentially schedules a thread and allows
-- it to perform a single step. If the main thread is finished, it
-- returns the final result of the comptuation.
execVM :: Executable f => IOSpec f a -> VM a
execVM main = do
  (tid,t) <- schedule main
  case t of
    (Main (Pure x)) -> return x
    (Main (Impure p)) -> do x <- step p
                            case x of
                              Step y -> execVM y
                              Block -> execVM main
    (Aux (Pure _)) -> do finishThread tid
                         execVM main
    (Aux (Impure p)) -> do x <- step p
                           case x of
                             Step y -> updateSoup tid y >> execVM main
                             Block -> execVM main
                             
-- A Process is the result of a call to the scheduler.
data Process a = 
     forall f . Executable f => Main (IOSpec f a)
  |  forall f b . Executable f => Aux (IOSpec f b)

-- Gets the ThreadId of the next thread to schedule.
getNextThreadId :: VM ThreadId
getNextThreadId = do  Scheduler sch <- gets scheduler
                      (ThreadId n) <- gets nextTid
                      let (tid,s) = sch n
                      modifyScheduler (const s)
                      return tid

-- The 'schedule' function tries to schedule an active thread,
-- returning the scheduled thread's ThreadId and the process
-- associated with that id.
schedule :: Executable f => IOSpec f a -> VM (ThreadId, Process a)
schedule main = do  tid <- getNextThreadId
                    if tid == mainTid
                      then return (mainTid, Main main)
                      else do
                        tsoup <- gets threadSoup
                        case tsoup tid of
                          Finished ->  schedule main
                          Running p -> return (tid, Aux p)

-- | The 'runIOSpec' function is the heart of this library.  Given
-- the scheduling algorithm you want to use, it will run a value of
-- type 'IOSpec f a', returning the sequence of observable effects
-- together with the final store.
runIOSpec :: Executable f => IOSpec f a -> Scheduler -> Effect (a, Store)
runIOSpec io sched = runStateT 
                       (execVM io)
                       (initialStore sched)

-- | The 'execIOSpec' returns the final 'Store' after executing a
-- computation.
--
-- /Beware/: this function assumes that your computation will
-- succeed, without any other visible 'Effect'. If your computation
-- reads a character from the teletype, for instance, it will return
-- an error.
execIOSpec :: Executable f => IOSpec f a -> Scheduler -> Store
execIOSpec io sched = 
  case runIOSpec io sched of
    Done (_,s) -> s
    _ -> error $ "Failed application of Test.IOSpec.execIOSpec.\n" ++
                 "Probable cause: your function uses functions such as " ++ 
                 "putChar and getChar. Check the preconditions for calling " ++
                 "this function in the IOSpec documentation."

-- | The 'evalIOSpec' function returns the effects a computation
-- | yields, but discards the final state of the virtual machine.
evalIOSpec :: Executable f => IOSpec f a -> Scheduler -> Effect a
evalIOSpec io sched = fmap fst (runIOSpec io sched)

internalError :: String -> a
internalError msg = error ("IOSpec.VirtualMachine: " ++ msg)
