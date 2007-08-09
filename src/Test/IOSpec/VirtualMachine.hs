-- | A pure specification of mutable variables. 
module Test.IOSpec.VirtualMachine
  ( ThreadId
  , Store
  , Scheduler
  , Data
  , Loc
  , VM
  , Effect (..)
  , printChar
  , readChar
  , runVM 
  , alloc
  , updateHeap
  , lookupHeap
  , freshThreadId
  , updateSoup
  , initialStore
  , emptyLoc
  , Executable(..)
  , Step(..)
  , execVM
  , runIOSpecSingleThreaded
  , runIOSpec
  )
  where 

import Control.Monad.State
import Data.Dynamic
import Data.Stream as Stream
import Test.IOSpec.Types
import Test.QuickCheck

newtype ThreadId  = ThreadId Int deriving (Eq, Show)
type Data         = Dynamic
type Loc          = Int
type Heap         = Loc -> Maybe Data

newtype Scheduler = 
  Scheduler (Int -> (ThreadId, Scheduler))

data ThreadStatus = 
     forall f b . Executable f => Running (IOSpec f b) 
  |  Finished

type ThreadSoup = ThreadId -> ThreadStatus

data Store = Store {  fresh :: Loc
                   ,  heap :: Heap
                   ,  nextTid :: ThreadId
                   ,  scheduler :: Scheduler
                   ,  threadSoup :: ThreadSoup
                   }

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

internalError :: String -> a
internalError msg = error ("IOSpec.VirtualMachine: " ++ msg)

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

type VM a = StateT Store Effect a

lookupHeap :: Loc -> VM (Maybe Data)
lookupHeap l = do  h <- gets heap
                   return (h l)

updateHeap :: Loc -> Maybe Data -> VM ()
updateHeap l d  = modifyHeap (update l d)

emptyLoc :: Loc -> VM ()
emptyLoc loc = updateHeap loc Nothing

updateSoup :: Executable f => ThreadId -> IOSpec f a -> VM ()
updateSoup tid p = modifyThreadSoup (update tid (Running p))

freshThreadId :: VM ThreadId
freshThreadId = do
  t <- gets nextTid
  modifyNextTid (\(ThreadId n) -> ThreadId (n+1))
  return t

alloc :: VM Loc 
alloc = do  loc <- gets fresh
            modifyFresh ((+) 1)
            return loc

update :: Eq a => a -> b -> (a -> b) -> (a -> b)
update l d h k
  | l == k       = d
  | otherwise    = h k


readChar :: VM Char
readChar = StateT (\s -> (ReadChar (\c -> (Done (c,s)))))

-- this function should be nicer somehow...
printChar :: Char -> VM ()
printChar c = StateT (\s -> (Print c (Done ((),s))))
              

runVM :: VM a -> Store -> Effect (a, Store)
runVM vm store = runStateT vm store

initialStore :: Scheduler -> Store
initialStore sch = Store { fresh = 0
                       , heap = internalError "Access of unallocated memory."
                       , nextTid = ThreadId 1
                       , scheduler = sch
                       , threadSoup = internalError "Unknown thread scheduled"
                       }

class Functor f => Executable f where
  step :: f a -> VM (Step a)


data Step a = Step a | Block

instance (Executable f, Executable g) => Executable (f :+: g) where 
  step (Inl x) = step x
  step (Inr y) = step y

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
                             

runIOSpecSingleThreaded :: Executable f => IOSpec f a -> Effect a
runIOSpecSingleThreaded io = evalStateT 
                               (execVM io) 
                               (initialStore (streamSched (Stream.repeat mainTid)))

runIOSpec :: Executable f =>  IOSpec f a -> Scheduler -> Effect a
runIOSpec io xs = evalStateT 
                    (execVM io)
                    (initialStore xs)

instance Arbitrary ThreadId where
  arbitrary                = liftM ThreadId arbitrary
  coarbitrary (ThreadId k) = coarbitrary k

instance Arbitrary Scheduler where
  arbitrary   = liftM streamSched arbitrary
  coarbitrary = internalError 
    "Test.IOSpec: no definition of coarbitrary for Schedulers."

instance Show Scheduler where
  show _ = "Test.IOSpec.Scheduler"

finishThread :: ThreadId -> VM ()
finishThread tid = modifyThreadSoup (update tid Finished)

data Process a = 
     forall f . Executable f => Main (IOSpec f a)
  |  forall f b . Executable f => Aux (IOSpec f b)

getNextThreadId :: VM ThreadId
getNextThreadId = do  Scheduler sch <- gets scheduler
                      (ThreadId n) <- gets nextTid
                      let (tid,s) = sch n
                      modifyScheduler (const s)
                      return tid

schedule :: Executable f => IOSpec f a -> VM (ThreadId, Process a)
schedule main = do  tid <- getNextThreadId
                    if tid == mainTid
                      then return (mainTid, Main main)
                      else do
                        tsoup <- gets threadSoup
                        case tsoup tid of
                          Finished ->  schedule main
                          Running p -> return (tid, Aux p)

mainTid :: ThreadId
mainTid = ThreadId 0

streamSched :: Stream.Stream ThreadId -> Scheduler
streamSched (Stream.Cons (ThreadId x) xs) = 
  Scheduler (\k -> (ThreadId (x `mod` k), streamSched xs))


-- | A simple round-robin scheduler.
--roundRobin :: Scheduler
--roundRobin = streamSched (Stream.unfold (\k -> (k, k+1)) 0)

