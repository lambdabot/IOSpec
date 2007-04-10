{-#  OPTIONS -fglasgow-exts -fno-warn-missing-fields  #-}

-- | A pure specification of mutable variables. 
module Test.IOSpec.IORef 
   (
    -- * The IOState monad
     IOState
   , runIORef
    -- * Manipulation of IORefs
   , IORef
   , newIORef
   , readIORef
   , writeIORef
   , modifyIORef
   ) 
   where

import Control.Monad.State 
import Data.Dynamic
import Data.Maybe (fromJust)

type Data           = Dynamic
type Loc            = Int

-- | The IOState monad

data IOState a  = 
     NewIORef Data (Loc -> IOState a) 
  |  ReadIORef Loc (Data -> IOState a)
  |  WriteIORef Loc Data (IOState  a) 
  |  Return a 

instance Functor IOState where
  fmap f (NewIORef d io)     = NewIORef d (\l -> fmap f (io l))
  fmap f (ReadIORef l io)    = ReadIORef l (\d -> fmap f (io d))
  fmap f (WriteIORef l d io) = WriteIORef l d (fmap f io)
  fmap f (Return x)     = Return (f x)

instance Monad IOState where
  return                    = Return
  (Return a) >>= g          = g a
  (NewIORef d f) >>= g      = NewIORef d (\l -> f l >>= g)
  (ReadIORef l f) >>= g     = ReadIORef l (\d -> f d >>= g)
  (WriteIORef l d s) >>= g  = WriteIORef l d (s >>= g)

-- | A mutable variable in the IOState monad
newtype IORef a = IORef Loc

-- | The 'newIORef' function creates a new mutable variable.
newIORef :: Typeable a => a -> IOState (IORef a)
newIORef d = NewIORef (toDyn d) (Return . IORef)

-- | The 'readIORef' function reads the value stored in a mutable variable.
readIORef :: Typeable a => IORef a -> IOState a
readIORef (IORef l) = ReadIORef l (Return . unsafeFromDynamic)

-- | The 'writeIORef' function overwrites the value stored in an IORef.
writeIORef :: Typeable a => IORef a -> a -> IOState ()
writeIORef (IORef l) d = WriteIORef l (toDyn d) (Return ())

-- | The 'modifyIORef' function applies a function to the value stored in 
-- and IORef.
modifyIORef :: Typeable a => IORef a -> (a -> a) -> IOState ()
modifyIORef ref f = readIORef ref >>= \x -> writeIORef ref (f x)

unsafeFromDynamic :: Typeable a => Dynamic -> a
unsafeFromDynamic = fromJust . fromDynamic

data Store = Store {fresh :: Loc, heap :: Heap}
type Heap = Loc -> Data 

emptyStore :: Store
emptyStore = Store {fresh = 0}

-- | The 'runIORef' function executes a computation in the `IOState' monad.
runIORef :: IOState a -> a
runIORef io = evalState (step io) emptyStore

step :: IOState a -> State Store a
step (Return a) = return a
step (NewIORef d g)      
  = do  loc <- alloc
        extendHeap loc d
        step (g loc) 
step (ReadIORef l g)     
  = do  d <- lookupHeap l
        step (g d)
step (WriteIORef l d p)
  = do  extendHeap l d
        step p

alloc :: State Store Loc 
alloc = do  loc <- gets fresh
            modifyFresh ((+) 1)
            return loc

lookupHeap :: Loc -> State Store Data
lookupHeap l = do  h <- gets heap
                   return (h l)

extendHeap :: Loc -> Data -> State Store ()
extendHeap l d  = modifyHeap (update l d)

modifyHeap :: (Heap -> Heap) -> State Store ()
modifyHeap f = do  s <- get
                   put (s {heap = f (heap s)})

modifyFresh :: (Loc -> Loc) -> State Store ()
modifyFresh f = do  s <- get
                    put (s {fresh = f (fresh s)})

update :: Loc -> Data -> Heap -> Heap
update l d h k
  | l == k       = d
  | otherwise    = h k
