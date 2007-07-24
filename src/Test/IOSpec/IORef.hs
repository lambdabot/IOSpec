
{-#  OPTIONS -fglasgow-exts  #-}

-- | A pure specification of mutable variables. 
module Test.IOSpec.IORef 
   (
    -- * The IOState monad
     IOState
    -- * Manipulation of IORefs
   , IORef
   , newIORef
   , readIORef
   , writeIORef
   , modifyIORef
   ) 
   where

import Data.Dynamic
import Data.Maybe (fromJust)
import Test.IOSpec.Types
import Test.IOSpec.VirtualMachine 

data IOState a  = 
     NewIORef Data (Loc -> a)
  |  ReadIORef Loc (Data -> a)
  |  WriteIORef Loc Data a

instance Functor IOState where
  fmap f (NewIORef d io)     = NewIORef d (f . io)
  fmap f (ReadIORef l io)    = ReadIORef l (f . io)
  fmap f (WriteIORef l d io) = WriteIORef l d (f io)

-- | A mutable variable in the IOState monad
newtype IORef a = IORef Loc

-- | The 'newIORef' function creates a new mutable variable.
newIORef :: (Typeable a, IOState :<: f) => a -> IOSpec f (IORef a)
newIORef d = inject $ NewIORef (toDyn d) (return . IORef)

-- | The 'readIORef' function reads the value stored in a mutable variable.
readIORef :: (Typeable a, IOState :<:f ) => IORef a -> IOSpec f a
readIORef (IORef l) = inject $ ReadIORef l (return .  fromJust . fromDynamic)

-- | The 'writeIORef' function overwrites the value stored in an IORef.
writeIORef :: (Typeable a, IOState :<: f) => IORef a -> a -> IOSpec f ()
writeIORef (IORef l) d = inject $ WriteIORef l (toDyn d) (return ())

-- | The 'modifyIORef' function applies a function to the value stored in 
-- and IORef.
modifyIORef :: (Typeable a, IOState :<: f) 
  => IORef a -> (a -> a) -> IOSpec f ()
modifyIORef ref f = readIORef ref >>= \x -> writeIORef ref (f x)

-- | The 'Executable' instance for the `IOState' monad.
instance Executable IOState where
  step (NewIORef d t)     = do loc <- alloc
                               updateHeap loc (Just d)
                               return (Step (t loc))
  step (ReadIORef l t)    = do Just d <- lookupHeap l
                               return (Step (t d))
  step (WriteIORef l d t) = do updateHeap l (Just d)
                               return (Step t)