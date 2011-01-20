{-# LANGUAGE FlexibleContexts #-}
-- | A pure specification of mutable variables.
module Test.IOSpec.IORef
   (
    -- * The 'IORefS' spec
     IORefS
    -- * Manipulation and creation of IORefs
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


-- The 'IORefS' spec.
-- | An expression of type @IOSpec IORefS a@ corresponds to an @IO@
-- computation that uses mutable references and returns a value of
-- type @a@.
data IORefS a  =
     NewIORef Data (Loc -> a)
  |  ReadIORef Loc (Data -> a)
  |  WriteIORef Loc Data a

instance Functor IORefS where
  fmap f (NewIORef d io)     = NewIORef d (f . io)
  fmap f (ReadIORef l io)    = ReadIORef l (f . io)
  fmap f (WriteIORef l d io) = WriteIORef l d (f io)

-- | A mutable variable storing a value of type a. Note that the
-- types stored by an 'IORef' are assumed to be @Typeable@.
newtype IORef a = IORef Loc

-- | The 'newIORef' function creates a new mutable variable.
newIORef :: (Typeable a, IORefS :<: f) => a -> IOSpec f (IORef a)
newIORef d = inject $ NewIORef (toDyn d) (return . IORef)

-- | The 'readIORef' function reads the value stored in a mutable variable.
readIORef :: (Typeable a, IORefS :<:f ) => IORef a -> IOSpec f a
readIORef (IORef l) = inject $ ReadIORef l (return .  fromJust . fromDynamic)

-- | The 'writeIORef' function overwrites the value stored in a
-- mutable variable.
writeIORef :: (Typeable a, IORefS :<: f) => IORef a -> a -> IOSpec f ()
writeIORef (IORef l) d = inject $ WriteIORef l (toDyn d) (return ())

-- | The 'modifyIORef' function applies a function to the value stored in
-- and 'IORef'.
modifyIORef :: (Typeable a, IORefS :<: f)
  => IORef a -> (a -> a) -> IOSpec f ()
modifyIORef ref f = readIORef ref >>= \x -> writeIORef ref (f x)

-- | The 'Executable' instance for the `IORefS' monad.
instance Executable IORefS where
  step (NewIORef d t)     = do loc <- alloc
                               updateHeap loc d
                               return (Step (t loc))
  step (ReadIORef l t)    = do Just d <- lookupHeap l
                               return (Step (t d))
  step (WriteIORef l d t) = do updateHeap l d
                               return (Step t)

