-- | A pure specification of basic operations on MVars.

module Test.IOSpec.MVar
   (
   -- * The 'MVarS' spec
     MVarS
   -- * Supported functions
   , MVar
   , newEmptyMVar
   , takeMVar
   , putMVar
   )
   where

import Data.Dynamic
import Data.Maybe (fromJust)
import Test.IOSpec.Types
import Test.IOSpec.VirtualMachine

-- The 'MVarS' data type and its instances.
--
-- | An expression of type @IOSpec MVarS a@ corresponds to an @IO@
-- computation that uses shared, mutable variables and returns a
-- value of type @a@.
--
-- By itself, 'MVarS' is not terribly useful. You will probably want
-- to use @IOSpec (ForkS :+: MVarS)@.

data MVarS a =
     NewEmptyMVar (Loc -> a)
  |  TakeMVar Loc (Data -> a)
  |  PutMVar Loc Data a

instance Functor MVarS where
  fmap f (NewEmptyMVar io) = NewEmptyMVar (f . io)
  fmap f (TakeMVar l io) = TakeMVar l (f . io)
  fmap f (PutMVar l d io) = PutMVar l d (f io)

-- | An 'MVar' is a shared, mutable variable.
newtype MVar a = MVar Loc deriving Typeable

-- | The 'newEmptyMVar' function creates a new 'MVar' that is initially empty.
newEmptyMVar        :: (Typeable a, MVarS :<: f) => IOSpec f (MVar a)
newEmptyMVar        = inject $ NewEmptyMVar (return . MVar)

-- | The 'takeMVar' function removes the value stored in an
-- 'MVar'. If the 'MVar' is empty, the thread is blocked.
takeMVar            :: (Typeable a, MVarS :<: f) => MVar a -> IOSpec f a
takeMVar (MVar l)   = inject $ TakeMVar l (return . fromJust . fromDynamic)

-- | The 'putMVar' function fills an 'MVar' with a new value. If the
-- 'MVar' is not empty, the thread is blocked.
putMVar             :: (Typeable a, MVarS :<: f) => MVar a -> a -> IOSpec f ()
putMVar (MVar l) d  = inject $ PutMVar l (toDyn d) (return ())

instance Executable MVarS where
  step (NewEmptyMVar t) = do loc <- alloc
                             emptyLoc loc
                             return (Step (t loc))
  step (TakeMVar loc t) = do var <- lookupHeap loc
                             case var of
                               Nothing -> return Block
                               Just x -> do
                                 emptyLoc loc
                                 return (Step (t x))
  step (PutMVar loc d t) = do var <- lookupHeap loc
                              case var of
                                Nothing -> do
                                  updateHeap loc d
                                  return (Step t)
                                Just _ -> return Block
