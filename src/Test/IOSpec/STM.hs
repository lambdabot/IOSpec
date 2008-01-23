module Test.IOSpec.STM
   (
   -- * The specification of STM
     STMS
   -- * Atomically
   , atomically
   -- * The STM monad
   , STM
   , TVar
   , newTVar
   , readTVar
   , writeTVar
   , retry
   , orElse
   , check
   )
   where

import Test.IOSpec.VirtualMachine
import Test.IOSpec.Types
import Data.Dynamic
import Data.Maybe (fromJust)
import Control.Monad.State

-- The 'STMS' data type and its instances.
--
-- | An expression of type @IOSpec 'STMS' a@ corresponds to an 'IO'
-- computation that may use 'atomically' and returns a value of type
-- @a@.
--
-- By itself, 'STMS' is not terribly useful. You will probably want
-- to use @IOSpec (ForkS :+: STMS)@.
data STMS a =
  forall b . Atomically (STM b) (b -> a)

instance Functor STMS where
  fmap f (Atomically s io) = Atomically s (f . io)

-- | The 'atomically' function atomically executes an 'STM' action.
atomically     :: (STMS :<: f) => STM a -> IOSpec f a
atomically stm = inject $ Atomically stm (return)

instance Executable STMS where
  step (Atomically stm b) =
    do state <- get
       case runStateT (executeSTM stm) state of
         Done (Nothing,_)         -> return Block
         Done (Just x,finalState) -> put finalState >> return (Step (b x))
         _                        -> internalError "Unsafe usage of STM"

-- The 'STM' data type and its instances.
data STM a =
    STMReturn a
  | NewTVar Data (Loc -> STM a)
  | ReadTVar Loc (Data -> STM a)
  | WriteTVar Loc Data (STM a)
  | Retry
  | OrElse (STM a) (STM a)

instance Functor STM where
  fmap f (STMReturn x)      = STMReturn (f x)
  fmap f (NewTVar d io)     = NewTVar d (fmap f . io)
  fmap f (ReadTVar l io)    = ReadTVar l (fmap f . io)
  fmap f (WriteTVar l d io) = WriteTVar l d (fmap f io)
  fmap _ Retry              = Retry
  fmap f (OrElse io1 io2)   = OrElse (fmap f io1) (fmap f io2)

instance Monad STM where
    return                = STMReturn
    STMReturn a >>= f     = f a
    NewTVar d g >>= f     = NewTVar d (\l -> g l >>= f)
    ReadTVar l g >>= f    = ReadTVar l (\d -> g d >>= f)
    WriteTVar l d p >>= f = WriteTVar l d (p >>= f)
    Retry >>= _           = Retry
    OrElse p q >>= f      = OrElse (p >>= f) (q >>= f)

-- | A 'TVar' is a shared, mutable variable used by STM.
newtype TVar a = TVar Loc

-- | The 'newTVar' function creates a new transactional variable.
newTVar   :: Typeable a => a -> STM (TVar a)
newTVar d = NewTVar (toDyn d) (STMReturn . TVar)

-- | The 'readTVar' function reads the value stored in a
-- transactional variable.
readTVar          :: Typeable a => TVar a -> STM a
readTVar (TVar l) = ReadTVar l (STMReturn . fromJust . fromDynamic)

-- | The 'writeTVar' function overwrites the value stored in a
-- transactional variable.
writeTVar            :: Typeable a => TVar a -> a -> STM ()
writeTVar (TVar l) d = WriteTVar l (toDyn d) (STMReturn ())

-- | The 'retry' function abandons a transaction and retries at some
-- later time.
retry :: STM a
retry = Retry

-- | The 'check' function checks if its boolean argument holds. If
-- the boolean is true, it returns (); otherwise it calls 'retry'.
check       :: Bool -> STM ()
check True  = return ()
check False = retry

-- | The 'orElse' function takes two 'STM' actions @stm1@ and @stm2@ and
-- performs @stm1@. If @stm1@ calls 'retry' it performs @stm2@. If @stm1@
-- succeeds, on the other hand, @stm2@ is not executed.
orElse     :: STM a -> STM a -> STM a
orElse p q = OrElse p q

executeSTM :: STM a -> VM (Maybe a)
executeSTM (STMReturn x)      = return (return x)
executeSTM (NewTVar d io)     = do
  loc <- alloc
  updateHeap loc d
  executeSTM (io loc)
executeSTM (ReadTVar l io)    = do
  (Just d) <- lookupHeap l
  executeSTM (io d)
executeSTM (WriteTVar l d io) = do
  updateHeap l d
  executeSTM io
executeSTM Retry              = return Nothing
executeSTM (OrElse p q)       = do
  state <- get
  case runStateT (executeSTM p) state of
    Done (Nothing,_) -> executeSTM q
    Done (Just x,s)  -> put s >> return (Just x)
    _                -> internalError "Unsafe usage of STM"

internalError :: String -> a
internalError msg = error ("IOSpec.STM: " ++ msg)
