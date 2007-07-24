module Test.IOSpec.STM where

import Test.IOSpec.VirtualMachine
import Test.IOSpec.Types
import Data.Dynamic
import Data.Maybe (fromJust)
import Control.Monad.State

newtype TVar a = TVar Loc

data STMSpec a = 
  forall b . Atomically (STM b) (b -> a)

instance Functor STMSpec where
  fmap f (Atomically s io) = Atomically s (f . io)

data STM a =   
    STMReturn a
  | NewTVar Data (Loc -> STM a)
  | ReadTVar Loc (Data -> STM a)
  | WriteTVar Loc Data (STM a)
  | Retry
  | OrElse (STM a) (STM a)

instance Monad STM where 
    return = STMReturn
    STMReturn a >>= f = f a
    NewTVar d g >>= f = NewTVar d (\l -> g l >>= f)
    ReadTVar l g >>= f = ReadTVar l (\d -> g d >>= f)
    WriteTVar l d p >>= f = WriteTVar l d (p >>= f)
    Retry >>= _ = Retry
    OrElse p q >>= f = OrElse (p >>= f) (q >>= f)

newTVar :: Typeable a => a -> STM (TVar a)
newTVar d = NewTVar (toDyn d) (STMReturn . TVar)

readTVar :: Typeable a => TVar a -> STM a
readTVar (TVar l) = ReadTVar l (STMReturn . fromJust . fromDynamic)

writeTVar :: Typeable a => TVar a -> a -> STM ()
writeTVar (TVar l) d = WriteTVar l (toDyn d) (STMReturn ())

retry :: STM a
retry = Retry

orElse :: STM a -> STM a -> STM a
orElse p q = OrElse p q

atomic :: (STMSpec :<: f) => STM a -> IOSpec f a
atomic p = inject $ Atomically p return

executeSTM :: STM a -> VM (Maybe a)
executeSTM (STMReturn x) = return (return x)
executeSTM (NewTVar d io) = do
  loc <- alloc 
  updateHeap loc (Just d)
  executeSTM (io loc)
executeSTM (ReadTVar l io) =
  do (Just d) <- lookupHeap l
     executeSTM (io d)
executeSTM (WriteTVar l d io) = 
  do updateHeap l (Just d)
     executeSTM io           
executeSTM Retry = return Nothing
executeSTM (OrElse p q) = do
  state <- get
  case runStateT (executeSTM p) state of
    Done (Nothing,_) -> executeSTM q
    Done (Just x,s) -> put s >> return (Just x)
    _ -> internalError "Unsafe usage of STM"

internalError :: String -> a
internalError msg = error ("IOSpec.STM: " ++ msg)

atomically :: (STMSpec :<: f) => STM a -> IOSpec f a
atomically stm = inject $ Atomically stm (return)

check :: Bool -> STM ()
check True = return ()
check False = retry

instance Executable STMSpec where
  step (Atomically stm b) = 
    do state <- get
       case runStateT (executeSTM stm) state of
         Done (Nothing,_) -> return Block
         Done (Just x,_) -> return (Step (b x))
         _ -> internalError "Unsafe usage of STM"
       
