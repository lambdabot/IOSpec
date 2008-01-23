-- | A pure specification of 'forkIO'.
module Test.IOSpec.Fork
   (
     ForkS
   , forkIO
   )
   where

import Test.IOSpec.VirtualMachine
import Test.IOSpec.Types

-- The 'ForkS' data type and its instances.
--
-- | An expression of type @IOSpec ForkS a@ corresponds to an 'IO'
-- computation that uses 'forkIO' and returns a value of
-- type 'a'.
--
-- By itself, 'ForkS' is not terribly useful. You will probably want
-- to use @IOSpec (ForkS :+: MVarS)@ or @IOSpec (ForkS :+: STMS)@.
data ForkS a =
  forall f b . Executable f => Fork (IOSpec f b) (ThreadId -> a)

instance Functor ForkS where
  fmap f (Fork l io)      = Fork l (f . io)

-- | The 'forkIO' function forks off a new thread.
forkIO :: (Executable f, ForkS :<: g) => IOSpec f a -> IOSpec g ThreadId
forkIO p =  inject (Fork p return)

instance Executable ForkS where
  step (Fork t p) = do
    tid <- freshThreadId
    updateSoup tid t
    return (Step (p tid))
