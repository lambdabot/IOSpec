-- | A pure specification of 'forkIO'.
module Test.IOSpec.Fork
   (
     Forks
   , forkIO
   ) 
   where

import Test.IOSpec.VirtualMachine
import Test.IOSpec.Types

-- | The 'Forks' data type and its instances.
-- An expression of type 'IOSpec Forks a' corresponds to an 'IO'
-- computation that uses 'forkIO' and returns a value of
-- type 'a'. 
--
-- By itself, 'Forks' is not terribly useful. You will probably want
-- to use 'IOSpec (Forks :+: MVars)' or 'IOSpec (Forks :+: STMs)'.
data Forks a =
  forall f b . Executable f => Fork (IOSpec f b) (ThreadId -> a)

instance Functor Forks where 
  fmap f (Fork l io)      = Fork l (f . io)

-- | The 'forkIO' function forks off a new thread. It is not much
-- | use by itself, but may be useful if you combine it with either
-- | 'STM' or 'MVars'.
forkIO :: (Executable f, Forks :<: g) => IOSpec f a -> IOSpec g ThreadId 
forkIO p =  inject (Fork p return)

instance Executable Forks where
  step (Fork t p) = do
    tid <- freshThreadId
    updateSoup tid t
    return (Step (p tid))
