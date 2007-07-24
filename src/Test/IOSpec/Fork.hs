{-# OPTIONS -fglasgow-exts #-}
module Test.IOSpec.Fork
   (
     Fork
   , forkIO
   ) 
   where

import Test.IOSpec.VirtualMachine
import Test.IOSpec.Types

data Fork a =
  forall f b . Executable f => Fork (IOSpec f b) (ThreadId -> a)

instance Functor Fork where 
  fmap f (Fork l io)      = Fork l (f . io)

-- | The 'forkIO' function forks off a new thread.
forkIO :: (Executable f, Fork :<: g) => IOSpec f a -> IOSpec g ThreadId 
forkIO p =  inject (Fork p return)

instance Executable Fork where
  step (Fork t p) = do
    tid <- freshThreadId
    updateSoup tid t
    return (Step (p tid))
