{-# OPTIONS_GHC -fglasgow-exts #-}

import Test.QuickCheck
import Control.Monad
import Data.Maybe (fromJust, isJust)
import Data.List (sort)
import Test.IOSpec.Concurrent
import Data.Dynamic

-- An implementation of channels using MVars
--  see the Awkward Squad for more explanation.

data Data =  Cell Int (MVar Data) deriving Typeable

type Channel = (MVar (MVar Data), MVar (MVar Data))

newChan :: IOConc Channel
newChan = do read <- newEmptyMVar
	     write <- newEmptyMVar
	     hole <- newEmptyMVar
	     putMVar read hole
	     putMVar write hole
	     return (read,write)

putChan :: Channel -> Int -> IOConc ()
putChan (_,write) val = 
  do newHole <- newEmptyMVar
     oldHole <- takeMVar write
     putMVar write newHole
     putMVar oldHole (Cell val newHole)

getChan :: Channel -> IOConc Int
getChan (read,write) = 
  do headVar <- takeMVar read
     Cell val newHead <- takeMVar headVar
     putMVar read newHead
     return val

-- We can now check that data is never lost of duplicated.  We fork
--   off n threads that write an integer to a channel, together with
--   n threads that read from the channel and record the read value
--   in an MVar.  The main thread waits till all the threads have
--   successfully read a value. We can then check that the data
--   written to the channel is the same as the data read from it.

reader ::  Channel -> MVar [Int] -> IOConc ()
reader channel var =  do x <- getChan channel
                         xs <- takeMVar var
                         putMVar var (x:xs)

writer :: Channel -> Int -> IOConc ()
writer channel i = putChan channel i

chanTest :: [Int] -> IOConc [Int]
chanTest ints = do
  ch <- newChan
  result <- newEmptyMVar
  putMVar result []
  forM ints (\i -> forkIO (writer ch i)) 
  replicateM (length ints) (forkIO (reader ch result))
  wait result ints 

wait :: MVar [Int] -> [Int] -> IOConc [Int]
wait var xs  = do
  res <- takeMVar var
  if length res == length xs 
    then return res
    else putMVar var res >> wait var xs

-- Using the streamSched function, we let QuickCheck function as a
-- random scheduler.
chanProp ints stream =
  sort (fromJust (runIOConc (chanTest ints) (streamSched stream))) ==  sort ints


-- A blocking computation. This computation could return Just x or
-- Nothing, depending on the scheduler.
blocked :: Int -> IOConc Int
blocked x = do
  var <- newEmptyMVar
  putMVar var x
  forkIO (takeMVar var)
  takeMVar var

blockedTest = runIOConc (blocked 0) roundRobin



