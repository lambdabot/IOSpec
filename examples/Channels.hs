{-# OPTIONS_GHC -fglasgow-exts #-}
import Test.QuickCheck
import Control.Monad
import Data.Maybe (fromJust, isJust)
import Data.List (sort)
import Test.IOSpec.Concurrent
import Data.Dynamic

-- An implementation of channels using MVars. Simon Peyton Jones's
-- paper "Tackling the Awkward Squad" explains this implementation
-- of queues in a bit more detail.

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
-- off n threads that write an integer to a channel, together with n
-- threads that read from the channel and record the read value in
-- an MVar.  The main thread waits till all the threads have
-- successfully read a value. We can then check that the data
-- written to the channel is the same as the data read from it.

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

-- To actually run concurrent programs, we must choose the scheduler
-- with which to run. At the moment, IOSpec provides a simple
-- round-robin scheduler; alternatively we can write our own
-- scheduler using "streamSched" that takes a stream of integers to
-- a scheduler.

-- Using QuickCheck to generate a random stream, we can use the
-- streamSched to implement a random scheduler -- thereby testing as
-- many interleavings as possible.
chanProp ints stream =
  sort (fromJust (runIOConc (chanTest ints) (streamSched stream))) 
  ==  sort ints

main = do putStrLn "Testing channels..."
          quickCheck chanProp