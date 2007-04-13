{-# OPTIONS_GHC -fglasgow-exts #-}
import Test.QuickCheck
import Test.IOSpec.IORef
import Data.Dynamic
import Control.Monad

-- We begin by giving an implementation of queues using our pure
-- specification of IORefs.

type Queue = (IORef Data, IORef Data)

data Data  = Cell Int (IORef Data) | NULL deriving Typeable

-- There is one important point here. To use the IORefs in IOSpec,
-- we need to make sure that any data we store in an IORef is an
-- instance of Typeable. Fortunately, GHC can derive instances of
-- Typeable for most data types.

-- The implementation of Queues is fairly standard. We use a linked
-- list, with special pointers to the head and tail of the queue.

emptyQueue :: IOState Queue
emptyQueue  = do  
  front <- newIORef NULL 
  back <- newIORef NULL
  return (front,back)

enqueue :: Queue -> Int -> IOState ()
enqueue (front,back) x = 
  do  newBack <- newIORef NULL
      let cell = Cell x newBack
      c <- readIORef back
      writeIORef back cell 
      case c of
        NULL -> writeIORef front cell
        Cell y t -> writeIORef t cell

dequeue :: Queue -> IOState (Maybe Int)
dequeue (front,back) = do
  c <- readIORef front
  case c of
    NULL -> return Nothing
    (Cell x nextRef) -> do
      next <- readIORef nextRef
      writeIORef front next
      return (Just x)

-- Besides basic queue operations, we also implement queue reversal.

reverseQueue :: Queue -> IOState ()
reverseQueue (front,back) = do
  f <- readIORef front
  case f of
    NULL -> return ()
    Cell x nextRef -> do
      flipPointers NULL (Cell x nextRef)
      f <- readIORef front
      b <- readIORef back
      writeIORef front b
      writeIORef back f

flipPointers :: Data -> Data -> IOState ()
flipPointers prev NULL = return ()
flipPointers prev (Cell x next) = do
      nextCell <- readIORef next
      writeIORef next prev
      flipPointers (Cell x next) nextCell
    
-- A pair of functions that convert lists to queues and vice versa.

queueToList :: Queue -> IOState [Int]
queueToList = unfoldM dequeue

listToQueue :: [Int] -> IOState Queue
listToQueue xs = do q <- emptyQueue
                    sequence_ (map (enqueue q) xs)
                    return q

unfoldM :: Monad m => (a -> m (Maybe x)) -> a -> m [x]
unfoldM f a = do
  x <- f a
  case x of
    Nothing -> return []
    Just x -> liftM (x:) (unfoldM f a)

-- Now we can state a few properties of queues.

inversesProp :: [Int] -> Bool
inversesProp xs = xs == runIOState (listToQueue xs >>= queueToList)

revRevProp xs = runIOState revRevProg == xs
  where
  revRevProg = do q <- listToQueue xs
                  reverseQueue q
                  reverseQueue q
                  queueToList q

revProp xs = runIOState revProg == reverse xs
  where
  revProg = do q <- listToQueue xs
               reverseQueue q
               queueToList q

queueProp1 x = runIOState queueProg1 == Just x
  where
  queueProg1 = do q <- emptyQueue
                  enqueue q x
                  dequeue q

queueProp2 x y = runIOState queueProg2 == Just y
  where
  queueProg2 = do q <- emptyQueue
                  enqueue q x
                  enqueue q y
                  dequeue q
                  dequeue q

main = do putStrLn "Testing first queue property..."
          quickCheck queueProp1
          putStrLn "Testing second queue property..."
          quickCheck queueProp2
          putStrLn "Testing queueToList and listToQueue.."
          quickCheck inversesProp
          putStrLn "Testing that reverseQueue is its own inverse..."
          quickCheck revRevProp
          putStrLn "Testing reverseQueue matches the spec..."
          quickCheck revProp
-- Once we are satisfied with our implementation, we can import the
-- "real" Data.IORef instead of Test.IOSpec.IORef.