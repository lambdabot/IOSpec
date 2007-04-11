import Control.Monad.State 
import Test.QuickCheck
import Test.IOSpec.IORef
import Data.Dynamic

-- An implementation of queues using IORefs

data Data  = Cell Int (IORef Data) | NULL deriving Typeable

type Queue = (IORef Data, IORef Data)

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

-- Besides basic queue operations, we also implement queue reversal

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

-- A few QuickCheck properties.

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

main = do putStrLn "Testing first queue property."
          quickCheck queueProp1
          putStrLn "Testing second queue property."
          quickCheck queueProp2
          putStrLn "Testing queueToList and listToQueue"
          quickCheck inversesProp
          putStrLn "Testing that reverseQueue is its own inverse."
          quickCheck revRevProp
          putStrLn "Testing reverseQueue matches the spec."
          quickCheck revProp

