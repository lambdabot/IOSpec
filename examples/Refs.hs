import Test.IOSpec
import Test.QuickCheck

readOnce :: Int -> IOSpec IORefS Int
readOnce x = do ref <- newIORef x
                readIORef ref

readTwice :: Int -> IOSpec IORefS Int
readTwice x = do ref <- newIORef x
                 readIORef ref
                 readIORef ref

readIORefProp :: Int -> Bool
readIORefProp x =
  let once  = evalIOSpec (readOnce x) singleThreaded
      twice = evalIOSpec (readTwice x) singleThreaded
  in once == twice

main = quickCheck readIORefProp

instance Eq a => Eq (Effect a) where
  (Done x) == (Done y) = x == y
  _ == _ = error "Incomparable effects."