module Echo where

import Prelude hiding (getChar, putChar)
import qualified Data.Stream as Stream
import Test.IOSpec.Teletype
import Test.QuickCheck

-- The echo function, as we have always known it
echo :: IOTeletype ()
echo = getChar >>= putChar >> echo

-- It should echo any character entered at the teletype.
copy :: Stream.Stream Char -> Output ()
copy (Stream.Cons x xs) = Print x (copy xs)

-- An auxiliary function that takes the first n elements printed to the teletype
takeOutput :: Int -> Output () -> String
takeOutput 0 _ = ""
takeOutput (n + 1) (Print c xs) = c : takeOutput n xs

-- We can use QuickCheck to test if our echo function meets
-- the desired specification:
echoProp :: Int -> Stream.Stream Char -> Property
echoProp n input = n > 0 ==>  takeOutput n (runTT echo input) == takeOutput n (copy input)

instance Arbitrary Char where
  arbitrary = choose ('a','z')

main = quickCheck echoProp