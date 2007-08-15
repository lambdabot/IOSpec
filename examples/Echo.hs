-- Note that the Prelude and Test.IOSpec.Teletype both export
-- functions called getChar and putChar. To begin with, we hide the
-- definitions in the prelude and work with the pure specification.

import Prelude hiding (getChar, putChar)
import qualified Data.Stream as Stream
import Test.IOSpec
import Test.QuickCheck

-- The echo function, as we have always known it
echo :: IOSpec Teletype ()
echo = getChar >>= putChar >> echo

-- It should echo any character entered at the teletype.  This is
-- the behaviour we would expect echo to have.  The Output data type
-- is defined in Test.IOSpec.Teletype and represents the observable
-- behaviour of a teletype interaction.
copy :: Effect ()
copy = ReadChar (\x -> Print x copy)

-- An auxiliary function that takes the first n elements printed to
-- the teletype.
takeOutput :: Int -> Effect () -> String
takeOutput 0 _ = ""
takeOutput (n + 1) (Print c xs) = c : takeOutput n xs
takeOutput _ _ = error "Echo.takeOutput"

-- withInput runs an Effect, passing the argument stream of
-- characters as the characters entered to stdin. Any effects left
-- over will be either Print statements, or a final Done result.
withInput :: Stream.Stream Char -> Effect a -> Effect a
withInput stdin (Done x)     = Done x
withInput stdin (Print c e)  = Print c (withInput stdin e)
withInput stdin (ReadChar f) = withInput (Stream.tail stdin) 
                                 (f (Stream.head stdin))

-- We can use QuickCheck to test if our echo function meets the
-- desired specification: that is that for every input the user
-- enters, every finite prefix of runTT echo input and copy input is
-- the same.
echoProp :: Int -> Stream.Stream Char -> Property
echoProp n input =
  n > 0 ==>
    takeOutput n (withInput input (evalIOSpec echo singleThreaded))
    == takeOutput n (withInput input copy)

instance Arbitrary Char where
  arbitrary = choose ('a','z')

main = do
  putStrLn "Testing echo..."
  quickCheck echoProp

-- Once we are satisfied with our definition of echo, we can change
-- our imports. Rather than importing Test.IOSpec.Teletype, we
-- import the "real" getChar and putChar, as defined in the Prelude.