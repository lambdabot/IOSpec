-- Note that the Prelude and Test.IOSpec.Teletype both export
-- functions called getChar and putChar. To begin with, we hide the
-- definitions in the prelude and work with the pure specification.

import Prelude hiding (getChar, putChar)
import qualified Data.Stream as Stream
import Test.IOSpec.Teletype
import Test.IOSpec.VirtualMachine
import Test.IOSpec.Types
import Test.QuickCheck


-- The echo function, as we have always known it
echo :: IOSpec Teletype ()
echo = getChar >>= putChar >> echo

-- It should echo any character entered at the teletype.  This is
-- the behaviour we would expect echo to have.  The Output data type
-- is defined in Test.IOSpec.Teletype and represents the observable
-- behaviour of a teletype interaction.
copy :: Stream.Stream Char -> Effect ()
copy (Stream.Cons x xs) = ReadChar (Print x (copy xs))

-- An auxiliary function that takes the first n elements printed to
-- the teletype.
takeOutput :: Int -> Effect () -> String
takeOutput 0 _ = ""
takeOutput (n + 1) (Print c xs) = c : takeOutput n xs
takeOutput (n + 1) (ReadChar t) = (takeOutput n t)

-- We can use QuickCheck to test if our echo function meets the
-- desired specification: that is that for every input the user
-- enters, every finite prefix of runTT echo input and copy input is
-- the same.
echoProp :: Int -> Stream.Stream Char -> Property
echoProp n input = 
  n > 0 ==>  
    takeOutput n (executeTeletype echo input) 
    == takeOutput n (copy input)

instance Arbitrary Char where
  arbitrary = choose ('a','z')

main = do putStrLn "Testing echo..."
          let x = executeTeletype echo (Stream.cycle "abcd")
          print (takeOutput 4 x)

-- Once we are satisfied with our definition of echo, we can change
-- our imports. Rather than importing Test.IOSpec.Teletype, we
-- import the "real" getChar and putChar, as defined in the Prelude.