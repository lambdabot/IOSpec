-- | A pure implementation of getChar and putChar.

module Test.IOSpec.Teletype
   (
   -- * The IOTeletype monad
     IOTeletype
   , Output(..)
   , runTT
   -- * Pure getChar and putChar
   , getChar
   , putChar
   ) 
   where

import qualified Data.Stream as Stream
import Prelude hiding (getChar, putChar)

-- | The IOTeletype monad
data IOTeletype a = 
     GetChar (Char -> IOTeletype a)
  |  PutChar Char (IOTeletype a)
  |  ReturnTeletype a

instance Functor IOTeletype where
  fmap f (GetChar tt)       = GetChar (\x -> fmap f (tt x))
  fmap f (PutChar c tt)     = PutChar c (fmap f tt)
  fmap f (ReturnTeletype x) = ReturnTeletype (f x)

instance Monad IOTeletype where
  return = ReturnTeletype
  (ReturnTeletype a)  >>= g     = g a
  (GetChar f)         >>= g     = GetChar (\c -> f c >>= g)
  (PutChar c a)       >>= g     = PutChar c (a >>= g)


-- | Once you have constructed something of type 'IOTeletype' you
-- can run the interaction. If you pass in a stream of characters
-- entered at the teletype, it will produce a value of type 'Output'
runTT :: IOTeletype a -> Stream.Stream Char -> Output a
runTT (ReturnTeletype a) cs  = Finish a
runTT (GetChar f) cs         = runTT (f (Stream.head cs)) (Stream.tail cs)
runTT (PutChar c p) cs       = Print c (runTT p cs)

-- | The result of running a teletype interation is a (potentially
-- infinite) list of characters, that are printed to the screen. The
-- interaction can also end, and return a final value, using the
-- 'Finish' constructor.
data Output a = 
     Print Char (Output a) 
  |  Finish a


-- | The getChar function can be used to read input from the teletype.
getChar    ::  IOTeletype Char 
getChar    =   GetChar ReturnTeletype

-- | The getChar function can be used to print to the teletype.
putChar    ::  Char -> IOTeletype () 
putChar c  =   PutChar c (ReturnTeletype ())

