-- | A pure specification of getChar and putChar.
module Test.IOSpec.Teletype
   (
   -- * The IOTeletype monad
     Teletype
   -- * Pure getChar and putChar
   , getChar
   , putChar
   ) 
   where

import Prelude hiding (getChar, putChar)
import Test.IOSpec.Types
import Test.IOSpec.VirtualMachine

-- | The Teletype type and its instance
data Teletype a = 
     GetChar (Char -> a)
  |  PutChar Char a

instance Functor Teletype where
  fmap f (GetChar tt)       = GetChar (f . tt)
  fmap f (PutChar c tt)     = PutChar c (f tt)

-- | The getChar function can be used to read input from the teletype.
getChar    :: (:<:) Teletype f => IOSpec f Char 
getChar    = inject (GetChar return)

-- | The getChar function can be used to print to the teletype.
putChar    ::  (Teletype :<: f) => Char -> IOSpec f ()
putChar c  =   inject (PutChar c (return ()))

instance Executable Teletype where
  step (GetChar f)   = do c <- readChar
                          return (Step (f c))
  step (PutChar c a) = do printChar c
                          return (Step a)