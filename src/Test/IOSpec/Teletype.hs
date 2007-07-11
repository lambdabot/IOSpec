-- | A pure specification of getChar and putChar.
module Test.IOSpec.Teletype
   (
   -- * The IOTeletype monad
     Teletype
   -- * Pure getChar and putChar
   , getChar
   , putChar
   -- * Execution
   , executeTeletype
   ) 
   where

import qualified Data.Stream as Stream (Stream, head, tail)
import Prelude hiding (getChar, putChar)
import Test.IOSpec.Types
import Test.IOSpec.VirtualMachine


-- | The Teletype 
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

executeTeletype :: IOSpec Teletype a -> Effect a
executeTeletype tt = 
  fmap fst (runVM (execute tt) (internalError "executeTeletype"))

internalError msg = error ("IOSpec.Teletype: internal error: " ++ msg)