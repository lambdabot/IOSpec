{-# LANGUAGE FlexibleContexts #-}
-- | A pure specification of getChar and putChar.
module Test.IOSpec.Teletype
   (
   -- * The IOTeletype monad
     Teletype
   -- * Pure getChar and putChar
   , getChar
   , putChar
   , putStr
   , putStrLn
   , getLine
   )
   where

import Prelude hiding (getChar, putChar, putStr, putStrLn, getLine)
import Control.Monad (forM_)
import Test.IOSpec.Types
import Test.IOSpec.VirtualMachine

-- The 'Teletype' specification.
--
-- | An expression of type 'IOSpec' 'Teletype' @a@ corresponds to an @IO@
-- computation that may print to or read from stdout and stdin
-- respectively.
--
-- There is a minor caveat here. I assume that stdin and stdout are
-- not buffered. This is not the standard behaviour in many Haskell
-- compilers.
data Teletype a =
     GetChar (Char -> a)
  |  PutChar Char a

instance Functor Teletype where
  fmap f (GetChar tt)       = GetChar (f . tt)
  fmap f (PutChar c tt)     = PutChar c (f tt)

-- | The 'getChar' function can be used to read a character from the
-- teletype.
getChar    :: (:<:) Teletype f => IOSpec f Char
getChar    = inject (GetChar return)

-- | The 'getChar' function can be used to print a character to the
-- teletype.
putChar    ::  (Teletype :<: f) => Char -> IOSpec f ()
putChar c  =   inject (PutChar c (return ()))

instance Executable Teletype where
  step (GetChar f)   = do
    c <- readChar
    return (Step (f c))
  step (PutChar c a) = do
    printChar c
    return (Step a)

putStr :: (Teletype :<: f) => String -> IOSpec f ()
putStr str = forM_ str putChar

putStrLn :: (Teletype :<: f) => String -> IOSpec f ()
putStrLn str = putStr str >> putChar '\n'

getLine :: (Teletype :<: f) => IOSpec f String
getLine = do
  c <- getChar
  if c == '\n'
    then return []
    else getLine >>= \line -> return (c : line)
