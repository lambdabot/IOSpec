-- | This module contains a few type signatures to help replace pure
-- specifications by their effectful counterparts.
module Test.IOSpec.Surrogate
  (
  -- * The IOSpec type
    IOSpec
  -- * The specifications
  , Forks
  , MVars
  , Refs
  , STMs
  , Teletype
  , (:+:)
  )
  where

-- | The 'IOSpec f a' is merely type synonym for 'IO a'.
type IOSpec f a  = IO a

-- | The original pure specifications now contain no information.
data Forks a
data MVars a
data Refs a
data STMs a
data Teletype a
data (f :+: g) x
