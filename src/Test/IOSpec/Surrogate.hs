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
data Forks
data MVars
data Refs
data STMs
data Teletype
data (f :+: g)
