-- | This module contains a few type signatures to help replace pure
-- specifications by their effectful counterparts.
module Test.IOSpec.Surrogate
  (
  -- * The IOSpec type
    IOSpec
  -- * The specifications
  , ForkS
  , MVarS
  , IORefS
  , STMS
  , Teletype
  , (:+:)
  )
  where

-- | The 'IOSpec f a' is merely type synonym for 'IO a'.
type IOSpec f a  = IO a

-- | The original pure specifications now contain no information.
data ForkS
data MVarS
data IORefS
data STMS
data Teletype
data (f :+: g)
