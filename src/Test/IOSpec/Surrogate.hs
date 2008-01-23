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

-- | The @IOSpec f a@ is merely type synonym for @IO a@. Once you've
-- tested a module, you can use these definitions to avoid having to
-- change your type signatures.
--
-- Note that because this definition of 'IOSpec' ignores its @f@
-- argument, each of 'ForkS', 'MVarS', etc., is simply an empty data
-- type.
type IOSpec f a  = IO a

data ForkS
data MVarS
data IORefS
data STMS
data Teletype
data (f :+: g)
