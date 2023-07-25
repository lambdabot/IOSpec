{-# LANGUAGE OverlappingInstances, TypeOperators, FlexibleInstances #-}
-- | This module contains the basic data types underlying the
-- 'IOSpec' library. Most of the types and classes in this module
-- are described in
-- <https://webspace.science.uu.nl/~swier004//publications/2008-jfp.pdf>
module Test.IOSpec.Types
  (
  -- * The 'IOSpec' type.
    IOSpec(..)
  , foldIOSpec
  -- * Coproducts of functors
  , (:+:)(..)
  -- * Injections from one functor to another
  , (:<:)
  , inject
  ) where

import Control.Monad (ap)

-- | A value of type 'IOSpec' @f@ @a@ is either a pure value of type @a@
-- or some effect, determined by @f@. Crucially, 'IOSpec' @f@ is a
-- monad, provided @f@ is a functor.
data IOSpec f a =
    Pure a
  | Impure (f (IOSpec f a))

instance (Functor f) => Functor (IOSpec f) where
  fmap f (Pure x)   = Pure (f x)
  fmap f (Impure t) = Impure (fmap (fmap f) t)

instance (Functor f) => Applicative (IOSpec f) where
  pure             = Pure
  (<*>)            = ap

instance (Functor f) => Monad (IOSpec f) where
  return           = Pure
  (Pure x) >>= f   = f x
  (Impure t) >>= f = Impure (fmap (>>= f) t)

-- | The fold over 'IOSpec' values.
foldIOSpec :: Functor f => (a -> b) -> (f b -> b) -> IOSpec f a -> b
foldIOSpec pure _      (Pure x)    = pure x
foldIOSpec pure impure (Impure t)  = impure (fmap (foldIOSpec pure impure) t)

-- | The coproduct of functors
data (f :+: g) x = Inl (f x) | Inr (g x)

infixr 5 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr y) = Inr (fmap f y)

-- | The (:<:) class

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => (:<:) f f where
  inj = id

instance (Functor f, Functor g) => (:<:) f (f :+: g) where
  inj = Inl

instance ((:<:) f g, Functor f, Functor g, Functor h)
  => (:<:) f (h :+: g) where
    inj = Inr . inj

inject :: (g :<: f) => g (IOSpec f a) -> IOSpec f a
inject = Impure . inj
