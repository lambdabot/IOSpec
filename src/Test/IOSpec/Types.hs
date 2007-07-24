{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances#-}
module Test.IOSpec.Types 
  (IOSpec(..), (:+:)(..), (:<:), inject) where

data IOSpec f a = 
    Pure a
  | Impure (f (IOSpec f a))

instance (Functor f) => Monad (IOSpec f) where
  return            = Pure
  (Pure x) >>= f    = f x
  (Impure t) >>= f  = Impure (fmap (>>= f) t)

foldIOSpec :: Functor f => (a -> b) -> (f b -> b) -> IOSpec f a -> b
foldIOSpec pure impure (Pure x)   = pure x
foldIOSpec pure impure (Impure t) = impure (fmap (foldIOSpec pure impure) t)

-- Coproducts of functors
data (f :+: g) x = Inl (f x) | Inr (g x)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr y) = Inr (fmap f y)

-- Subtyping hackery

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