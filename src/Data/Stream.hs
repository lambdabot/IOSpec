-- | A module defining infinite lists. Most operations on streams
-- are completely analogous to the definition in 'Data.List'

module Data.Stream
   (
     Stream(..) 
   , head 
   , tail
   , intersperse 
   , iterate
   , repeat
   , cycle
   , unfold 
   , take
   , drop
   , splitAt
   , takeWhile
   , dropWhile
   , span
   , break
   , isPrefixOf
   , filter
   , partition
   , (!!)
   , zip
   , zipWith
   , unzip
   , words
   , unwords
   , lines
   , unlines
   , listToStream
   , streamToList
   )
   where

import Prelude hiding (head, tail, iterate, take, drop, takeWhile,
  dropWhile, repeat, cycle, filter, (!!), zip, unzip,
  zipWith,words,unwords,lines,unlines, break, span, splitAt)

import Control.Applicative
import Data.Char (isSpace)
import Test.QuickCheck

data Stream a = Cons a (Stream a) deriving (Show, Eq)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Stream where
  pure = repeat
  (<*>) = zipWith ($)

instance Arbitrary a => Arbitrary (Stream a) where
  arbitrary = do  x <- arbitrary
                  xs <- arbitrary
                  return (Cons x xs)
  coarbitrary = coarbitrary . streamToList

head :: Stream a -> a
head (Cons x _ ) = x

tail :: Stream a -> Stream a
tail (Cons _ xs) = xs

intersperse :: a -> Stream a -> Stream a
intersperse y (Cons x xs) = Cons x (Cons y (intersperse y xs))

unfold :: (c -> (a,c)) -> c -> Stream a
unfold f c = 
  let (x,d) = f c 
  in Cons x (unfold f d)
          
iterate :: (a -> a) -> a -> Stream a
iterate f x = Cons x (iterate f (f x))

take :: Int -> Stream a  -> [a]
take n (Cons x xs)
  | n == 0    = []
  | n > 0     =  x : (take (n - 1) xs)
  | otherwise = error "Stream.take: negative argument."

drop n xs
  | n == 0    = xs
  | n > 0     = drop (n - 1) (tail xs)
  | otherwise = error "Stream.drop: negative argument."

takeWhile :: (a -> Bool) -> Stream a -> [a]
takeWhile p (Cons x xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile p (Cons x xs)
  | p x       = dropWhile p xs
  | otherwise = Cons x xs

repeat :: a -> Stream a
repeat x = Cons x (repeat x)

cycle :: [a] -> Stream a
cycle xs = foldr Cons (cycle xs) xs

filter :: (a -> Bool) -> Stream a -> Stream a
filter p (Cons x xs)
  | p x       = Cons x (filter p xs)
  | otherwise = filter p xs

(!!) :: Int -> Stream a -> a
(!!) n (Cons x xs)
  | n == 0    = x
  | n > 0     = (!!) (n - 1) xs
  | otherwise = error "Stream.!! negative argument"

zip :: Stream a -> Stream b -> Stream (a,b)
zip (Cons x xs) (Cons y ys) = Cons (x,y) (zip xs ys)

unzip :: Stream (a,b) -> (Stream a, Stream b)
unzip (Cons (x,y) xys) = (Cons x (fst (unzip xys)),
                                Cons y (snd (unzip xys)))     

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)

span :: (a -> Bool) -> Stream a -> ([a], Stream a)
span p (Cons x xs)
  | p x       = let (trues, falses) = span p xs
                in (x : trues, falses)
  | otherwise = ([], Cons x xs)

break :: (a -> Bool) -> Stream a -> ([a], Stream a)
break p = span (not . p)

words :: Stream Char -> Stream String
words xs = let (w, ys) = break isSpace xs
                 in Cons w (words ys)

unwords :: Stream String -> Stream Char
unwords (Cons x xs) = foldr Cons (Cons ' ' (unwords xs)) x

lines :: Stream Char -> Stream String
lines xs = let (l, ys) = break (== '\n') xs
                 in Cons l (lines (tail ys))

unlines :: Stream String -> Stream Char
unlines (Cons x xs) = foldr Cons (Cons '\n' (unlines xs)) x

isPrefixOf :: Eq a => [a] -> Stream a -> Bool
isPrefixOf [] _ = True
isPrefixOf (y:ys) (Cons x xs)
  | y == x    = isPrefixOf ys xs
  | otherwise = False

partition :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
partition p (Cons x xs) = 
  let (trues,falses) = partition p xs
  in if p x then (Cons x trues, falses)
            else (trues, Cons x falses)

inits :: Stream a -> Stream ([a])
inits (Cons x xs) = Cons [] (fmap (x:) (inits xs))

tails :: Stream a -> Stream (Stream a)
tails xs = Cons xs (tails (tail xs))

splitAt :: Int -> Stream a -> ([a], Stream a)
splitAt n xs
  | n == 0    = ([],xs)
  | n > 0     = let (prefix,rest) = splitAt (n-1) (tail xs)
                in (head xs : prefix, rest)
  | otherwise = error "Stream.splitAt negative argument."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

listToStream (x:xs) = Cons xs (listToStream xs)
listToStream []     = error "Stream.listToStream applied to finite list"

