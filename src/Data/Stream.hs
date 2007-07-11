-- | Streams are infinite lists. Most operations on streams are
-- completely analogous to the definition in Data.List.

module Data.Stream
   (
   -- * The type of streams
     Stream(..) 
   -- * Basic functions
   , head 
   , tail
   -- * Stream transformations
   , map
   , intersperse 
   -- * Building streams
   , iterate
   , repeat
   , cycle
   , unfold 
   -- * Extracting sublists
   , take
   , drop
   , splitAt
   , takeWhile
   , dropWhile
   , span
   , break
   , filter
   , partition
   -- * Sublist predicates
   , isPrefixOf
   -- * Indexing streams
   , (!!)
   -- * Zipping and unzipping streams
   , zip
   , zipWith
   , unzip
   -- * Functions on streams of characters
   , words
   , unwords
   , lines
   , unlines
   -- * Converting to and from an infinite list
   , listToStream
   , streamToList
   )
   where

import Prelude hiding (head, tail, map, iterate, take, drop, takeWhile,
  dropWhile, repeat, cycle, filter, (!!), zip, unzip,
  zipWith,words,unwords,lines,unlines, break, span, splitAt)

import Control.Applicative
import Data.Char (isSpace)

-- | An infinite sequence.
data Stream a = Cons a (Stream a) deriving (Show, Eq)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Stream where
  pure = repeat
  (<*>) = zipWith ($)

instance Monad Stream where
  return = repeat
  (Cons x xs) >>= f = Cons (head (f x)) (tail (xs >>= f))


-- | Extract the first element of the sequence.
head :: Stream a -> a
head (Cons x _ ) = x

-- | Extract the sequence following the head of the stream.
tail :: Stream a -> Stream a
tail (Cons _ xs) = xs

intersperse :: a -> Stream a -> Stream a
intersperse y (Cons x xs) = Cons x (Cons y (intersperse y xs))

-- | Apply a function uniformly over all elements of a sequence.
map :: (a -> b) -> Stream a -> Stream b
map f (Cons x xs) = Cons (f x) (map f xs)

-- | The unfold function is similar to the unfold for lists. Note there is no base case: all streams must be infinite.
unfold :: (c -> (a,c)) -> c -> Stream a
unfold f c = 
  let (x,d) = f c 
  in Cons x (unfold f d)

-- | 'iterate' @f@ @x@ function produces the infinite sequence
-- of repeated applications of @f@ to @x@.
--  
-- > iterate f x = [x, f x, f (f x), ..]
          
iterate :: (a -> a) -> a -> Stream a
iterate f x = Cons x (iterate f (f x))

-- | 'take' @n@ @xs@ returns the first @n@ elements of @xs@.
take :: Int -> Stream a  -> [a]
take n (Cons x xs)
  | n == 0    = []
  | n > 0     =  x : (take (n - 1) xs)
  | otherwise = error "Stream.take: negative argument."

-- | 'drop' @n@ @xs@ drops the first @n@ elements off the front of the sequence @xs@.
drop n xs
  | n == 0    = xs
  | n > 0     = drop (n - 1) (tail xs)
  | otherwise = error "Stream.drop: negative argument."

-- | 'takeWhile' @p@ @xs@ returns the longest prefix of the stream @xs@ for which the predicate @p@ holds.
takeWhile :: (a -> Bool) -> Stream a -> [a]
takeWhile p (Cons x xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

-- | 'dropWhile' @p@ @xs@ returns the suffix remaining after 'takeWhile' @p@ @xs@.
dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile p (Cons x xs)
  | p x       = dropWhile p xs
  | otherwise = Cons x xs

-- | 'repeat' @x@ returns a constant stream, where all elements are equal to @x@.
repeat :: a -> Stream a
repeat x = Cons x (repeat x)

-- | 'cycle' @xs@ returns the infinite repetition of @xs@:
--
-- > cycle [1,2,3] = Cons 1 (Cons 2 (Cons 3 (Cons 1 (Cons 2 ...
cycle :: [a] -> Stream a
cycle xs = foldr Cons (cycle xs) xs

-- | 'filter' @p@ @xs@, removes any elements from @xs@ that do not satisfy @p@.
filter :: (a -> Bool) -> Stream a -> Stream a
filter p (Cons x xs)
  | p x       = Cons x (filter p xs)
  | otherwise = filter p xs

-- | @xs !! n@ returns the element of the stream @xs@ at index
-- @n@. Note that the head of the stream has index 0.
(!!) :: Int -> Stream a -> a
(!!) n (Cons x xs)
  | n == 0    = x
  | n > 0     = (!!) (n - 1) xs
  | otherwise = error "Stream.!! negative argument"

-- | The 'zip' function takes two streams and returns a list of corresponding pairs.
zip :: Stream a -> Stream b -> Stream (a,b)
zip (Cons x xs) (Cons y ys) = Cons (x,y) (zip xs ys)

-- | The 'unzip' function is the inverse of the 'zip' function.
unzip :: Stream (a,b) -> (Stream a, Stream b)
unzip (Cons (x,y) xys) = (Cons x (fst (unzip xys)),
                                Cons y (snd (unzip xys)))     

-- | The 'zipWith' function generalizes 'zip'. Rather than tupling
-- the functions, the elements are combined using the function
-- passed as the first argument to 'zipWith'.
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)

-- | 'span' @p@ @xs@ returns the longest prefix of @xs@ that satisfies
-- @p@, together with the remainder of the stream. 
span :: (a -> Bool) -> Stream a -> ([a], Stream a)
span p (Cons x xs)
  | p x       = let (trues, falses) = span p xs
                in (x : trues, falses)
  | otherwise = ([], Cons x xs)

-- | The 'break' @p@ function is equivalent to 'span' @not . p@.
break :: (a -> Bool) -> Stream a -> ([a], Stream a)
break p = span (not . p)

-- | The 'words' function breaks a stream of characters into a stream of words,
-- which were delimited by white space.
words :: Stream Char -> Stream String
words xs = let (w, ys) = break isSpace xs
                 in Cons w (words ys)

-- | The 'unwords' function is an inverse operation to 'words'. It
-- joins words with separating spaces.
unwords :: Stream String -> Stream Char
unwords (Cons x xs) = foldr Cons (Cons ' ' (unwords xs)) x

-- | The 'lines' function breaks a stream of characters into a list
-- of strings at newline characters. The resulting strings do not
-- contain newlines.
lines :: Stream Char -> Stream String
lines xs = let (l, ys) = break (== '\n') xs
                 in Cons l (lines (tail ys))

-- | The 'unlines' function is an inverse operation to 'lines'. It
-- joins lines, after appending a terminating newline to each.
unlines :: Stream String -> Stream Char
unlines (Cons x xs) = foldr Cons (Cons '\n' (unlines xs)) x


-- | The 'isPrefix' function returns @True@ if the first argument is a prefix of the second.
isPrefixOf :: Eq a => [a] -> Stream a -> Bool
isPrefixOf [] _ = True
isPrefixOf (y:ys) (Cons x xs)
  | y == x    = isPrefixOf ys xs
  | otherwise = False

-- | The 'partition' function takes a predicate @p@ and a stream
-- @xs@, and returns a pair of streams. The first stream corresponds
-- to the elements of @xs@ for which @p@ holds; the second stream
-- corresponds to the elements of @xs@ for which @p@ does not hold.
partition :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
partition p (Cons x xs) = 
  let (trues,falses) = partition p xs
  in if p x then (Cons x trues, falses)
            else (trues, Cons x falses)

-- | The 'inits' function takes a stream @xs@ and returns all the
-- finite prefixes of @xs@.
inits :: Stream a -> Stream ([a])
inits (Cons x xs) = Cons [] (fmap (x:) (inits xs))

-- | The 'tails' function takes a stream @xs@ and returns all the
-- suffixes of @xs@.
tails :: Stream a -> Stream (Stream a)
tails xs = Cons xs (tails (tail xs))


-- | The 'splitAt' function takes an integer @n@ and a stream @xs@
-- | and returns a pair consisting of the prefix of @xs@ of length
-- | @n@ and the remaining stream immediately following this prefix.
splitAt :: Int -> Stream a -> ([a], Stream a)
splitAt n xs
  | n == 0    = ([],xs)
  | n > 0     = let (prefix,rest) = splitAt (n-1) (tail xs)
                in (head xs : prefix, rest)
  | otherwise = error "Stream.splitAt negative argument."

-- | The 'streamToList' converts a stream into an infinite list.
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- | The 'listToStream' converts an infinite list to a
-- stream. Passing a finite list will result in an error.
listToStream :: [a] -> Stream a
listToStream (x:xs) = Cons x (listToStream xs)
listToStream []     = error "Stream.listToStream applied to finite list"

