{-# OPTIONS -Wno-orphans #-}

module Effpee.Many where

import Data.Function ((.))
import Data.Int
import Effpee
import Effpee.ADT    (Boolean (..), Many (..))
import GHC.Show

-- only needed for REPL interactions
instance (Show a) => Show (Many a) where
  show = show . toList

-- | Returns the head of the given @Many a@ or the provided default value.
-- >>> headOrDefault 5 (6 :. Empty)
-- 6
-- it :: Num a => a
-- >>> headOrDefault 5 Empty
-- 5
-- it :: Num a => a
headOrDefault
  :: a
  -> Many a
  -> a
headOrDefault = todo "Effpee.Many#headOrDefault"

-- | Converts a @Many a@ to a @[a]@.
--   Should retain order of elements and length property.
-- >>> toList (1 :. (2 :. (3 :. Empty)))
-- [1, 2, 3]
-- >>> length (toList (1 :. Empty)) == length [1]
-- True
-- >>> fromList (toList (1 :. (2 :. (3 :. Empty)))) == (1 :. (2 :. (3 :. Empty)))
-- True
toList
  :: Many a
  -> [a]
toList = todo "Effpee.Many#toList"

-- | Converts a @[a]@ to a @Many a@.
--   Should retain order of elements and length property.
fromList
  :: [a]
  -> Many a
fromList = todo "Effpee.Many#fromList"

-- | Append two @Many a@ together in the order they are passed to the function.
-- >>> append (1 :. (2 :. (3 :. Empty))) Empty
-- (1 :. (2 :. (3 :. Empty)))
-- >>> Empty `append` (3 :. (2 :. (1 :. Empty)))
-- (3 :. (2 :. (1 :. Empty)))
-- >>> append (1 :. Empty) (2 :. Empty)
-- (1 :. (2 :. Empty))
append
  :: Many a
  -> Many a
  -> Many a
append = todo "Effpee.Many#append"


-- | Drop the first @Integer@ elements in the given @Many a@.
-- >>> drop 5 Empty
-- Empty
-- >>> drop 3 (1 :. 2 :. Empty)
-- Empty
-- >>> drop 1 (1 :. 2 :. 3 :. 4 :. Empty)
-- (2 :. 3 :. 4 :. Empty)
-- Would this work with /infinite/ lists?
drop :: Integer -> Many a -> Many a
drop = todo "Effpee.Many#drop"

-- | Drop the first elements in the given @Many a@ that do not satisfy the
-- given predicate function @(a -> Bool)@.
-- >>> let isOdd = \x -> x `rem` 2 == 1
-- >>> dropWhile isOdd (13 :. 21 :. 32 :. 41 :. Empty)
-- (32 :. 41 :. Empty)
dropWhile :: (a -> Bool) -> Many a -> Many a
dropWhile = todo "Effpee.Many#dropWhile"

-- | Take the first @Integer@ elements from the given @Many a@.
-- >>> take 5 (1 :. 2 :. Empty)
-- Empty
-- >>> take 3 $ fromList [1 .. 10]
-- (1 :. 2 :. 3 :. Empty)
take
  :: Integer
  -> Many a
  -> Many a
take = todo "Effpee.Many#take"

-- | Take the first elements satisfying the given predicate function @(a -> Bool)@.
-- >>> takeWhile isOdd (51 :. 53 :. 6 :. 55 :. Empty)
-- (51 :. 53 :. Empty)
takeWhile
  :: (a -> Bool)
  -> Many a
  -> Many a
takeWhile = todo "Effpee.Many#takeWhile"

-- | Partition the first elements that satisfy the given predicate function
-- @(a -> Bool)@ from the rest of the given @Many a@, returning a 2-tuple
-- with the first series of matches in the first field of the 2-tuple and
-- the remainder in the second field of the 2-tuple produced.
-- >>> partition isOdd (1 :. 2 :. 3 :. 4 :. Empty)
-- ((1 :. Empty), (2 :. 3 :. 4 :. Empty))
partition
  :: (a -> Bool)
  -> Many a
  -> (Many a, Many a)
partition = todo "Effpee.Many#partition"

-- | Given a function @(a -> b -> c)@, a @Many a@, and a @Many b@ produce a @Many c@.
-- >>> zipWith (+) (fromList [1 .. 5]) (fromList [1 .. 3])
-- (1 :. 4 :. 6 :. Empty)
-- >>> zipWith (*) Empty (fromList [1 .. 10])
-- Empty
zipWith
  :: (a -> b -> c)
  -> Many a
  -> Many b
  -> Many c
zipWith = todo "Effpee.Many#zipWith"

-- | fold from a seed value starting from the right most element in a @Many a@.
-- >>> foldR (:.) Empty (1 :. 2 :. 3 :. Empty)
-- (1 :. 2 :. 3 :. Empty)
-- >>> foldR (+) 0 (1 :. 2 :. 3 :. Empty)
-- 6
-- >>> foldR (&&) True (True :. False :. True :. Empty)
-- False
-- >>> foldR (||) False (False :. False :. True :. Empty)
-- True
-- >>> foldR (++) "" ("hello " :. "world" :. "!" :. Empty)
-- "hello world!"
foldR
  :: (a -> b -> b)
  -> b
  -> Many a
  -> b
-- foldR (+) 0 (1 :. 2 :. 3 :. Empty)
-- foldR (+) 1 (2 :. 3 :. Empty)
-- foldR (+) 3 (3 :. Empty)
-- foldR (+) 6 Empty
-- 6
-- foldR (:.) Empty (1 :. 2 :. 3 :. Empty)
-- foldL (flip (:.)) Empty ...

foldR _ b Empty     = b
foldR f b (x :. xs) = foldR f (f x b) xs

-- | fold from a seed value starting from the first element in a @Many a@.
-- >>> foldL (+) 0 (1 :. 2 :. Empty)
-- 3
foldL
  :: (b -> a -> b)
  -> b
  -> Many a
  -> b
foldL _ b Empty     = b
foldL f b (x :. xs) = foldL f (f b x) xs -- highly inefficient, in practice use foldl'

-- | Produce the produce of all the elements of a @Many Int@.
-- >>> product (fromList [1 .. 10])
-- 3628800
product :: Many Int -> Int
product = todo "Effpee.Many#product"

product' :: Many Int -> Int
product' = todo "Effpee.Many#product' -- express in terms of foldR"

-- | Sum the elements of the given @Many Int@.
-- >>> sum (fromList [1 .. 3])
-- 6
sum :: Many Int -> Int
sum = todo "Effpee.Many#sum -- express in terms of explicit recursion"

-- >>> sum' (fromList [1 .. 3])
-- 6
sum' :: Many Int -> Int
sum' = todo "Effpee.Many#sum' -- express in terms of foldR"

-- | Computes the length of the sequence of =a=s in a =ManyCons a=.
-- Should be O(n).
-- >>> length Empty
-- 0
-- >>> length (1 :. (2 :. (3 :. Empty)))
-- 3
length
  :: Many a
  -> Integer
length = todo "Effpee.Many#length -- express in terms of foldR"

-- | OR-ing a @Many Boolean@ together.
-- >>> or (Nah :. Yeah :. Empty)
-- Yeah
-- >>> or Empty
-- Nah
or :: Many Boolean -> Boolean
-- TODO What did I do wrong here?
or = todo "Effpee.Many#or -- express in terms of explicit recursion"

-- | Generalize @or@.
or' :: (a -> Bool) -> Many a -> Boolean
or' = todo "Effpee.Many#or' -- can you express in terms of foldR/FoldL? If so, how?"

-- | AND-ing a @Many Boolean@ together.
-- >>> and (Yeah :. Nah :. Yeah :. Empty)
-- Nah
-- >>> and Empty
-- Nah
and :: Many Boolean -> Boolean
and = todo "Effpee.Many#and -- explicit recursion"

-- | Generalize @and@.
and' :: (a -> Bool) -> Many a -> Bool
and' = todo "Effpee.Many#and' -- how else can ths be expressed?"

-- | Transform each element based on the given function @(a -> b)@.
-- >>> transform (+1) (1 :. 2 :. 3 :. Empty)
-- (2 :. 3 :. 4 :. Empty)
-- >>> transform (*2) (3 :. 5 :. 7 :. Empty)
-- (6 :. 10 :. 14 :. Empty)
transform
  :: (a -> b)
  -> Many a
  -> Many b
transform = todo "Effpee.Many#transform"

-- | Filter elements out of the @Many a@ that do not meet the predicate criteria.
-- The predicate function should produce @True@ to keep the element in the result.
-- >>> filter (\x -> x `rem` 2 == 0) (1 :. 2 :. 3 :. 4 :. Empty)
-- (2 :. 4 :. Empty)
filter
  :: (a -> Bool)
  -> Many a
  -> Many a
filter _ Empty     = Empty
filter f (x :. xs) = todo "Do during the session live"

-- | Produce a singleton @Many a@ from a given @a@ value.
-- >>> singleton "Victoria"
-- ("Victoria" :. Empty)
-- >>> singleton 42
-- (42 :. Empty)
singleton :: a -> Many a
singleton a = a :. Empty

-- | Apply the given @(a -> Many b)@ function to each element of the @Many a@ and
-- then flatten the result into a @Many b@.
-- >>> flatMap singleton (1 :. 2 :. 3 :. Empty)
-- (1 :. 2 :. 3 :. Empty)
flatMap
  :: (a -> Many b)
  -> Many a
  -> Many b
flatMap = todo "Effpee.Many.flatMap"

-- | Flatten nested @Many (Many a)@ into a @Many a@.
-- >>> flatten ((1 :. 2 :. Empty) :. (3 :. 4 :. Empty) :. Empty)
-- (1 :. 2 :. 3 :. 4 :. Empty)
flatten :: Many (Many a) -> Many a
flatten = todo "Effpee.Many.flatten -- define in terms of flatMap"

-- | Reverses the sequence contains in the =ManyCons a=.
--   Ignore any troubling runtime performance complexities. :)
--   Note: there are multiple implementations if you are familiar with common
--   higher-order functions, however, we haven't been introduced to the necessary
--   HOF in this course yet, so first implement the highly-inefficient implementation.
-- >>> reverse (3 :. (2 :. (1 :. Empty)))
-- (1 :. (2 :. (3 :. Empty)))
-- >>> reverse Empty
-- Empty
reverse
  :: Many a
  -> Many a
reverse Empty = Empty
reverse (x :. xs) = reverse xs `append` (x :. Empty)

reverse' :: Many a -> Many a
reverse' = todo "Effpee.Many#reverse' -- express in terms of foldL and/or foldR"