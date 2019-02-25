{-# OPTIONS -Wno-orphans #-}

module Effpee.Many
  ( Many (..)
  , reverse
  , length
  , toList
  , fromList
  ) where

import Data.Int
import Data.Function ((.))
import Effpee
import Effpee.ADT (Many (..))
import Effpee.Predicate
import GHC.Show

-- only needed for REPL interactions
instance (Show a) => Show (Many a) where
  show = show . toList

-- | Returns the head of the given @Many a@ or the provided default value.
-- >>> headOrDefault 5 (6 :. Empty)
-- 6
-- >>> headOrDefault 5 Empty
-- 5
headOrDefault
  :: a
  -> Many a
  -> a
headOrDefault = todo "Effpee.Many.headOrDefault"

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
append Empty ys = ys
append xs Empty = xs
-- append (x :. xs) ys = todo "Effpee.Many.append (x :. xs) ys"
append (x :. xs) ys = x :. append xs ys

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
-- reverse = todo "Effpee.Many.reverse"
reverse Empty = Empty
reverse (a :. as) = append (reverse as) (a :. Empty)

-- | Computes the length of the sequence of =a=s in a =ManyCons a=.
--   Should be O(n).
-- >>> length Empty
-- 0
-- >>> length (1 :. (2 :. (3 :. Empty)))
-- 3
length
  :: Many a
  -> Int
-- length = todo "Effpee.Many.length"
length Empty = 0
length (_ :. as)  = 1 + length as

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
toList Empty = []
toList (a :. as) = a : toList as

-- | Converts a @[a]@ to a @Many a@.
--   Should retain order of elements and length property.
-- >>> 
fromList
  :: [a]
  -> Many a
-- fromList = todo "Effpee.Many.fromList"
fromList [] = Empty
fromList (a : as) = a :. fromList as

drop :: Integer -> Many a -> Many a
drop = todo "Effpee.Many.drop"

dropWhile :: Predicate a -> Many a -> Many a
dropWhile = todo "Effpee.Many.dropWhile"
