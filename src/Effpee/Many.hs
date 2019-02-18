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
import GHC.Show

instance (Show a) => Show (Many a) where
  show = show . toList

-- | Reverses the sequence contains in the =ManyCons a=.
reverse
  :: Many a
  -> Many a
reverse = todo "Effpee.Many.reverse"

-- | Computes the length of the sequence of =a=s in a =ManyCons a=.
length
  :: Many a
  -> Int
length = todo "Effpee.Many.length"

toList
  :: Many a
  -> [a]
toList = todo "Effpee.Many.toList"

fromList
  :: [a]
  -> Many a
fromList = todo "Effpee.Many.fromList"
