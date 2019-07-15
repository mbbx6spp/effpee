{-# LANGUAGE InstanceSigs #-}

module Effpee.Pair
  (
  ) where

import Effpee
import Effpee.ADT
import Data.Functor

{-
data Pair a
  = Pair a a
    deriving (Eq, Show, Generic)
-}

instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair x y) = todo "Functor#fmap Effpee.Pair.Pair"
