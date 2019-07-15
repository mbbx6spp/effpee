{-# LANGUAGE FlexibleInstances #-}

module Effpee.Crap where

import Data.Functor
import Data.Int
import Data.Bool

newtype FromA b a = FromA (a -> b)
-- Must define: fmap :: (a -> b) -> FromA () a -> FromA () b
newtype Predicate a = Predicate (a -> Bool)
-- Must define: fmap :: (a -> b) -> Predicate a -> Predicate b
data SwappedTuple b a = SwappedTuple (a, b)
-- Must define: fmap :: (a -> b) -> SwappedTuple () a -> SwappedTuple () b
