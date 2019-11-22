{-# LANGUAGE FlexibleInstances #-}

module Effpee.Crap where

import Data.Bool
import Data.Functor
import Data.Int

newtype FromA b a = FromA (a -> b)
-- If a covariant functor must be able to define:
-- (a -> b) -> FromA () a -> FromA () b
-- TODO: Exercise is it a covariant functor?
-- (a) If so, define the function with type signature above.
-- (b) If not, why not?

newtype Predicate a = Predicate (a -> Bool)
-- If a covariant functor must be able to define:
-- (a -> b) -> Predicate a -> Predicate b
-- TODO: Exercise is it a covariant functor?
-- (a) If so, define the function with type signature above.
-- (b) If not, why not?

data SwappedTuple b a = SwappedTuple (a, b)
-- If a covariant functor must be able to define:
-- (a -> b) -> SwappedTuple () a -> SwappedTuple () b
-- TODO: Exercise is it a covariant functor?
-- (a) If so, define the function with type signature above.
-- (b) If not, why not?
