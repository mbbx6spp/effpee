module Effpee.Predicate where

import Effpee
import Effpee.ADT
import Effpee.Option

-- | A wrapped (typesafe) representation of a function that takes an @a@ and determines
-- if a conditions applies for the given value of @a@.
-- The following decides (given a value of a numeric type) whether it is above zero:
-- >>> let p0 = Predicate (\x -> x > 0)
-- The following decides (given a value of a list of elements) whether it is empty:
-- >>> let p1 = Predicate (\x -> not (null x))
newtype Predicate a
  = Predicate { runPredicate :: a -> Bool }

instance Show (Predicate a) where
  show _ = "<predicate>"

toOption :: Predicate a -> (a -> Option a)
toOption = todo "Effpee.Predicate#toOption"

toPredicate :: (a -> Option a) -> Predicate a
toPredicate = todo "Effpee.Predicate#toPredicate"

