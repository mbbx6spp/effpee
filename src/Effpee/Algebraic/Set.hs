module Effpee.Algebraic.Set where

import Data.Bool
import Data.Eq
import Data.Function (($), (.))
import Data.Int
import GHC.Real      (rem)

-- Term-level definition of a set
data Set a = MkSet { hasMember :: a -> Bool }

isEven :: Int -> Bool
isEven x = x `rem` 2 == 0

-- >>> evens `hasMember` 5
-- False
-- >>> evens `hasMember` 6
-- True
evens :: Set Int
evens = MkSet isEven

-- >>> odds `hasMember` 3
-- True
-- >>> odds `hasMember` 4
-- False
odds :: Set Int
odds = MkSet $ not . isEven

-- But we can view any type in Haskell as a Set too and
-- bring concepts to the type-level

-- Correct by value construction, no hasMember test required
data Even = EvenZero | EvenSucc Odd
data Odd  = OddSucc Even
