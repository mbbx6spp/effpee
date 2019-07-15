{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}


module Effpee.Church where

import Data.Function (id, (.))
import Data.List     ((++))

true :: r -> r -> r
true t _ = t

false :: r -> r -> r
false _ f = f

not p b a = p b a

or p q = p true q

and p q = p q false

xor a b = a (not b) b

ifte p a b = p a b

newtype Codensity (f :: * -> *) (a :: *)
  = Codensity { runCodensity :: forall b. (a -> f b) -> f b }

type DifferenceList a = Codensity [] [a]

nil :: DifferenceList a
nil = Codensity (\fn -> fn [])

infixr 5 `cons`
cons :: a -> DifferenceList a -> DifferenceList a
cons x (Codensity xs) = Codensity (\fn -> fn (xs (x : )))

append :: DifferenceList a -> DifferenceList a -> DifferenceList a
append (Codensity xs) ys = Codensity (\fn -> fn (xs (++ toList ys)))

fromList :: [a] -> DifferenceList a
fromList xs = Codensity (\fn -> fn xs)

toList :: DifferenceList a -> [a]
toList dl = runCodensity dl id

