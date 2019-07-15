module Effpee.Stream where

import Effpee
import Effpee.ADT
import Effpee.Option
import Effpee.Predicate

{-

 ______________________
< Codata (corecursion) >
 ----------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
-}

data Stream a
  = EmptyStream
  | a :$ Stream a
  deriving (Generic, Show, Eq)
infixr 5 :$

data Nat = Zero | Succ Nat deriving (Generic, Show)

-- Given a seed integer produce an infinite
naturals
  :: Nat
  -> Stream Nat
naturals n = n :$ naturals (Succ n)

takeS
  :: Nat
  -> Stream Nat
  -> Stream Nat
takeS = todo "Effpee.Stream.takeS"

takeWhileS
  :: Predicate a
  -> Stream a
  -> Stream a
takeWhileS _ EmptyStream = EmptyStream
takeWhileS p (a :$ restS) = todo "Effpee.Stream.takeWhileS"

takeWhileSM
  :: (a -> Option a)
  -> Stream a
  -> Stream a
takeWhileSM _ EmptyStream = EmptyStream
takeWhileSM f (a :$ rest) = todo "Effpee.Stream.takeWhileSM"
