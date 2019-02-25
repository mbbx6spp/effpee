module Effpee.Stream where

import Effpee
import Effpee.ADT
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
  deriving (Generic, Show)

-- Given a seed integer produce an infinite
naturals
  :: Integer
  -> Stream Integer
naturals n = n :$ naturals (n + 1)

take
  :: Integer
  -> Stream Integer
  -> Stream Integer
take = todo "Effpee.Stream.take"

takeWhile
  :: Predicate a
  -> Stream a
  -> Stream a
takeWhile = todo "Effpee.Stream.takeWhile"

