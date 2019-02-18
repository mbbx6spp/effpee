module Effpee.ADT
  ( Void
  , Unit (..)
  , Boolean (..)
  , President (..)
  , USCoin (..)
  , USBill (..)
  , Option (..)
  , Or (..)
  , Pair (..)
  , Many (..)
  , ManyReversed (..)
  , Deferred (..)
  ) where

import Data.Eq
import Data.Functor ((<$>))
import Data.List    (intersperse)
import GHC.Enum
import GHC.Show

{-
 _______________________
< seemingly silly types >
 -----------------------
        \   ^__^
         \  (**)\_______
            (__)\       )\/\
             U  ||----w |
                ||     ||

The following are seemingly silly types because they are so trivial. Later we will see
how =Unit= is used and how =Void= can be used to simplify higher-kinded types (HKT)
from an equivalence reasoning perspective.
-}

-- A type without any data constructors
data Void

-- A type with exactly one data constructor
data Unit = Unit

data Boolean = Yeah | Nah deriving (Eq, Show)

{-
 ____________
< coproducts >
 ------------
        \   ^__^
         \  (--)\_______
            (__)\       )\/\
                ||----w |
                ||     ||



-}

-- Coproduct type of US presidents appearing on US bills.
-- * Washington
-- * Jefferson
-- * Lincoln
-- * Hamilton
-- * Jackson
-- * Grant
-- * Franklin (not really a president but we are going with it, maybe I should I named
--   the type Portrait?)
data President
  = Washington
  | Jefferson
  | Lincoln
  | Hamilton
  | Jackson
  | Grant
  | Franklin
  deriving (Enum, Show, Eq)

-- Coproduct type representing each type of US bill
-- * one dollar
-- * two dollar
-- * five dollar
-- * ten dollar
-- * twenty dollar
-- * fifty dollar
-- * one hundred dollar
data USBill
  = OneDollar
  | TwoDollar
  | FiveDollar
  | TenDollar
  | TwentyDollar
  | FiftyDollar
  | OneHundredDollar
  deriving (Enum, Show, Eq)

-- TODO define a coproduct type providing data constructors for each type of US coin
-- * penny
-- * nickel
-- * dime
-- * quarter
-- * one dollar
-- * two dollar
data USCoin
  = Penny
  | Nickel
  | Dime
  | Quarter
  | OneDollarCoin
  | TwoDollarCoin
  deriving (Enum, Show, Eq)

{-
 ________________________
< type constructors, yo! >
 ------------------------
        \   ^__^
         \  (OO)\_______
            (__)\       )\/\
                ||----w |
                ||     ||

Type constructors are algebraic data types that take a type argument such that values
can be parameterized on that type without needing to redefine your API for around that
type for each possible specialized type. For example, a list can have integers or
strings, or more complex types inside of it at least element. The basic operations of
a list should remain the same no matter what is inside a list.
-}

-- TODO define a type constructor that contains exactly one value of type =a=.
data One a = One a

-- A type constructor that can contain one value of type =a= or nothing.
data Option a
  = Nothing
  | Something a

-- TODO define a type constructor that can contain an error value of type =e= or a
-- success value of type =a=.
data Or e a = Failure e | Success a

-- TODO define a type constructor that contains exactly two values of type =a=.
data Pair a

-- A type constructor that contains zero or more values of type =a= with the head of
-- the structure accessible via pattern matching when more than zero values exist.
data Many a
  = Empty
  | a :. Many a
  deriving (Eq)

-- TODO define a type constructor that contains zero or more values of type =a=
-- with the last element of the structure accessible when more than zero values exist.
data ManyReversed a

-- TODO define a non-empty sequence of elements of type =a= in terms of =Many a=s.
data NonEmpty a

-- TODO define a type constructor that contains either a fully evaluated value of type
-- =a= or a deferred computation that will produce an =a= from a =Unit=.
data Deferred a
