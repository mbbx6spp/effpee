{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE DerivingVia    #-}


module Effpee.ADT
  ( Void
  , Unit (..)
  , Boolean (..)
  , Portrait (..)
  , USCoin (..)
  , USBill (..)
  , One (..)
  , Option (..)
  , Or (..)
  , Pair (..)
  , Many (..)
  , ManyReversed (..)
  , NonEmpty (..)
  , Deferred (..)
  ) where

import Data.Eq
import Data.Monoid            ((<>))
import Data.Text
import Effpee
import Generics.Deriving.Show
import GHC.Enum
import GHC.Generics           (Generic)
import TextShow
import TextShow.Generic       hiding (One)

-- >>> :set -Wno-missing-home-modules
-- >>> :load src/Effpee/ADT.hs
-- [2 of 2] Compiling Effpee.ADT       ( src/Effpee/ADT.hs, interpreted )
-- Ok, two modules loaded.

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

-- A type with two values: one to signify "true", the other to signify "false"
data Boolean
  = Yeah
  | Nah
  deriving (Eq, Generic)
  deriving (TextShow) via FromGeneric (Boolean)

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
-- * Franklin
data Portrait
  = Washington
  | Jefferson
  | Lincoln
  | Hamilton
  | Jackson
  | Grant
  | Franklin
  deriving (Enum, Eq, Generic)
  deriving (TextShow) via FromGeneric (Portrait)

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
  deriving (Enum, Eq, Generic)

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
  deriving (Enum, Eq, Generic)

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

-- Define a type constructor that contains exactly one value of type =a=.
-- >>> x = One 15
-- x :: Num a => One a
-- >>> y = One "hello"
-- y :: Data.String.IsString a => One a
data One a
  = One a
  deriving (Eq, Generic)

-- A type constructor that can contain one value of type =a= or nothing.
-- >>> a0 = Nothing
-- a0 :: Option a
-- >>> a1 = Something "Harry"
-- a1 :: Data.String.IsString a => Option a
-- >>> a2 = Something "Potter"
-- a2 :: Data.String.IsString a => Option a
-- >>> data Wizard = MkWizard Text Text deriving (Show)
-- data Wizard = ...
data Option a
  = Nothing
  | Something a
  deriving (Eq, Generic)

-- A type constructor that represents either a failure value of type @e@ *or*
-- a success value of type @a@.
-- >>> data FileError = FileNotFoundError | NotEnoughPermissionsError | UnknownFileError
-- data FileError = ...
-- >>> c = Failure FileNotFoundError
-- c :: Or FileError a
-- >>> d = Success "contents of file here!"
-- d :: Data.String.IsString a => Or e a
data Or e a
  = Failure e
  | Success a
  deriving (Eq, Generic)

-- Define a type constructor that contains exactly two values of type =a=.
-- >>> point = Pair 0 0
-- point :: Num a => Pair a
-- >>> :kind Pair
-- Pair :: * -> *
data Pair a
  = Pair a a
  deriving (Eq, Generic)

-- A type constructor that contains zero or more values of type =a= with the head of
-- >>> :set -XFlexibleContexts
-- >>> xs = 1 :. (2 :. (3 :. (4 :. Empty)))
-- xs :: Num a => Many a
-- >>> ys = 1 :. 2 :. 3 :. Empty
-- ys :: Num a => Many a
data Many a
  = Empty
  | a :. Many a
  deriving (Generic, Eq)
infixr 5 :.

-- Define a type constructor that contains zero or more values of type =a=
-- with the last element of the structure accessible when more than zero values exist.
-- aka Snoc list
data ManyReversed a
  = REmpty
  | Many a :- a
  deriving (Generic)

-- Define a non-empty sequence of elements of type =a= in terms of =Many a=s.
data NonEmpty a
  = a :> Many a
  deriving (Generic)

-- Define a type constructor that contains either a fully evaluated value of type
-- =a= or a deferred computation that will produce an =a= from a =Unit=.
data Deferred a
  = Lazy (() -> a)
  | Now a
  deriving (Generic)

-- Define a binary tree structure parameterized over @a@ with a leaf that has a value
-- of @a@, a branch with a left and right tree of @a@'s.
data BinTree a
  = BinLeaf a
  | BinBranch { leftBranch :: (BinTree a), rightBranch :: (BinTree a) }
  deriving (Eq, Generic)

-- Represents an F-algebra
newtype FAlgebra (f :: * -> *) (a :: *)
  = FAlgebra (a -> f a)
  deriving (Generic)
