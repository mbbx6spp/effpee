{-# LANGUAGE StandaloneDeriving #-}

module Effpee.Composition.IndependentEffects where

import Control.Applicative (pure, (<*>))
import Data.Bool
import Data.Function       (($))
import Data.Int
import Data.Maybe
import Data.Ord            (Ord, compare, Ordering (..), (<), (>))
import Data.Eq             ((==), Eq)
import Data.Text
import GHC.Err             (error)
import GHC.Num
import GHC.Show (Show)

data Date = MkDate Year Month Day deriving (Show, Eq)
data YearIndicator = BC | AD deriving (Show, Eq)
data Year   = Year Int YearIndicator deriving (Show, Eq)

instance Ord YearIndicator where
  compare AD BC = GT
  compare AD AD = EQ
  compare BC AD = LT
  compare BC BC = EQ

instance Ord Year where
  compare :: Year -> Year -> Ordering
  compare (Year x indx) (Year y indy)
    | indy == indx = compare x y
    | otherwise = compare indx indy
newtype Day = MkDay Int deriving (Show, Eq)
data Month
  = Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
  deriving (Show, Eq)

deriving instance Ord Month

mkYear :: Int -> Maybe Year
mkYear n | n < 0 = Just $ Year (-n) BC
mkYear n = Just $ Year n AD

mkMonth :: Int -> Maybe Month
mkMonth 1  = Just Jan
mkMonth 2  = Just Feb
mkMonth 3  = Just Mar
mkMonth 4  = Just Apr
mkMonth 5  = Just May
mkMonth 6  = Just Jun
mkMonth 7  = Just Jul
mkMonth 8  = Just Aug
mkMonth 9  = Just Sep
mkMonth 10 = Just Oct
mkMonth 11 = Just Nov
mkMonth 12 = Just Dec
mkMonth _  = Nothing

mkDay :: Int -> Maybe Day
mkDay n | n > 0 && n < 32 = Just (MkDay n)
mkDay _ = Nothing

-- TODO Exercise: implement mkDate
-- Hints: look at the type of (<*>) and pure
mkDate :: (Int, Int, Int) -> Maybe Date
mkDate (y, m, d) = error "TODO: implement using <*>"

getFixture0Date :: Maybe Date
getFixture0Date = mkDate fixture0

getFixture1Date :: Maybe Date
getFixture1Date = mkDate fixture1

getFixture2Date :: Maybe Date
getFixture2Date = mkDate fixture2

getFixture3Date :: Maybe Date
getFixture3Date = mkDate fixture3

fixture0 = (-30, 13, 5)   -- should produce: Nothing
fixture1 = (2000, 1, 1)   -- should produce: Just (Year 2000 AD) Jan 1
fixture2 = (2018, 11, 31) -- should produce: Just (Year 2018 AD) Nov 31
fixture3 = (1999, 12, 31) -- should produce: Just (Year 1999 AD) Dec 31

fixture0Expectation = Nothing
fixture1Expectation = Just $ MkDate (Year 2000 AD) Jan (MkDay 1)
fixture2Expectation = Just $ MkDate (Year 2018 AD) Nov (MkDay 31)
fixture3Expectation = Just $ MkDate (Year 1999 AD) Dec (MkDay 31)
