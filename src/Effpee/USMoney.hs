{-# OPTIONS -Wno-unused-matches #-}

module Effpee.USMoney
  ( evalCoins
  , evalBills
  , getPresident
  , President (..)
  , USCoin (..)
  , USBill (..)
  , Many (..)
  ) where

import Data.Int
import Effpee
import Effpee.ADT

evalCoin :: USCoin -> Int
evalCoin = todo "Effpee.USMoney.evalCoin"

evalCoins
  :: Many USCoin
  -> Int
evalCoins Empty = 0
evalCoins (x :. xs) = todo "Effpee.USMoney.evalCoins"

evalBill :: USBill -> Int
evalBill = todo "Effpee.USMoney.evalBill"

evalBills
  :: Many USBill
  -> Int
evalBills = todo "Effpee.USMoney.evalBills"

-- Given a US bill/note produce the presient whose portrait appears on it.
-- * $1   => Washington
-- * $2   => Jefferson
-- * $5   => Lincoln
-- * $10  => Hamilton
-- * $20  => Jackson
-- * $50  => Grant
-- * $100 => Franklin
getPresident
  :: USBill
  -> President
getPresident FiveDollar = Lincoln -- To get you started with a passing case
getPresident _ = todo "Effpee.USMoney.getPresident"
