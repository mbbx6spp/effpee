{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS -fno-warn-orphans #-}

module Effpee.Or where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.String
import Effpee
import Effpee.ADT

-- data Or e a = Failure e | Success a deriving (Eq, Show, Generic)

type err \/ succ = Or err succ

type Defaultable a = a \/ a

mkDefault :: a -> Defaultable a
mkDefault = todo "Effpee.Or.mkDefault"

either :: Or e a -> (e -> b) -> (a -> b) -> b
either = todo "Effpee.Or#either"

instance Functor (Or err) where
  fmap :: (a -> b) -> Or err a -> Or err b
  fmap = todo "Functor#fmap Or"

instance Applicative (Or err) where
  pure :: a -> Or err a
  pure = todo "Applicative#pure Or"

  (<*>) :: Or err (a -> b) -> Or err a -> Or err b
  (<*>) = todo "Applicative#(<*>) Or"

instance Monoid err => Alternative (Or err) where
  empty :: Or err a
  empty = todo "(Monoid err => Alternative (Or err))#empty"

  (<|>) :: Or err a -> Or err a -> Or err a
  (<|>) = todo "(Monoid err => Alternative (Or err))#(<|>)"

instance Monad (Or err) where
  (>>=) :: Or err a -> (a -> Or err b) -> Or err b
  (>>=) = todo "Monad#(>>=) (Or err)"