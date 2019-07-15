{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS -fno-warn-orphans #-}

module Effpee.Option
  ( (??)
  , optional
  ) where

import Control.Applicative hiding (optional)
import Data.Functor
import Effpee
import Effpee.ADT

(??) :: Option a -> a -> a
(??) = todo "Effpee.Option.(??)"
infixr 4 ??

optional :: target -> (source -> target) -> Option source -> target
optional = todo "Effpee.Option.optional"

instance Functor Option where
  fmap :: (a -> b) -> Option a -> Option b
  fmap = todo "Functor#fmap Option"

instance Applicative Option where
  pure :: a -> Option a
  pure = todo "Applicative#pure Option"

  (<*>) :: Option (a -> b) -> Option a -> Option b
  (<*>) = todo "Applicative#(<*>) Option"
