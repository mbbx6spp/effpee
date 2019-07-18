{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Effpee.Debug where

import Effpee
import GHC.Show
import Data.String
import Data.Text (unpack)
import Data.Text.Conversions
import Control.Category ((<<<))

instance ToText a => Show a where
  show :: a -> String
  show = unpack <<< toText

