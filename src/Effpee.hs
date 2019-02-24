module Effpee
  ( todo
  , fromString
  , Bool (..)
  , Generic (..)
  , Num (..)
  , Show (..)
  , Integer
  ) where

import Data.Bool    (Bool (..))
import Data.Monoid  (mconcat)
import Data.String  (String, fromString)
import Data.Text    (Text, unpack)
import GHC.Err      (error)
import GHC.Generics (Generic)
import GHC.Num      (Integer, Num (..))
import GHC.Show     (Show (..))

todo :: String -> a
todo s = error (mconcat ["TODO ", s])
