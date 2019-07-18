module Effpee
  ( todo
  , fromString
  , Bool (..)
  , (&&)
  , (||)
  , Generic
  , Num (..)
  , Show (..)
  , Integer
  , (<$>)
  , (<*>)
  , (<)
  , id
  , Eq (..)
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Bool           (Bool (..), (||), (&&))
import Data.Eq             (Eq (..))
import Data.Function (id)
import Data.Monoid         (mconcat)
import Data.Ord            ((<))
import Data.String         (String, fromString)
import Data.Text           (Text, unpack)
import GHC.Err             (error)
import GHC.Generics        (Generic)
import GHC.Num             (Integer, Num (..))
import GHC.Show            (Show (..))

todo :: String -> a
todo s = error (mconcat ["TODO ", s])
