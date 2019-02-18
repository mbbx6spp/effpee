module Effpee
  ( todo
  , fromString
  ) where

import Data.Monoid (mconcat)
import Data.String (String, fromString)
import Data.Text   (Text, unpack)
import GHC.Err     (error)

todo :: String -> a
todo s = error (mconcat ["TODO ", s])
