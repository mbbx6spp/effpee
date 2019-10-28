module Effpee.Composition.Arrows where

import Control.Arrow      ((&&&), (***))
import Control.Category   ((<<<))
import Data.Foldable      (Foldable, foldMap)
import Data.Function      (id)
import Data.Functor.Const (Const (Const))
import Data.Int
import Data.List
import Data.Monoid
import Data.String

type Count = Const (Sum Int)

count :: Int -> Count a
count = Const <<< Sum

countL :: String -> Count a
countL = count <<< length <<< lines

countW :: String -> Count a
countW = count <<< length <<< words

countC :: String -> Count a
countC = count <<< length <<< id

countAll :: Foldable f => f String -> (Count a, (Count d, (Count b, Count c)))
countAll = foldMap (countL &&& countW &&& countC &&& countC)

fixture1
  = [ "hello\nworld!"
    , "hi World,\nHow are you?"
    , "foo bar"
    ]
