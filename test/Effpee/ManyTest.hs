module Effpee.ManyTest (suite) where

import Data.Char
import Data.Function (($))
import Data.Functor.Identity (Identity)
import Control.Applicative ((<$>), (<*>), pure)
import Effpee.Test

-- module under test
import Effpee.Many

suite
  = testGroup "Effpee.Many"
    [ testProperty "reverse . reverse == identity" propReverseReverseIsId
    , testProperty "reverse == identity when list empty" propReverseIsIdEmpty
    , testProperty "reverse == identity when list singleton" propReverseIsIdSingleton
    , testProperty "reverse != identity when list not empty and not singleton" propReverseNotId
    , testProperty "length . reverse == length . identity" propReverseLengthIsIdLength
    ]

genMany :: GenT Identity (Many Char)
genMany = fromList <$> list (linear 0 100) alpha

genManyEmpty :: GenT Identity (Many Char)
genManyEmpty = constant Empty

genManyMany :: GenT Identity (Many Char)
genManyMany = fromList <$> list (linear 2 100) alpha

genManySingleton :: GenT Identity (Many Char)
genManySingleton = fromList <$> list (linear 1 1) alpha

propReverseReverseIsId = property $ do
  xs <- forAll genMany
  reverse (reverse xs) === xs

propReverseNotId = property $ do
  xs <- forAll genManyMany
  reverse xs /== xs

propReverseIsIdEmpty = property $ do
  xs <- forAll genManyEmpty
  reverse xs === xs

propReverseIsIdSingleton = property $ do
  xs <- forAll genManySingleton
  reverse xs === xs

propReverseLengthIsIdLength = property $ do
  xs <- forAll genMany
  length (reverse xs) === length xs
