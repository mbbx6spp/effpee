module Effpee.ManyTest (suite) where

import Effpee.Test

-- module under test
import Effpee.Many

suite
  = testGroup "Many"
    [ testProperty "reverse . reverse == identity" propReverseReverseIsId
    , testProperty "reverse == identity when list empty" propReverseIsIdEmpty
    , testProperty "reverse == identity when list singleton" propReverseIsIdSingleton
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

propReverseIsIdEmpty = property $ do
  xs <- forAll genManyEmpty
  reverse xs === xs

propReverseIsIdSingleton = property $ do
  xs <- forAll genManySingleton
  reverse xs === xs

propReverseLengthIsIdLength = property $ do
  xs <- forAll genMany
  length (reverse xs) === length xs
