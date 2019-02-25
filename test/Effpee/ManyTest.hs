module Effpee.ManyTest (suite) where

import Effpee.Test
import Effpee

-- module under test
import Effpee.Many

suite
  = testGroup "Many"
  [ reverseTests
  , headOrDefaultTests
  , appendTests
  , toListTests
  , fromListTests
  , dropTests
  , dropWhileTests
  , takeTests
  , takeWhileTests
  , partitionTests
  , zipWithTests
  , foldRTests
  , foldLTests
  , productTests
  , product'Tests
  , sumTests
  , sum'Tests
  , lengthTests
  , orTests
  , or'Tests
  , andTests
  , and'Tests
  , transformTests
  , filterTests
  , singletonTests
  , flatMapTests
  , flattenTests
  , reverseTests
  ]

genMany :: GenT Identity (Many Char)
genMany = fromList <$> list (linear 0 100) alpha

genManyEmpty :: GenT Identity (Many Char)
genManyEmpty = constant Empty

genManyMany :: GenT Identity (Many Char)
genManyMany = fromList <$> list (linear 2 100) alpha

genManySingleton :: GenT Identity (Many Char)
genManySingleton = fromList <$> list (linear 1 1) alpha

{-
 _______________
< headOrDefault >
 ---------------
        \   ^__^
         \  (--)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
-}

headOrDefaultTests = _

appendTests = _

toListTests = _

fromListTests = _

dropTests = _

dropWhileTests = _

takeTests = _

takeWhileTests = _

partitionTests = _

zipWithTests = _

foldRTests = _

foldLTests = _

productTests = _

product'Tests = _

sumTests = _

sum'Tests = _

lengthTests = _

orTests = _

or'Tests = _

andTests = _

and'Tests = _

transformTests = _

filterTests = _

singletonTests = _

{-
 _________
< reverse >
 ---------
        \   ^__^
         \  (**)\_______
            (__)\       )\/\
             U  ||----w |
                ||     ||
-}

reverseTests = testGroup "reverse"
    [ testProperty "reverse . reverse == identity" propReverseReverseIsId
    , testProperty "reverse == identity when list empty" propReverseIsIdEmpty
    , testProperty "reverse == identity when list singleton" propReverseIsIdSingleton
    , testProperty "length . reverse == length . identity" propReverseLengthIsIdLength
    ]

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



flatMapTests = _

flattenTests = _
