module Effpee.ManyTest (suite) where

import Effpee.Test

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

headOrDefaultTests
  = testGroup "headOrDefault"
    [ testProperty "headOrDefault produces head when non-empty" $
      property $
        do x <- forAll alpha
           y <- forAll alpha
           tail <- forAll genMany
           headOrDefault x (y :. tail) === y

    , testProperty "headOrDefault produces default when empty" $
      property $
        do x <- forAll alpha
           headOrDefault x Empty === x
    ]

appendTests
  = testGroup "append"
    [ testProperty "headOrDefault on an appended result produces head of the first argument" $
      property $
        do def <- forAll alpha
           xs  <- forAll genMany
           ys  <- forAll genMany
           headOrDefault def (append xs ys) === headOrDefault def xs
    ]

toListTests
  = testGroup "toList"
    [ testProperty "fromList . toList == id" $
      property $
        do xs <- forAll genMany
           (fromList . toList) xs === xs
    ]

fromListTests
  = testGroup "fromList"
    [ testProperty "toList . fromList == id" $
      property $
        do xs <- forAll $ list (linear 1 10) alphaNum
           (toList . fromList) xs === xs
    ]

dropTests
  = testGroup "drop"
    [ testProperty "drop anything on @Empty@ produces @Empty@" $
      property $
        do n <- forAll $ int (linear 1 10)
           drop (toInteger n) Empty === (Empty :: Many Char)
    , testProperty "drop ((length xs) + 1) xs == Empty" $
      property $
        do xs <- forAll genMany
           let n = (length xs) + 1
           drop n xs === (Empty :: Many Char)
    ]

dropWhileTests
  = testGroup "dropWhile"
    [ testProperty "dropWhile on @Empty@ produces @Empty@" $
      property $ dropWhile (const False) Empty === (Empty :: Many Char)
    ]

takeTests
  = testGroup "take"
    [ testProperty "take on @Empty@ produces @Empty@" $
      property $
        do n <- forAll $ int (linear 1 10)
           take (toInteger n) Empty === (Empty :: Many Char)
    , testProperty "take ((length xs) + 1) xs == xs" $
      property $
        do xs <- forAll genMany
           let n = toInteger $ (length xs) + 1
           take n xs === xs
    , testProperty "length $ take ((length xs) - 1) xs == length xs - 1" $
      property $
        do xs <- forAll genMany
           let n = toInteger $ (length xs) - 1
           (length (take n xs)) === n
    ]

takeWhileTests
  = testGroup "takeWhile"
    [
    ]

partitionTests
  = testGroup "partition"
    [
    ]

zipWithTests
  = testGroup "zipWith"
    [
    ]

foldRTests
  = testGroup "foldR"
    [
    ]

foldLTests
  = testGroup "foldL"
    [
    ]

productTests
  = testGroup "product"
    [
    ]


product'Tests
  = testGroup "product'"
    [
    ]

sumTests
  = testGroup "sum"
    [
    ]

sum'Tests
  = testGroup "sum'"
    [
    ]

lengthTests
  = testGroup "length"
    [
    ]


orTests
  = testGroup "or"
    [
    ]


or'Tests
  = testGroup "or'"
    [
    ]


andTests
  = testGroup "and"
    [
    ]


and'Tests
  = testGroup "and'"
    [
    ]


transformTests
  = testGroup "transform"
    [
    ]


filterTests
  = testGroup "filter"
    [
    ]


singletonTests
  = testGroup "singleton"
    [
    ]


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



flatMapTests
  = testGroup "flatMap"
    [
    ]

flattenTests
  = testGroup "flatten"
    [
    ]
