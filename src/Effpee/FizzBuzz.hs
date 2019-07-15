module Effpee.FizzBuzz where

import Control.Applicative (pure)
import Control.Monad       (mapM_, (>>=))
import Data.Bool
import Data.Function       (($))
import Data.Int
import Data.String         (String)
import Effpee
import Effpee.ADT
import Effpee.Or
import GHC.Show            (show)
import Prelude             (IO, print, rem)

-- Given a list of integers when
-- * the integer is a multiple of 15 xor a multiple of 7 then we want to output "bloop"
-- * the integer is a multiple of 5 and a multiple of 3 then we want to output "fizzbuzz"
-- * the integer is a multiple of 5 we want to output "buzz"
-- * the integer is a multiple of 3 we want to output "fizz"
-- * otherwise we want to print out the number

list :: [Int]
list = [1..100]

multN :: Int -> Int -> String -> Or String Int
multN base n s = if n `rem` base == 0 then Failure s else Success n

type Predicate a = a -> Bool

type BinOp a = a -> a -> a
type Kleisli m a b = a -> m b

fizz :: Int -> Or String Int
fizz n = multN 3 n "fizz"

buzz :: Int -> Or String Int
buzz n = multN 5 n "buzz"

seven :: Int -> Or String Int
seven n = multN 7 n "SEVEN"

fizzbuzz :: Int -> Or String Int
fizzbuzz = and' fizz buzz "fizzbuzz"

bloop :: Int -> Or String Int
bloop = xor' fizzbuzz seven "bloop"

toPredicate :: (Int -> Or String Int) -> Predicate Int
toPredicate f = \n -> either (f n) (\_ -> True) (\_ -> False)

fromPredicate :: Predicate a -> err -> (a -> Or err a)
fromPredicate p e = \a -> if p a then Failure e else Success a

and'
  :: (Int -> Or String Int)
  -> (Int -> Or String Int)
  -> String
  -> (Int -> Or String Int)
and' =  combine' andP

xor' :: (Int -> Or String Int) -> (Int -> Or String Int) -> String -> (Int -> Or String Int)
xor' = combine' xorP

combine'
  :: BinOp (Predicate Int)
  -> (Int -> Or String Int)
  -> (Int -> Or String Int)
  -> String
  -> (Int -> Or String Int)
combine' op f g = fromPredicate $ op (toPredicate f) (toPredicate g)

andP :: Predicate a -> Predicate a -> Predicate a
andP = combineP (&&)

xorP :: BinOp (Predicate a)
xorP = combineP (\p q -> (p || q) && not (p && q))

combineP :: (Bool -> Bool -> Bool) -> Predicate a -> Predicate a -> Predicate a
combineP op p q = \n -> p n `op` q n

convert :: Int -> String
convert n = toString (pure n >>= bloop >>= fizzbuzz >>= buzz >>= fizz)

toString :: Or String Int -> String
toString (Failure e) = e
toString (Success a) = show a

converted :: [String]
converted = convert <$> list

main :: IO ()
main = mapM_ print converted

