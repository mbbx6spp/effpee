module Effpee.Composition.Functions where

import Data.Int
import GHC.Num

after :: (b -> c)
      -> (a -> b)
      -> (a -> c)
after g f = \a -> g (f a)

double :: Int -> Int
double x = x * 2
incr :: Int -> Int
incr x   = x + 1

-- >>> foo 3
-- 7
foo :: Int -> Int
foo = incr `after` double

-- >>> bar 3
-- 8
bar :: Int -> Int
bar = double `after` incr

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \y x -> f x y

before :: (a -> b)
       -> (b -> c)
       -> (a -> c)
before = flip after

-- >>> foo' 3
-- 7
foo' :: Int -> Int
foo' = double `before` incr

-- >>> bar' 3
-- 8
bar' :: Int -> Int
bar' = incr `before` double

-- TODO: Exercise - import Control.Category at the top of the module
-- Look at the types of (<<<) and (>>>) and substitute them into the
-- appropriate foo/foo' and bar/bar' definitions to yield the same
-- results as `after` and `before`


