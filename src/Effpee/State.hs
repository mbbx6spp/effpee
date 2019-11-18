{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module State where

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Either
import Data.Function
import Data.Int
import Data.Ord

newtype RetryPolicy
  = MkRetryPolicy { retries :: Int }

mkRetryPolicy :: Monad m => Int -> m RetryPolicy
mkRetryPolicy n | n >=0 = pure $ MkRetryPolicy n
mkRetryPolicy _ = fail "can't create RetryPolicy with negative Int"

type Retrier
  = forall a b e
  . RetryPolicy
  -> (b -> Either e a)
  -> b
  -> (RetryPolicy, Either e a)

alwaysFails
  :: forall a b. b
  -> Either () a
alwaysFails _ = Left ()

iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p karr a
  = karr a
    >>= \x -> if p x
            then pure x
            else iterateUntilM p karr x


 -- Properties
-- prop> \(n : PositiveInt) -> until (retries (MkRetryPolicy n) == 0) $ retrier (MkRetryPolicy (n - 1)) (alwaysFails ()) == Left error

-- Function level property patterns:
-- * equalities
--   e.g. \xs -> reverse . reverse xs === xs
-- * invariants
--   e.g. \xs -> pick a number between 0 and n=((length xs) - 1); lookup the value of at the nth location in xs and compare to the (length - n - 1)th element of the (reverse xs)
--   e.g. \xs -> length (fmap inc xs) === length xs
-- * idempotency
--   e.g. \xs -> sort (sort xs) == sort xs
-- * chain equalities
--   e.g. \xs -> sort (inc <$> xs) == inc <$> sort xs
-- * inverses
--   e.g. \bs -> decode64 (encode64 bs) == bs

-- System/Application-level property patterns:
-- * difference
--   e.g. \keyword scope1 -> count (search keyword globalScope) >= count (search keyword scope1)
-- * equality
--   e.g. \keyword limit1 limit2 -> slice 1 (min limit1 limit2) (results $ search keyword globalScope limit1) === slice 1 (min limit1 limit2) (results $ search keyword globalScope limit2)
