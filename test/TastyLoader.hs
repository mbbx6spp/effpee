module Main (main) where

import Data.Function (($))
import Test.Tasty (testGroup, defaultMain)
import System.IO (IO)

import qualified Effpee.ADTTest as ADT
import qualified Effpee.USMoneyTest as USMoney
import qualified Effpee.ManyTest as Many
import qualified Effpee.OptionTest as Option
import qualified Effpee.DeferredTest as Deferred
import qualified Effpee.FunctorTest as Functor
import qualified Effpee.NonEmptyTest as NonEmpty
import qualified Effpee.OrTest as Or
import qualified Effpee.PairTest as Pair
import qualified Effpee.StreamTest as Stream

main :: IO ()
main =
  defaultMain $
    testGroup "Effpee"
      [ ADT.suite
      , USMoney.suite
      , Many.suite
      , Option.suite
      , Deferred.suite
      , Functor.suite
      , NonEmpty.suite
      , Or.suite
      , Pair.suite
      , Stream.suite
      ]
