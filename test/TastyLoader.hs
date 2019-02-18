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

main :: IO ()
main =
  defaultMain $
    testGroup "Tasty"
      [ ADT.suite
      , USMoney.suite
      , Many.suite
      , Option.suite
      , Deferred.suite
      , Functor.suite
      ]
