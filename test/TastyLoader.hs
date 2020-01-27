module Main (main) where

import Data.Function (($))
import System.IO     (IO)
import Test.Tasty    (defaultMain, testGroup)

import qualified Effpee.ADTTest      as ADT
import qualified Effpee.CardsTest    as Cards
import qualified Effpee.DeferredTest as Deferred
import qualified Effpee.FunctorTest  as Functor
import qualified Effpee.ManyTest     as Many
import qualified Effpee.OptionTest   as Option
import qualified Effpee.USMoneyTest  as USMoney

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
      , Cards.suite
      ]
