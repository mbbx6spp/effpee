module Effpee.USMoneyTest (suite) where

import Data.Function (($))
import Effpee
import Effpee.ADT
import Effpee.Test

-- module under test
import Effpee.USMoney

suite
  = testGroup "Effpee.USMoney"
    [ testCase "getPresident $1 == Washington"  $ Washington @=? getPresident OneDollar
    , testCase "getPresident $2 == Jefferson"   $ Jefferson @=? getPresident TwoDollar
    , testCase "getPresident $5 == Lincoln"     $ Lincoln @=? getPresident FiveDollar
    , testCase "getPresident $10 == Hamilton"   $ Hamilton @=? getPresident TenDollar
    , testCase "getPresident $20 == Jackson"    $ Jackson @=? getPresident TwentyDollar
    , testCase "getPresident $50 == Grant"      $ Grant @=? getPresident FiftyDollar
    , testCase "getPresident $100 == Franklin"  $ Franklin @=? getPresident OneHundredDollar
    , testCase "evalBills [OneDollar, OneHundredDollar] == 101" $ 101 @=? evalBills (OneDollar :. (OneHundredDollar :. Empty))
    , testCase "evalBills [OneDollar, FiveDollar, FiftyDollar] == 56" $ 56 @=? evalBills (OneDollar :. (FiveDollar :. (FiftyDollar :. Empty)))
    , testCase "evalBills [] == 0" $ 0 @=? evalBills Empty
    , testCase "evalCoins [] == 0" $ 0 @=? evalCoins Empty
    , testCase "evalCoins [TwoDollarCoin, Penny, Nickel] == 206" $ 206 @=? evalCoins (TwoDollarCoin :. (Penny :. (Nickel :. Empty)))
    , testCase "reallyPresident Franklin == Nah" $ Nah @=? reallyPresident Franklin
    , testCase "reallyPresident Lincoln == Yeah" $ Yeah @=? reallyPresident Lincoln
    ]

