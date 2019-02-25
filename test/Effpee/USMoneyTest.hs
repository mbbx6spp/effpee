module Effpee.USMoneyTest (suite) where

import Effpee
import Effpee.Test

-- module under test
import Effpee.USMoney

suite
  = testGroup "USMoney"
    -- getPortrait test cases
    [ testCase "getPortrait $1 == Washington"  $ Washington @=? getPortrait OneDollar
    , testCase "getPortrait $2 == Jefferson"   $ Jefferson @=? getPortrait TwoDollar
    , testCase "getPortrait $5 == Lincoln"     $ Lincoln @=? getPortrait FiveDollar
    , testCase "getPortrait $10 == Hamilton"   $ Hamilton @=? getPortrait TenDollar
    , testCase "getPortrait $20 == Jackson"    $ Jackson @=? getPortrait TwentyDollar
    , testCase "getPortrait $50 == Grant"      $ Grant @=? getPortrait FiftyDollar
    , testCase "getPortrait $100 == Franklin"  $ Franklin @=? getPortrait OneHundredDollar
    -- evalBills test cases
    , testCase "evalBills [OneDollar, OneHundredDollar] == 101" $ 101 @=? evalBills (OneDollar :. (OneHundredDollar :. Empty))
    , testCase "evalBills [OneDollar, FiveDollar, FiftyDollar] == 56" $ 56 @=? evalBills (OneDollar :. (FiveDollar :. (FiftyDollar :. Empty)))
    , testCase "evalBills [] == 0" $ 0 @=? evalBills Empty
    -- evalCoins test cases
    , testCase "evalCoins [] == 0" $ 0 @=? evalCoins Empty
    , testCase "evalCoins [TwoDollarCoin, Penny, Nickel] == 206" $ 206 @=? evalCoins (TwoDollarCoin :. (Penny :. (Nickel :. Empty)))
    -- isPresident test cases
    , testCase "isPresident Franklin == Nah" $ Nah @=? isPresident Franklin
    , testCase "isPresident Lincoln == Yeah" $ Yeah @=? isPresident Lincoln
    ]

