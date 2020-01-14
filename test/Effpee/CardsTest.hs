module Effpee.CardsTest (suite) where

import Effpee
import Effpee.Test

-- module under test
import Effpee.Cards

suite
  = testGroup "Cards"
    -- evalColor test cases
    [ testCase "evalColor Diamonds == Red" $ Red @=? evalColor Diamonds
    , testCase "evalColor Hearts == Red"   $ Red @=? evalColor Hearts
    , testCase "evalColor Clubs == Black"  $ Black @=? evalColor Clubs
    , testCase "evalColor Spades == Black" $ Black @=? evalColor Spades
    ]
    -- scoreCard tests
    [  -- TODO: write some example-based tests for scoreCard function to give you
       -- enough confidence that it works given your chosen representation for Card
    ]
    -- scoreHand tests
    [  -- TODO: same as above but for scoreHand
    ]
    -- cardDeckFull tests
    [  -- TODO: same as above but for cardDeckFull
    ]
    -- cardDeckValid tests
    [  -- TODO: same as above but for cardDeckValid
    ]
