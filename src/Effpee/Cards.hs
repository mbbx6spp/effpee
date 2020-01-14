module Effpee.Cards where

import Control.Monad (Monad)
import Effpee

-- | An example of an "enum" type or simple "sum" type representing color of a suit
data Color
  -- | Red
  = Red
  -- | Black
  | Black
  deriving (Generic, Show)

-- | Represents the suits in a deck of cards
-- This should include: clubs, diamonds, hearts, spades
data Suit
  -- | Clubs
  = Clubs
  -- | Diamonds
  -- TODO: flesh out the rest of this "enum" type (also called "sum" types)
  deriving (Generic, Show)

-- | Takes a value of `Suit` and produces the corresponding `Color`.
-- TODO: implement this function
evalColor :: Suit -> Color
evalColor = todo "Effpee.Cards.evalColor"

-- | Represents the rank of a card in a deck. The rank would be numeric or picture on
-- the card independent of suit or color.
-- TODO: Decide how you want to model this data as there are multiple encodings that
-- equivalent and have different trade-offs depending how you need to use this data.
-- There is no /one perfect way/ to encode this without the additional context of usage.
data Rank
  = Ace
  | King
  | Queen
  | Jack
  -- TODO: decide if the following data constructor will always yield valid cards in a
  -- typical deck of cards? What happens if a caller passes negative integers as the
  -- argument? How about larger values? There are many choices to be made here, write
  -- some notes of your internal thought process to discuss together next week.
  | Numeric Int
  deriving (Generic, Show)

-- | Represents the card composed of the rank and suit.
-- TODO: Fill in the blanks
data Card
  = MkCard
    -- TODO: Uncomment the front of the following three lines and then fill in the types
    -- { suit :: _ -- ^ <- fill in the type here
    -- , rank :: _ -- ^ <- fill in the type here
    -- }
    deriving (Generic, Show)

-- NOTES: sometime we don't need to define a new enum, sum of products or product type.
-- We might just need to alias or wrap an existing type. Aliasing a type merely gives
-- better readability in Haskell source. There is no type checking differentiating
-- between the alias target or the alias. However, we might want to make sure that
-- a numeric type is wrapped up as a `Celsius` to distinguish at typechecking from
-- `Kelvin`. There are different trade-offs and you might need one over the other
-- for different purposes, even when pointing to the same alias target. Using `type`
-- gives us type aliasing (I wish the keyword was `alias` but Haskell has made lots
-- of legacy decision like every older language, so I forgive it). We can declare
-- a `newtype` as a way to "wrap" a type target and providing strong type checking
-- guarantees without run-time overhead.

-- | Represents a "hand" that a player has available to them at any time.
type Hand = [Card]

-- TODO: You can comment out the alias for the newtype at the end of all the exercises
-- to see if it compiles and if not, do the exercise of fixing all the type checking
-- errors. This will help inform you of the relative strengths of each approach
-- so you can choose the appropriate technique for your subsequent applications
-- of this.
-- newtype Hand = MkHand [Card]


-- NOTES: We sometimes have a need to define recursive structures. We could represent
-- a deck of cards as such a recursive structure representing the "head" of the deck.
data CardDeck
  = CardDeckEnd Card
  | CardDeckNext CardDeck
  deriving (Generic, Show)

-- This is another way to represent a deck of cards by using a newtype wrapper.
newtype CardDeckWrapper
  = MkCardDeck { unDeck :: [Card] }

-- | a function that consumes a `Card` value and produces its score.
-- We will assume for this exercise that Ace produces 13.
scoreCard :: Card -> Int
scoreCard = todo "Effpee.Cards.scoreCard"

-- | a function that, when given a `Hand`, will sum up the scores of each `Card` in
-- the `Hand``.
scoreHand :: Hand -> Int
scoreHand = todo "Effpee.Cards.scoreCard"

-- | a function that checks whether the deck of cards is complete in count.
-- i.e. it checks that there is exactly 52 cards in the deck given.
cardDeckFull :: CardDeck -> Bool
cardDeckFull = todo "Effpee.Cards.cardDeckFull"

-- | a function that checks whether the deck of cards has 52 unique cards such that
-- only cards from one deck of cards is present (i.e. we haven't mixed up cards from
-- two decks).
cardDeckValid :: CardDeck -> Bool
cardDeckValid = todo "Effpee.Cards.cardDeckValid"

-- TODO: Spotter needs to define and introduce the Random effect in the Effpee.Random
-- module first before we tackle this. Ignore for the first week.
shuffle :: Monad m => CardDeck -> m CardDeck
shuffle = todo "Effpee.Cards.shuffle"
