module Effpee.Suit
  ( Suit (..)
  ) where

import GHC.Enum
import GHC.Show

-- ** Card Suits
-- Represents suits in a deck of cards
-- >>> [Diamonds .. Hearts]
-- [Diamonds,Clubs,Hearts]
-- it :: [Suit]
data Suit
  = Diamonds
  | Clubs
  | Hearts
  | Spades
  deriving (Enum, Show)
