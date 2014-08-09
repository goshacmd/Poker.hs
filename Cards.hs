module Cards (Deck, Card(..), Suit(..), Face(..), suit, face, deck)
  where

import Data.List
import Data.Ord
import Control.Applicative

type Deck = [Card]

data Card = Card Suit Face
            deriving (Show, Eq)

data Suit = Spades | Hearts | Diamonds | Clubs
          deriving (Show, Eq, Enum, Bounded)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
             | Jack | Queen | King | Ace
          deriving (Show, Eq, Ord, Enum, Bounded)

instance Ord Card where
  compare a b = compare (face a) (face b)

suit :: Card -> Suit
suit (Card s _) = s

face :: Card -> Face
face (Card _ f) = f

deck :: Deck
deck = Card <$> [minBound..maxBound] <*> [minBound..maxBound]
