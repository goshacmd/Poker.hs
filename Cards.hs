module Cards (Deck, Card(..), Suit(..), Face(..), deck)
  where

import Data.Function
import Data.List
import Data.Ord
import Control.Applicative

type Deck = [Card]

data Card = Card { suit :: Suit, face :: Face }
            deriving (Eq)

data Suit = Hearts | Diamonds | Clubs | Spades
          deriving (Show, Eq, Enum, Bounded)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
             | Jack | Queen | King | Ace
          deriving (Eq, Ord, Enum, Bounded)

instance Ord Card where
  compare = on compare face

instance Show Card where
  show (Card s f) = show f ++ [head $ show s]

instance Ord Suit where
  compare _ _ = EQ

instance Show Face where
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "10"
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"

deck :: Deck
deck = Card <$> [minBound..maxBound] <*> [minBound..maxBound]
