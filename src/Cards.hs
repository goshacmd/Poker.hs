module Cards where

import Utils
import Data.Tuple.Pack (packN)
import Data.Function (on)
import Data.List ((\\))
import Control.Applicative

type Deck = [Card]

data Card = Card { suit :: Suit, face :: Face }
            deriving (Eq)

data Suit = Hearts | Diamonds | Clubs | Spades
          deriving (Show, Eq, Enum, Bounded)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
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
  show Ten   = "T"
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"

readSuit :: Char -> Suit
readSuit 'H' = Hearts
readSuit 'D' = Diamonds
readSuit 'C' = Clubs
readSuit 'S' = Spades

readFace :: Char -> Face
readFace '2' = Two
readFace '3' = Three
readFace '4' = Four
readFace '5' = Five
readFace '6' = Six
readFace '7' = Seven
readFace '8' = Eight
readFace '9' = Nine
readFace 'T' = Ten
readFace 'J' = Jack
readFace 'Q' = Queen
readFace 'K' = King
readFace 'A' = Ace

readCard :: (Char, Char) -> Card
readCard (f, s) = Card (readSuit s) (readFace f)

readCards :: [String] -> [Card]
readCards = map (readCard . packN)

deck :: Deck
deck = Card <$> bounds <*> bounds

suitedDeck :: Suit -> Deck -> Deck
suitedDeck = filter . fEq suit

facedDeck :: Face -> Deck -> Deck
facedDeck = facedDeck' . pure

facedDeck' :: [Face] -> Deck -> Deck
facedDeck' = filter . fIn face

without :: [Card] -> Deck -> Deck
without cards deck = deck \\ cards

deckWithout :: [Card] -> Deck
deckWithout = (\\) deck
