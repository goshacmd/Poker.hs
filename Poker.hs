module Poker where

import Cards
import Data.List
import Control.Applicative

type Hand = (Card, Card, Card, Card, Card)

data HandCategory = HighCard Face
                  | OnePair Face
                  | TwoPair Face Face
                  | ThreeOfAKind Face
                  | Straight Face
                  | Flush Suit Face
                  | FullHouse Face Face
                  | FourOfAKind Face
                  | StraightFlush Suit Face
                  deriving (Show, Eq)

listToHand :: [Card] -> Hand
listToHand (a:b:c:d:e:rest) = (a, b, c, d, e)

handToList :: Hand -> [Card]
handToList (a, b, c, d, e) = sort [a, b, c, d, e]

lastCard :: Hand -> Card
lastCard (_, _, _, _, a) = a

sortHandByFace :: Hand -> Hand
sortHandByFace = listToHand . handToList

consec :: Hand -> Bool
consec hand = consec' $ map face $ handToList hand

consec' :: (Eq a, Enum a) => [a] -> Bool
consec' (x:[]) = True
consec' (x:y:zs) | y == succ x = consec' $ y:zs
consec' _ = False

groups :: Hand -> [(Face, Int)]
groups h = map (\xs -> ((head xs), (length xs)))
            $ filter ((>1) . length)
            $ group
            $ map face
            $ handToList h

pairs :: Hand -> [Face]
pairs h = map fst $ groups h

listToPair :: [a] -> (a, a)
listToPair (x:y:_) = (x, y)

pairToTwoPair :: (Face, Face) -> HandCategory
pairToTwoPair = uncurry TwoPair

doublePairs :: Hand -> [(Face, Face)]
doublePairs h = map listToPair $ filter ((==2) . length) $ subsequences $ pairs h

triplets :: Hand -> [Face]
triplets h = map fst $ filter ((>2) . snd) $ groups h

highFace :: Hand -> Face
highFace = face . lastCard . sortHandByFace

highCard :: Hand -> HandCategory
highCard = HighCard . highFace

straight = consec

categories = ["highCard", "onePair", "twoPair", "threeOfAKind", "straight",
              "flush", "fullHouse", "fourOfAKind", "straightFlush"]

matches :: Hand -> [HandCategory]
matches h = concat $ map matched categories <*> [h]

allSuits :: Hand -> [Suit]
allSuits h = nub $ map suit $ handToList h

oneSuit :: Hand -> Suit
oneSuit = head . allSuits

allSuitsSame :: Hand -> Bool
allSuitsSame h = (==1) $ length $ allSuits h

flush = allSuitsSame

matched :: String -> Hand -> [HandCategory]
matched "highCard" h = return $ highCard h
matched "onePair" h = map OnePair $ pairs h
matched "twoPair" h = map (uncurry TwoPair) $ doublePairs h
matched "threeOfAKind" h = map ThreeOfAKind $ triplets h
matched "straight" h =
  if straight h
  then return $ Straight $ highFace h
  else []
matched "flush" h =
  if flush h
  then return $ Flush (oneSuit h) (highFace h)
  else []
matched "straightFlush" h =
  if flush h && straight h
  then return $ StraightFlush (oneSuit h) (highFace h)
  else []
matched _ _ = []

h = (Card Spades Ace, Card Diamonds Jack, Card Diamonds Seven, Card Hearts Three, Card Spades Queen)
hh = (Card Clubs Queen, Card Clubs King, Card Hearts King, Card Spades Ace, Card Diamonds Six)
th = (Card Clubs Queen, Card Diamonds Queen, Card Spades King, Card Hearts King, Card Hearts Queen)
fh = (Card Clubs Ace, Card Clubs Four, Card Clubs Nine, Card Clubs Ten, Card Clubs King)
ch = (Card Diamonds Five, Card Diamonds Six, Card Clubs Seven, Card Hearts Eight, Card Diamonds Nine)
cfh = (Card Clubs Two, Card Clubs Three, Card Clubs Four, Card Clubs Five, Card Clubs Six)
