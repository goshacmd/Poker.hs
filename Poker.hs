module Poker
  where

import Cards
import Data.Function
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Arrow

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
                  deriving (Show, Eq, Ord)

-- Hand category detection

highCard :: Hand -> HandCategory
highCard = HighCard . highFace

onePair :: Hand -> [HandCategory]
onePair = map OnePair . pairs

twoPair :: Hand -> [HandCategory]
twoPair = map pairToTwoPair . doublePairs

threeOfAKind :: Hand -> [HandCategory]
threeOfAKind = map ThreeOfAKind . triplets

straight :: Hand -> Maybe HandCategory
straight h | isStraight h = return . Straight $ highFace h
straight _ = Nothing

flush :: Hand -> Maybe HandCategory
flush h | isFlush h = return $ Flush (oneSuit h) (highFace h)
flush _ = Nothing

straightFlush :: Hand -> Maybe HandCategory
straightFlush h | isStraightFlush h = return $ StraightFlush (oneSuit h) (highFace h)
straightFlush _ = Nothing

matches :: Hand -> [HandCategory]
matches h = concat $
  [
    return . highCard,
    onePair,
    twoPair,
    threeOfAKind,
    maybeToList . straight,
    maybeToList . flush,
    maybeToList . straightFlush
  ] <*> [h]

bestHandCategory :: Hand -> HandCategory
bestHandCategory = maximum . matches

winningHand :: [Hand] -> (Hand, HandCategory)
winningHand = maximumBy (on compare snd) . map (\h -> (h, bestHandCategory h))

-- Sample hands

s_hc = makeHand [(Spades, Ace), (Diamonds, Jack), (Diamonds, Seven), (Hearts, Three), (Spades, Queen)]
s_sf = makeHand [(Hearts, Three), (Hearts, Four), (Hearts, Five), (Hearts, Six), (Hearts, Seven), (Hearts, Eight)]

-- Util

listToHand :: [Card] -> Hand
listToHand (a:b:c:d:e:rest) = (a, b, c, d, e)

handToList :: Hand -> [Card]
handToList (a, b, c, d, e) = sort [a, b, c, d, e]

makeHand :: [(Suit, Face)] -> Hand
makeHand = listToHand . map (uncurry Card)

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
groups h = map (head &&& length)
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

allSuits :: Hand -> [Suit]
allSuits h = nub $ map suit $ handToList h

oneSuit :: Hand -> Suit
oneSuit = head . allSuits

allSuitsSame :: Hand -> Bool
allSuitsSame h = (==1) $ length $ allSuits h

isFlush = allSuitsSame
isStraight = consec
isStraightFlush h = isStraight h && isFlush h
