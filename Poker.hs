module Poker
  where

import Cards
import Utils
import Data.Function
import Data.List
import Data.Tuple.Curry
import Data.Maybe
import Control.Applicative
import Control.Arrow

type Hand = (Card, Card, Card, Card, Card)

type Kicker = Face

data HandCategory = HighCard Kicker Kicker Kicker Kicker Kicker
                  | OnePair Face Kicker Kicker Kicker
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
highCard = uncurryN HighCard . tuplify5 . reverse . highFaces

onePair :: Hand -> [HandCategory]
onePair = map (uncurryN OnePair) . pairsWithKickers

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

fullHouse :: Hand -> Maybe HandCategory
fullHouse h = uncurryN FullHouse `fmap` oneFullHouse h

fourOfAKind :: Hand -> Maybe HandCategory
fourOfAKind h = FourOfAKind `fmap` oneSet h

straightFlush :: Hand -> Maybe HandCategory
straightFlush h | isStraightFlush h = return $ StraightFlush (oneSuit h) (highFace h)
straightFlush _ = Nothing

matches :: Hand -> [HandCategory]
matches h = sort $ concat $
  [
    return . highCard,
    onePair,
    twoPair,
    threeOfAKind,
    maybeToList . straight,
    maybeToList . flush,
    maybeToList . fullHouse,
    maybeToList . fourOfAKind,
    maybeToList . straightFlush
  ] <*> [h]

bestHandCategory :: Hand -> HandCategory
bestHandCategory = maximum . matches

winningHand :: [Hand] -> (Hand, HandCategory)
winningHand = maximumBy (on compare snd) . map (tupF bestHandCategory)

-- Sample hands

s_hc = makeHand [(Spades, Ace), (Diamonds, Jack), (Diamonds, Seven), (Hearts, Three), (Spades, Queen)]
s_sf = makeHand [(Hearts, Three), (Hearts, Four), (Hearts, Five), (Hearts, Six), (Hearts, Seven), (Hearts, Eight)]
s_p  = makeHand [(Diamonds, Five), (Spades, Five), (Hearts, Seven), (Spades, Queen), (Clubs, Ace)]
s_fh = makeHand [(Diamonds, Two), (Spades, Two), (Hearts, Two), (Clubs, King), (Hearts, King)]
s_s  = makeHand [(Diamonds, Three), (Spades, Three), (Spades, Four), (Clubs, Three), (Hearts, Three)]

-- Util

listToHand :: [Card] -> Hand
listToHand = tuplify5

handToList :: Hand -> [Card]
handToList = sort . untuplify5

makeHand :: [(Suit, Face)] -> Hand
makeHand = listToHand . map (uncurry Card)

lastCard :: Hand -> Card
lastCard (_, _, _, _, a) = a

sortHandByFace :: Hand -> Hand
sortHandByFace = listToHand . handToList

consec :: Hand -> Bool
consec hand = consec' $ map face $ handToList hand

groups :: Hand -> [(Face, Int)]
groups h = map (head &&& length)
         $ filter ((>1) . length)
         $ group
         $ map face
         $ handToList h

groupsWithCount :: (Int -> Bool) -> Hand -> [Face]
groupsWithCount f = map fst . filter (f . snd) . groups

pairsWithKickers :: Hand -> [(Face, Kicker, Kicker, Kicker)]
pairsWithKickers h = map (tuplify4 . joinF rem . head)
                   $ filter ((>=2) . length)
                   $ group fs
  where fs = map face $ handToList h
        rem = sortBy (flip compare) . flip (rep 2 . delete) fs

pairs :: Hand -> [Face]
pairs h = map fst $ groups h

listToPair :: [a] -> (a, a)
listToPair (x:y:_) = (x, y)

pairToTwoPair :: (Face, Face) -> HandCategory
pairToTwoPair = uncurry TwoPair

doublePairs :: Hand -> [(Face, Face)]
doublePairs = map listToPair . filter ((==2) . length) . subsequences . pairs

triplets :: Hand -> [Face]
triplets = groupsWithCount (>=3)

exactlyPairs :: Hand -> [Face]
exactlyPairs = groupsWithCount (==2)

exactlyTriplets :: Hand -> [Face]
exactlyTriplets = groupsWithCount (==3)

exactlySets :: Hand -> [Face]
exactlySets = groupsWithCount (==4)

oneFullHouse :: Hand -> Maybe (Face, Face)
oneFullHouse h = f (maybeHead p) (maybeHead t)
  where p = exactlyPairs h
        t = exactlyTriplets h
        f (Just pp) (Just tt) = Just (pp, tt)
        f _ _ = Nothing

oneSet :: Hand -> Maybe Face
oneSet = maybeHead . exactlySets

highFaces :: Hand -> [Face]
highFaces = map face . handToList

highFace :: Hand -> Face
highFace = last . highFaces

allSuits :: Hand -> [Suit]
allSuits h = nub $ map suit $ handToList h

oneSuit :: Hand -> Suit
oneSuit = head . allSuits

allSuitsSame :: Hand -> Bool
allSuitsSame h = (==1) $ length $ allSuits h

isFlush = allSuitsSame
isStraight = consec
isStraightFlush h = isStraight h && isFlush h
