module Poker where

import Cards
import Utils
import Data.Function
import Data.List
import Data.List.Grouping
import Data.List.Subs
import Data.Tuple.Curry
import Data.Tuple.Pack
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Arrow

type Hand = (Card, Card, Card, Card, Card)

type Kicker = Face

data HandCategory = HighCard Kicker Kicker Kicker Kicker Kicker
                  | OnePair Face Kicker Kicker Kicker
                  | TwoPair Face Face Kicker
                  | ThreeOfAKind Face Kicker Kicker
                  | Straight Face
                  | Flush Suit Face
                  | FullHouse Face Face
                  | FourOfAKind Face Kicker
                  | StraightFlush Suit Face
                  deriving (Show, Eq, Ord)

data HandRank = HighCardR | OnePairR | TwoPairR | ThreeOfAKindR | StraightR | FlushR | FullHouseR | FourOfAKindR | StraightFlushR
                deriving (Show, Eq, Ord)

rank :: HandCategory -> HandRank
rank HighCard{}      = HighCardR
rank OnePair{}       = OnePairR
rank TwoPair{}       = TwoPairR
rank ThreeOfAKind{}  = ThreeOfAKindR
rank Straight{}      = StraightR
rank Flush{}         = FlushR
rank FullHouse{}     = FullHouseR
rank FourOfAKind{}   = FourOfAKindR
rank StraightFlush{} = StraightFlushR

-- Hand category detection

highCard :: Hand -> HandCategory
highCard = uncurryN HighCard . pack5 . reverse . highFaces

onePair :: Hand -> [HandCategory]
onePair = map (uncurryN OnePair) . pairsWithKickers . handToList

twoPair :: Hand -> [HandCategory]
twoPair = map (uncurryN TwoPair) . doublePairs

threeOfAKind :: Hand -> [HandCategory]
threeOfAKind = map (uncurryN ThreeOfAKind) . tripletsWithKickers . handToList

straight :: Hand -> Maybe HandCategory
straight h | isStraight h = return . Straight $ highFace h
straight _ = Nothing

flush :: Hand -> Maybe HandCategory
flush h | isFlush h = return $ Flush (oneSuit h) (highFace h)
flush _ = Nothing

fullHouse :: Hand -> Maybe HandCategory
fullHouse h = uncurryN FullHouse <$> oneFullHouse h

fourOfAKind :: Hand -> Maybe HandCategory
fourOfAKind h = uncurryN FourOfAKind <$> oneSet h

straightFlush :: Hand -> Maybe HandCategory
straightFlush h | isStraightFlush h = return $ StraightFlush (oneSuit h) (highFace h)
straightFlush _ = Nothing

matches :: Hand -> [HandCategory]
matches h = sort . concat $
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

matchesIn :: [Card] -> [HandCategory]
matchesIn = concatMap (matches . listToHand) . subsequencesN 5

bestHandCategory :: Hand -> HandCategory
bestHandCategory = maximum . matches

bestIn :: [Card] -> HandCategory
bestIn = maximum . matchesIn

bestRank :: [Card] -> HandRank
bestRank = rank . bestIn

winningHand :: [Hand] -> (Hand, HandCategory)
winningHand = maximumBy (on compare snd) . map (tupF bestHandCategory)

-- Util

listToHand :: [Card] -> Hand
listToHand = packN

handToList :: Hand -> [Card]
handToList = sort . unpackN

makeHand :: [(Suit, Face)] -> Hand
makeHand = listToHand . map (uncurry Card)

lastCard :: Hand -> Card
lastCard (_, _, _, _, a) = a

sortHandByFace :: Hand -> Hand
sortHandByFace = listToHand . handToList

consecH :: Hand -> Bool
consecH = consec . map face . handToList

groups :: [Card] -> [(Face, Int)]
groups = map (face . head &&& length)
       . filter ((>1) . length)
       . groupedBy face

groupsWithKickers :: Int -> [Card] -> [(Face, [Kicker])]
groupsWithKickers n = map ((face . head) *** map face)
                    . sizedGroupsWithRest face n

groupsWithCount :: (Int -> Bool) -> [Card] -> [Face]
groupsWithCount f = map fst . filter (f . snd) . groups

pairsWithKickers :: [Card] -> [(Face, Kicker, Kicker, Kicker)]
pairsWithKickers = map (packN . joinTup (:)) . groupsWithKickers 2

pairs :: [Card] -> [Face]
pairs = map fst . groups

doublePairs :: Hand -> [(Face, Face, Kicker)]
doublePairs h = map withKicker . doublePairs' . handToList $ h
  where fs = map face $ handToList h
        withKicker (x, y) = (x, y, head $ fs \\ [x, y])

doublePairs' :: [Card] -> [(Face, Face)]
doublePairs' = map packN . filter ((==2) . length) . subsequences . pairs

tripletsWithKickers :: [Card] -> [(Face, Kicker, Kicker)]
tripletsWithKickers = map (packN . joinTup (:)) . groupsWithKickers 3

exactlyPairs :: [Card] -> [Face]
exactlyPairs = groupsWithCount (==2)

exactlyTriplets :: [Card] -> [Face]
exactlyTriplets = groupsWithCount (==3)

exactlySets :: [Card] -> [(Face, Kicker)]
exactlySets = map (packN . joinTup (:)) . groupsWithKickers 4

oneFullHouse :: Hand -> Maybe (Face, Face)
oneFullHouse h = f (maybeHead p) (maybeHead t)
  where p = exactlyPairs $ handToList h
        t = exactlyTriplets $ handToList h
        f a b = pack2 <$> sequence [a, b]

oneSet :: Hand -> Maybe (Face, Kicker)
oneSet = maybeHead . exactlySets . handToList

highFaces :: Hand -> [Face]
highFaces = map face . handToList

highFace :: Hand -> Face
highFace = last . highFaces

allSuits :: Hand -> [Suit]
allSuits = nub . map suit . handToList

oneSuit :: Hand -> Suit
oneSuit = head . allSuits

allSuitsSame :: Hand -> Bool
allSuitsSame = (==1) . length . allSuits

isFlush = allSuitsSame
isStraight = consecH
isStraightFlush h = isStraight h && isFlush h
