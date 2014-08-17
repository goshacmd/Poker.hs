module Poker.Eval where

import Cards
import qualified Poker as P
import Utils
import Data.List
import Data.List.Subs
import Data.Tuple.Pack
import Control.Applicative
import Control.Arrow

type Pocket = (Card, Card) -- Starting hand

data Board = Flop  Card Card Card
           | Turn  Card Card Card Card
           | River Card Card Card Card Card
           deriving (Show)

type PocketBoard = (Pocket, Board)

data PocketCategory = Premium1 -- AA, KK
                    | Premium2 -- QQ, JJ, AK (suited)
                    | Premium3 -- TT, AK, AQ, AJ, KQ
                    | SuitedConnected
                    | Suited
                    | Connected
                    | Junk
                    deriving (Show)

evalPocket :: Pocket -> PocketCategory
evalPocket h | premium1 h        = Premium1
evalPocket h | premium2 h        = Premium2
evalPocket h | premium3 h        = Premium3
evalPocket h | suitedConnected h = SuitedConnected
evalPocket h | suited h          = Suited
evalPocket h | connected h       = Connected
evalPocket _                     = Junk

outs :: [Card] -> [Card]
outs xs = sort . nub. map snd
        . filter ((> P.bestRank xs) . P.rank . fst)
        . map (P.bestIn &&& head . (\\ xs))
        . possibleHands $ xs

-- Util

possibleHands :: [Card] -> [[Card]]
possibleHands = joinTup (:) . (deckWithout &&& subsequencesN 4)

possibleOpponentPockets :: [Card] -> [[Card]]
possibleOpponentPockets = subsequencesN 2 . deckWithout

possibleOpponentHands :: PocketBoard -> [P.HandCategory]
possibleOpponentHands = sort . nub
                      . app
                      . first (map . (P.bestIn . sort $$ (++)))
                      . (boardCards &&& possibleOpponentPockets . allCards)

betterOpponentHands :: PocketBoard -> [P.HandCategory]
betterOpponentHands = app
                    . first (filter. (<))
                    . (P.bestIn . allCards &&& possibleOpponentHands)

cards :: Board -> [Card]
cards (Flop a b c) = [a, b, c]
cards (Turn a b c d) = [a, b, c, d]
cards (River a b c d e) = [a, b, c, d, e]

pocketCards :: PocketBoard -> [Card]
pocketCards = unpackN . fst

boardCards :: PocketBoard -> [Card]
boardCards = cards . snd

allCards :: PocketBoard -> [Card]
allCards (p, b) = unpackN p ++ cards b

faces :: Pocket -> [Face]
faces (a, b) = [face a, face b]

premium1 :: Pocket -> Bool
premium1 = premium1' . faces

premium2 :: Pocket -> Bool
premium2 h = premium2' (sort $ faces h) (suited h)

premium3 :: Pocket -> Bool
premium3 = premium3' . sort . faces

premium1' :: [Face] -> Bool
premium1' fs = fs == [Ace, Ace] || fs == [King, King]

premium2' :: [Face] -> Bool -> Bool

premium2' fs suited = fs == [Queen, Queen] || fs == [Jack, Jack] || (suited && fs == [King, Ace])

premium3' :: [Face] -> Bool
premium3' fs = fs `elem` [[Ten, Ten], [King, Ace], [Queen, Ace], [Jack, Ace], [Queen, King]]

pair :: Pocket -> Bool
pair (a, b) = face a == face b

suitedConnected :: Pocket -> Bool
suitedConnected h = suited h && connected h

suited :: Pocket -> Bool
suited (a, b) = suit a == suit b

connected :: Pocket -> Bool
connected = consec . map face . unpackN
