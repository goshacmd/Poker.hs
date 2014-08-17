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
evalPocket p = ep (faces p) (suited p)

aa = [Ace, Ace]
kk = [King, King]
qq = [Queen, Queen]
jj = [Jack, Jack]
tt = [Ten, Ten]
ak = [Ace, King]
aq = [Ace, Queen]
aj = [Ace, Jack]
kq = [King, Queen]

ep :: [Face] -> Bool -> PocketCategory
ep f _    | f `elem` [aa, kk]             = Premium1
ep f _    | f `elem` [qq, jj]             = Premium2
ep f True | f == ak                       = Premium2
ep f _    | f `elem` [tt, ak, aq, aj, kq] = Premium3
ep f True | consec f                      = SuitedConnected
ep _ True                                 = Suited
ep f _    | consec f                      = Connected
ep _ _                                    = Junk

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
possibleOpponentHands = boardCards &&& possibleOpponentPockets . allCards
                      >>> first (map . (P.bestIn . sort $$ (++)))
                      >>> app >>> nub >>> sort

betterOpponentHands :: PocketBoard -> [P.HandCategory]
betterOpponentHands = P.bestIn . allCards &&& possibleOpponentHands
                    >>> first (filter . (<)) >>> app

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

suited :: Pocket -> Bool
suited (a, b) = suit a == suit b
