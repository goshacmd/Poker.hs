module Poker.Eval
  where

import Cards
import qualified Poker as P
import Utils
import Data.List
import Control.Applicative

type Pocket = (Card, Card) -- Starting hand

type CommunityFlop = (Card, Card, Card)

data PocketCategory = Premium1 -- AA, KK
                    | Premium2 -- QQ, JJ, AK (suited)
                    | Premium3 -- TT, AK, AQ, AJ, KQ
                    | SuitedConnected
                    | Suited
                    | Connected
                    | Junk
                    deriving (Show)

evalPocket :: Pocket -> PocketCategory
evalPocket h | premium1 h = Premium1
evalPocket h | premium2 h = Premium2
evalPocket h | premium3 h = Premium3
evalPocket h | suitedConnected h = SuitedConnected
evalPocket h | suited h = Suited
evalPocket h | connected h = Connected
evalPocket _ = Junk

outs :: [Card] -> [Card]
outs h = sort . nub . concat $
  [
    flushDrawOuts,
    twoPairToHouseDrawOuts
  ] <*> [sort h]

flushDrawOuts :: [Card] -> [Card]
flushDrawOuts = maybeList . fmap outsForFlushDraw . flushDraw

twoPairToHouseDrawOuts :: [Card] -> [Card]
twoPairToHouseDrawOuts xs = d \\ xs
  where d = facedDeck' (nub $ concatMap untuplify2 $ P.doublePairs' xs) deck

-- Util

type FlushDraw = ([Card], Suit)

flushDraw :: [Card] -> Maybe FlushDraw
flushDraw = maybeHead . flushDraws

outsForFlushDraw :: FlushDraw -> [Card]
outsForFlushDraw (cards, suit) = without cards $ suitedDeck suit deck

flushDraws :: [Card] -> [([Card], Suit)]
flushDraws = map withSuit . filter (fEq length 4) . groupedBy suit
  where withSuit cards@(a:_) = (cards, suit a)

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
connected = (==1) . gap

gap :: Pocket -> Int
gap = abs . foldr (-) 0 . map (fromEnum . face) . sort . untuplify2
