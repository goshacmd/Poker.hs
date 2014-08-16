module PokerEval
  where

import Cards
import Utils
import Data.List

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

-- Util

faces :: Pocket -> [Face]
faces (a, b) = [face a, face b]

premium1 :: Pocket -> Bool
premium1 = premium1' . faces

premium2 :: Pocket -> Bool
premium2 h = premium2' (faces h) (suited h)

premium3 :: Pocket -> Bool
premium3 = premium3' . sort . faces

premium1' :: [Face] -> Bool
premium1' fs = fs == [Ace, Ace] || fs == [King, King]

premium2' :: [Face] -> Bool -> Bool
premium2' fs suited = fs == [Queen, Queen] || fs == [Jack, Jack] || (suited && sort fs == [King, Ace])

premium3' :: [Face] -> Bool
premium3' fs = fs == [Ten, Ten] || fs == [King, Ace] || fs == [Queen, Ace] || fs == [Jack, Ace] || fs == [Queen, King]

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
