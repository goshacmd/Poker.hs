module PokerEval
  where

import Cards
import Utils
import Data.List

type SHand = (Card, Card) -- Starting hand

data SHandCategory = Premium1 -- AA, KK
                   | Premium2 -- QQ, JJ, AK (suited)
                   | Premium3 -- TT, AK, AQ, AJ, KQ
                   | SuitedConnected
                   | Suited
                   | Connected
                   | Junk
                   deriving (Show)

evalStartingHand :: SHand -> SHandCategory
evalStartingHand h | premium1 h = Premium1
evalStartingHand h | premium2 h = Premium2
evalStartingHand h | premium3 h = Premium3
evalStartingHand h | suitedConnected h = SuitedConnected
evalStartingHand h | suited h = Suited
evalStartingHand h | connected h = Connected
evalStartingHand _ = Junk

-- Util

faces :: SHand -> [Face]
faces (a, b) = [face a, face b]

premium1 :: SHand -> Bool
premium1 = premium1' . faces

premium2 :: SHand -> Bool
premium2 h = premium2' (faces h) (suited h)

premium3 :: SHand -> Bool
premium3 = premium3' . sort . faces

premium1' :: [Face] -> Bool
premium1' fs = fs == [Ace, Ace] || fs == [King, King]

premium2' :: [Face] -> Bool -> Bool
premium2' fs suited = fs == [Queen, Queen] || fs == [Jack, Jack] || (suited && sort fs == [King, Ace])

premium3' :: [Face] -> Bool
premium3' fs = fs == [Ten, Ten] || fs == [King, Ace] || fs == [Queen, Ace] || fs == [Jack, Ace] || fs == [Queen, King]

pair :: SHand -> Bool
pair (a, b) = face a == face b

suitedConnected :: SHand -> Bool
suitedConnected h = suited h && connected h

suited :: SHand -> Bool
suited (a, b) = suit a == suit b

connected :: SHand -> Bool
connected = (==1) . gap

gap :: SHand -> Int
gap = abs . foldr (-) 0 . map (fromEnum . face) . sort . untuplify2
