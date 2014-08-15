module PokerEval
  where

import Cards
import Utils
import Data.List

type SHand = (Card, Card) -- Starting hand

-- Util

pair :: SHand -> Bool
pair (a, b) = face a == face b

suited :: SHand -> Bool
suited (a, b) = suit a == suit b

connected :: SHand -> Bool
connected = (==1) . gap

gap :: SHand -> Int
gap = abs . foldr (-) 0 . map (fromEnum . face) . sort . untuplify2
