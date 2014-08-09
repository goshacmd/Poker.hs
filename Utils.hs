module Utils (tuplify5,
              untuplify5,
              tuplify4,
              consec',
              removeTwo,
              joinF,
              tupF)
  where

import Data.List
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Control.Monad.Random
import System.Random
import GHC.Arr

tuplify5 :: [a] -> (a, a, a, a, a)
tuplify5 (a:b:c:d:e:_) = (a, b, c, d, e)

untuplify5 :: (a, a, a, a, a) -> [a]
untuplify5 (a, b, c, d, e) = [a, b, c, d, e]

tuplify4 :: [a] -> (a, a, a, a)
tuplify4 (a:b:c:d:_) = (a, b, c, d)

consec' :: (Eq a, Enum a) => [a] -> Bool
consec' (x:[]) = True
consec' (x:y:zs) | y == succ x = consec' $ y:zs
consec' _ = False

removeTwo :: (Eq a) => a -> [a] -> [a]
removeTwo x = delete x . delete x

joinF :: (a -> [a]) -> a -> [a]
joinF f x = x : f x

tupF :: (a -> b) -> a -> (a, b)
tupF f x = (x, f x)

shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
  let l = length xs
  rands <- take l `fmap` getRandomRs (0, l-1)
  let ar = runSTArray $ do
      ar <- thawSTArray $ listArray (0, l-1) xs
      forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
        vi <- readSTArray ar i
        vj <- readSTArray ar j
        writeSTArray ar j vi
        writeSTArray ar i vj
      return ar
  return (elems ar)
