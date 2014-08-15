{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Utils (tuplify5,
              untuplify5,
              tuplify4,
              tuplify2,
              untuplify2,
              consec',
              rep,
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

tuplify2 :: [a] -> (a, a)
tuplify2 (a:b:_) = (a, b)

untuplify2 :: (a, a) -> [a]
untuplify2 (a, b) = [a, b]

consec' :: (Eq a, Enum a) => [a] -> Bool
consec' (x:[]) = True
consec' (x:y:zs) | y == succ x = consec' $ y:zs
consec' _ = False

rep :: Int -> (a -> a) -> (a -> a)
rep 1 f = f
rep n f = f . rep (n - 1) f

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
