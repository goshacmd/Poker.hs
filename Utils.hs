module Utils where

import Data.List

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
