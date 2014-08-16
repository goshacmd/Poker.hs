{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Utils
  where

import Data.List

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

fEq :: Eq b => (a -> b) -> b -> (a -> Bool)
fEq f val = (== val) . f

maybeList :: Maybe [a] -> [a]
maybeList (Just a) = a
maybeList Nothing  = []

maybeHead :: [a] -> Maybe a
maybeHead (a:_) = Just a
maybeHead []    = Nothing
