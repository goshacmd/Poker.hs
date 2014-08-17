module Utils
  where

import Data.List
import Data.Function

-- Check whether all elements of a list are consecutive.
consec :: (Eq a, Enum a) => [a] -> Bool
consec (x:[]) = True
consec (x:y:zs) | y == succ x = consec $ y:zs
consec _ = False

-- Repeat function f n times.
rep :: Int -> (a -> a) -> (a -> a)
rep 1 f = f
rep n f = f . rep (n - 1) f

joinF :: (a -> [a]) -> a -> [a]
joinF f x = x : f x

-- Join x and f x.
tupF :: (a -> b) -> a -> (a, b)
tupF f x = (x, f x)

-- Given a tuple (a, b) and f :: a -> b -> c, get c.
joinTup :: (a -> b -> c) -> (a, b) -> c
joinTup f (a, b) = f a b

-- Check whether f x equals val.
fEq :: Eq b => (a -> b) -> b -> (a -> Bool)
fEq f val = (== val) . f

-- Check whether f x is included in vals.
fIn :: Eq b => (a -> b) -> [b] -> (a -> Bool)
fIn f vals = flip elem vals . f

-- Sort and group elements based on return value of f.
groupedBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupedBy f = groupBy (on (==) f) . sortBy (on compare f)

-- Convert a Maybe List to List.
maybeList :: Maybe [a] -> [a]
maybeList (Just a) = a
maybeList Nothing  = []

-- Get the head of the list.
maybeHead :: [a] -> Maybe a
maybeHead (a:_) = Just a
maybeHead []    = Nothing

-- Double dot.
infixr 8 $$
($$) :: (c -> e) -> (a -> b -> c) -> a -> b -> e
($$) f1 f2 x = f1 . f2 x
