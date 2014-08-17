module Utils
  where

import Data.List
import Data.Function

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

fIn :: Eq b => (a -> b) -> [b] -> (a -> Bool)
fIn f vals = flip elem vals . f

groupedBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupedBy f = groupBy (on (==) f) . sortBy (on compare f)

maybeList :: Maybe [a] -> [a]
maybeList (Just a) = a
maybeList Nothing  = []

maybeHead :: [a] -> Maybe a
maybeHead (a:_) = Just a
maybeHead []    = Nothing
