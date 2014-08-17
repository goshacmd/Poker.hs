module Data.List.Grouping where

import Utils (tupF)
import Data.List (groupBy, sortBy, (\\))
import Data.Function (on)

-- Sort and group elements based on return value of f.
groupedBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupedBy f = groupBy (on (==) f) . sortBy (on compare f)

-- Group elements based on f, join grouped elements and the rest into a tuple.
-- > groupsWithRest id [1,2,3,1,1]
-- [ ([1,1,1], [2,3]), ([2], [1,1,1,3]), ([3], [1,1,1,2]) ]
groupsWithRest :: (Eq b, Ord b) => (a -> b) -> [a] -> [([a], [a])]
groupsWithRest f xs = map (tupF ff)
                    $ groupedBy f xs
  where filterOut = on (/=) f
        ff (x:_) = sortBy (on compare f) . filter (filterOut x) $ xs

sizedGroupsWithRest :: (Eq b, Ord b) => (a -> b) -> Int -> [a] -> [([a], [a])]
sizedGroupsWithRest f n = map move
                        . filter ((>=n) . length . fst)
                        . groupsWithRest f
  where move (as@(a0:a1), bs) = if length as > n
                                then move (a1, a0:bs)
                                else (as, bs)
