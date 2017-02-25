module ComplexList where

import Data.Ord
import Data.List

complexSort :: [(a -> a -> Ordering)] -> [a] -> [a]
complexSort fields = sortBy comparator
    where
        comparator = mconcat fields

complexFilter :: [(a -> Bool)] -> [a] -> [a]
complexFilter filters = filter globalFilter
    where
        globalFilter e = foldl (\acc f -> acc && f e) True filters
