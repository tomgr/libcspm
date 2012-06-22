module Util.List where

import Data.List

-- TODO: speed up
-- | Returns true iff the list has no duplicates.
noDups :: Eq a => [a] -> Bool
noDups xs = nub xs == xs

-- | Replaces the last item in a list. Assumes the list is non empty.
replaceLast :: [a] -> a -> [a]
replaceLast [_] v = [v]
replaceLast (x:xs) v = x : replaceLast xs v
replaceLast _ _ = error "Unsupported call to replaceLast"

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct vss = sequence vss
