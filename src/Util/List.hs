module Util.List where

-- | Removes all duplicates, assuming the list is sorted.
sortedNub :: Eq a => [a] -> [a]
sortedNub [] = []
sortedNub [x] = [x]
sortedNub (x:y:xs) | x == y = sortedNub (x:xs)
sortedNub (x:xs) = x : sortedNub xs

-- | Replaces the last item in a list. Assumes the list is non empty.
replaceLast :: [a] -> a -> [a]
replaceLast [_] v = [v]
replaceLast (x:xs) v = x : replaceLast xs v
replaceLast _ _ = error "Unsupported call to replaceLast"

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct vss = sequence vss

-- | Returns the first duplicate if in the given sorted list.
firstDuplicate :: Eq a => [a] -> Maybe a
firstDuplicate [] = Nothing
firstDuplicate [_] = Nothing
firstDuplicate (x:y:xs) =
    if x == y then Just x
    else firstDuplicate (y:xs)
