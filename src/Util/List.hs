module Util.List where
import Data.List

-- TODO: speed up
-- | Returns true iff the list has no duplicates.
noDups :: Eq a => [a] -> Bool
noDups xs = nub xs == xs
