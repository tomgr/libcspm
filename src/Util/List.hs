module Util.List where
import Data.List

-- TODO: speed up
noDups :: Eq a => [a] -> Bool
noDups xs = nub xs == xs
