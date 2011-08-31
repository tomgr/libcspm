module CSPM.Compiler.Set ( 
    Set,
    null,
    member,
    empty,
    unions,
    union,
    difference,
    fromList, 
    toList,
    unionMap,
    split,
    subseteq,
) where

import Data.List ((\\), nub)
import Prelude hiding (null)

type Set a = [a]

null s = s == []
empty = []
unions xss = nub (concat xss)
union xs ys = nub (xs ++ ys)
difference xs ys = xs \\ ys
fromList = id
toList = id
member x xs = elem x xs
subseteq xs ys = and (map (\x -> x `elem` ys) xs)

unionMap :: Ord b => (a -> Set b) -> Set a -> Set b
unionMap f = unions . map f . toList

-- TODO: replace with iterOverSet :: Set a -> (a -> m b) -> m (Set b) ?
split :: Set a -> (a, Set a)
split (x:xs) = (x,xs)


{- TODO use http://hackage.haskell.org/packages/archive/hashtables/1.0.0.0/doc/html/Data-HashTable-Class.html#v:new #-}