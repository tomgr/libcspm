module Util.PartialFunctions where

import List (nub)

-- *********************************************************************
-- Partial functions
-- *********************************************************************
type PartialFunction a b = [(a,b)] -- From a to b

functionDomain :: Eq a => PartialFunction a b -> [a]
functionDomain f = map fst f

functionImage :: Eq a => PartialFunction a b -> [b]
functionImage f = map snd f

identityFunction :: Eq a => [a] -> PartialFunction a a
identityFunction xs = [(x,x) | x <- xs]

invert :: (Eq a, Eq b) => PartialFunction a b -> PartialFunction b a
invert f = [(a,b) | (b,a) <- f]

apply :: Eq a => PartialFunction a b -> a -> b
apply f x =
	let
		pos = [b | (a,b) <- f, a == x]
	in
		if length pos == 0 then 
			error ("Partial function applied to value outside of domain")
		else head pos
		
applyRelation :: Eq a => PartialFunction a b -> a -> [b]
applyRelation f x = [b | (a,b) <- f, a == x]

safeApply :: Eq a => PartialFunction a b -> a -> Maybe b
safeApply f x =
	let
		pos = [b | (a,b) <- f, a == x]
	in
		if length pos == 0 then Nothing
		else Just (head pos)

composeFunctions :: 
	(Eq a, Eq b) => PartialFunction b c -> PartialFunction a b -> 
					PartialFunction a c
composeFunctions f g = 
	[(a, apply f b) | (a,b) <- g]

mapPF :: Eq a => PartialFunction a b -> [a] -> [b]
mapPF f xs = [apply f x | x <- xs]

safeMapPF :: Eq a => PartialFunction a b -> [a] -> [b]
safeMapPF f xs = [x | Just x <- [safeApply f x | x <- xs]]

updatePF :: Eq a => PartialFunction a b -> a -> b -> PartialFunction a b
updatePF f a b = (a,b):[(c,d) | (c,d) <- f, c /= a]

removeEntry :: Eq a => PartialFunction a b -> a -> PartialFunction a b
removeEntry f a = [(c,d) | (c,d) <- f, c /= a]
