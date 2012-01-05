{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module CSPM.Compiler.Map ( 
    Map,
    lookup,
    empty,
    insert,
    domain, unSafeLookup,
    toList,
    fromList,
    Relation, applyRelation
) where

import Data.List (nub)
import Prelude hiding (lookup)
import Util.PartialFunctions

-- TODO
type Relation k v = Map k v
type Map k v = PartialFunction k v

instance (Eq k, Eq v) => Eq (Map k v) where
    m1 == m2 = and [lookup k m1 == lookup k m1 | k <- domain m1]
    
lookup k m = safeApply m k
empty = []
insert k v m = (k,v):m
domain m = nub (functionDomain m)
unSafeLookup k m = case lookup k m of
    Just v -> v
    Nothing -> error "no value"
toList = id
fromList = id
