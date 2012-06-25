module CSPM.Evaluator.ValueSet where

import Data.Hashable

import {-# SOURCE #-} CSPM.Evaluator.Values

data ValueSet
data CartProductType = CartDot | CartTuple

instance Eq ValueSet
instance Hashable ValueSet
instance Ord ValueSet

toList :: ValueSet -> [Value]
fromList :: [Value] -> ValueSet
cartesianProduct :: CartProductType -> [ValueSet] -> ValueSet
compareValueSets :: ValueSet -> ValueSet -> Maybe Ordering
member :: Value -> ValueSet -> Bool
unDotProduct :: ValueSet -> Maybe [ValueSet]
difference :: ValueSet -> ValueSet -> ValueSet
