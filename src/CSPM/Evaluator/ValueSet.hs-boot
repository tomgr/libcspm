module CSPM.Evaluator.ValueSet where

import Data.Hashable

import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.PrettyPrint

data ValueSet

instance Eq ValueSet
instance Hashable ValueSet
instance Ord ValueSet
instance PrettyPrintable ValueSet

toList :: ValueSet -> [Value]
fromList :: [Value] -> ValueSet
cartesianProduct :: ([Value] -> Value) -> [ValueSet] -> ValueSet
compareValueSets :: ValueSet -> ValueSet -> Maybe Ordering
