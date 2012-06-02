module CSPM.Evaluator.ValueSet where

import Data.Hashable

import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.PrettyPrint

data ValueSet
data CartProductType

instance Eq ValueSet
instance Hashable ValueSet
instance Ord ValueSet
instance PrettyPrintable ValueSet
instance Show ValueSet

toList :: ValueSet -> [Value]
fromList :: [Value] -> ValueSet
cartesianProduct :: CartProductType -> [ValueSet] -> ValueSet
compareValueSets :: ValueSet -> ValueSet -> Maybe Ordering
member :: Value -> ValueSet -> Bool
isFinitePrintable :: ValueSet -> Bool
unDotProduct :: ValueSet -> Maybe [ValueSet]
