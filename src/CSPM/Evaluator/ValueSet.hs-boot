module CSPM.Evaluator.ValueSet where

import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.PrettyPrint

data ValueSet

instance Eq ValueSet
instance Ord ValueSet
instance PrettyPrintable ValueSet

toList :: ValueSet -> [Value]
compareValueSets :: ValueSet -> ValueSet -> Maybe Ordering
