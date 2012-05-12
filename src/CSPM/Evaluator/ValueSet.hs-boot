{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module CSPM.Evaluator.ValueSet where

import Data.Hashable

import {-# SOURCE #-} CSPM.Compiler.Processes
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.PrettyPrint

data ValueSet ops

instance Eq (ValueSet ops)
instance Hashable (ValueSet ops)
instance Ord (ValueSet ops)
instance PrettyPrintable (UProc ops) => PrettyPrintable (ValueSet ops)

toList :: ValueSet ops -> [Value ops]
fromList :: [Value ops] -> ValueSet ops
cartesianProduct :: ([Value ops] -> Value ops) -> [ValueSet ops] -> ValueSet ops
compareValueSets :: ValueSet ops -> ValueSet ops -> Maybe Ordering
