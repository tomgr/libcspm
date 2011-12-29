module CSPM.Evaluator.Environment (
    Environment,
    new, lookup, newLayerAndBind, toList,
) where

import qualified Data.Map as M
import Prelude hiding (lookup)

import CSPM.DataStructures.Names
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Exception

type Environment = M.Map Name Value

new :: Environment
new = M.empty

toList :: Environment -> [(Name, Value)]
toList = M.toList

lookup :: Environment -> Name -> Value
lookup env n = M.findWithDefault (panic "lookup not found") n env

newLayerAndBind :: Environment -> [(Name, Value)] -> Environment
newLayerAndBind env nvs = 
    foldr (\ (n,v) env -> M.insert n v env) env nvs
