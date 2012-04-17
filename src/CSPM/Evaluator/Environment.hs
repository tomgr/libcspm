module CSPM.Evaluator.Environment (
    Environment,
    new, lookup, newLayerAndBind, toList,
) where

import qualified Data.IntMap as M
import Prelude hiding (lookup)

import CSPM.DataStructures.Names
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Exception

type Environment = [M.IntMap Value]

new :: Environment
new = []

lookup :: Environment -> Name -> Value
lookup env n = 
    let
        nv = nameUnique n
        lookupInLayers [] = panic ("lookup not found: "++show n)
        lookupInLayers (m:ms) = 
            case M.lookup nv m of
                Just v -> v
                Nothing -> lookupInLayers ms
    in lookupInLayers env

toList :: Environment -> [(Name, Value)]
toList env = panic "not imp"

newLayerAndBind :: Environment -> [(Name, Value)] -> Environment
newLayerAndBind ms nvs =
    M.fromList [(nameUnique n, v) | (n,v) <- nvs] : ms
