module CSPM.Evaluator.Environment (
    Environment,
    new, lookup, newLayerAndBind,
    trimEnvironment,
) where

import qualified Data.IntMap as M
import Prelude hiding (lookup)

import CSPM.DataStructures.Names
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Exception

data Environment = Environment [M.IntMap Value]

new :: Environment
new = Environment []

lookup :: Environment -> Name -> Value
lookup (Environment env) n = 
    let
        nv = nameUnique n
        lookupInLayers [] = panic ("lookup not found: "++show n)
        lookupInLayers (m:ms) = 
            case M.lookup nv m of
                Just v -> v
                Nothing -> lookupInLayers ms
    in lookupInLayers env

newLayerAndBind :: Environment -> [(Name, Value)] -> Environment
newLayerAndBind (Environment ms) nvs =
    let ms' = M.fromList [(nameUnique n, v) | (n,v) <- nvs] : ms
    in Environment ms'

trimEnvironment :: Environment -> [Name] -> Environment
trimEnvironment (Environment ms) ns =
    Environment [M.fromList [(nameUnique n, lookup (Environment ms) n) | n <- ns]]
