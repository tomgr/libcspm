module CSPM.Evaluator.Environment (
    Environment,
    new, lookup, newLayerAndBind,
) where

import Data.List (nub, sort)
import qualified Data.IntMap as M
import Prelude hiding (lookup)

import CSPM.DataStructures.Names
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Exception

newtype Environment ops = Environment [M.IntMap (Value ops)]

new :: Environment ops
new = Environment []

lookup :: Environment ops -> Name -> Value ops
lookup (Environment env) n = 
    let
        nv = nameUnique n
        lookupInLayers [] = panic ("lookup not found: "++show n)
        lookupInLayers (m:ms) = 
            case M.lookup nv m of
                Just v -> v
                Nothing -> lookupInLayers ms
    in lookupInLayers env

newLayerAndBind :: Environment ops -> [(Name, Value ops)] -> Environment ops
newLayerAndBind (Environment ms) nvs =
    let ms' = M.fromList [(nameUnique n, v) | (n,v) <- nvs] : ms
    in Environment ms'
