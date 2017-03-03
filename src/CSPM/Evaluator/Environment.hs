module CSPM.Evaluator.Environment (
    Environment(..), StackFrame(..), StackTrace,
    new, maybeLookup, lookup, newLayerAndBind, addFrame, getFrames, withFrames,
) where

import qualified Data.IntMap as M
import Prelude hiding (lookup)

import CSPM.Syntax.Names
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Annotated
import Util.Exception

data StackFrame = StackFrame InstantiatedFrame SrcSpan
type StackTrace = [StackFrame]

data Environment = Environment [M.IntMap Value] StackTrace

new :: Environment
new = Environment [] []

maybeLookup :: Environment -> Name -> Maybe Value
maybeLookup (Environment env _) n =
    let
        nv = nameUnique n
        lookupInLayers [] = Nothing
        lookupInLayers (m:ms) = 
            case M.lookup nv m of
                Just v -> Just v
                Nothing -> lookupInLayers ms
    in lookupInLayers env

lookup :: Environment -> Name -> Value
lookup (Environment env _) n =
    let
        nv = nameUnique n
        lookupInLayers [] = panic ("lookup not found: "++show n)
        lookupInLayers (m:ms) = 
            case M.lookup nv m of
                Just v -> v
                Nothing -> lookupInLayers ms
    in lookupInLayers env

newLayerAndBind :: Environment -> [(Name, Value)] -> Environment
newLayerAndBind (Environment ms fs) nvs =
    let ms' = M.fromList [(nameUnique n, v) | (n,v) <- nvs] : ms
    in Environment ms' fs

addFrame :: Environment -> StackFrame -> Environment
addFrame (Environment ms fs) f = Environment ms (f:fs)

getFrames :: Environment -> StackTrace
getFrames (Environment _ fs) = fs

withFrames :: Environment -> StackTrace -> Environment
withFrames (Environment ms _) fs = Environment ms fs