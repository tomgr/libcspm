{-# LANGUAGE FlexibleContexts #-}
module CSPM.Evaluator.AnalyserMonad (
    AnalyserState, initialAnalyserState,
    AnalyserMonad, runAnalyser, getState,
    withinTimedSection, maybeTimed,
    shouldRecordStackTraces,
    freeVarsBoundByParentFrames,

    DataTypeInformation(..), DataTypeConstructor(..), FieldSet(..),
    dataTypeForName, addDataTypes, channelInformationForName,

    FrameInformation(..), createFunctionFrame, createVariableFrame,
    createBuiltinFunctionFrame, createVariableFrame', createLambdaFrame,
    createPartiallyAppliedFunctionFrame, frameParent, frameFreeVars,
    analyseRelevantVars,

    shouldTrackVariables,
    selectVariablesToTrack,
) where

import Control.Monad.ST
import Control.Monad.State
import Data.List (sort)
import Data.Hashable
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Supply
import System.IO.Unsafe

import CSPM.Syntax.AST hiding (timedSectionTockName)
import CSPM.Syntax.DataTypeAnalyser
import CSPM.Syntax.FreeVars
import CSPM.Syntax.Names
import CSPM.Syntax.Types
import CSPM.Syntax.Visitor
import CSPM.Prelude
import qualified Data.Graph.ST as G
import Util.Annotated
import Util.Exception
import Util.List
import Util.Monad

data AnalyserState = AnalyserState {
        timedSectionTockName :: Maybe Name,
        timedSectionFunctionName :: Maybe Name,
        registeredDataTypes :: M.Map Name DataTypeInformation,
        parentFrame :: Maybe FrameInformation,
        -- | For each non-top-level bound name, the set of variables that were
        -- bound at some point and can affect its value. This is not just free
        -- variables. For example, consider:
        -- 
        -- f(x) =
        --      let
        --          p = x()
        --          q = p()
        --      within ...
        --
        -- Then the relevant vars for q include x, because q depends on p, which
        -- depends on x.
        relevantVarMap :: M.Map Name [Name],
        recordStackTraces :: Bool,
        -- | Should variable values be tracked for process evaluation.
        trackVariables :: Bool,
        -- | A function that decides, if trackVariables is true, which variables
        -- should be tracked. The first argument is the name of the enclosing
        -- definition, if any. The second argument is the list of all the
        -- relevant free vars, and their corresponding types
        variablesToTrackFunction :: Maybe (Maybe Name -> [(Name, Type)] -> [Name])
    }

initialAnalyserState ::
    Bool
    -> Bool
    -> Maybe (Maybe Name -> [(Name, Type)] -> [Name])
    -> IO AnalyserState
initialAnalyserState recordStackTraces trackVariables trackerFn = do
    return $ AnalyserState {
        timedSectionFunctionName = Nothing,
        timedSectionTockName = Nothing,
        registeredDataTypes = M.empty,
        parentFrame = Nothing,
        relevantVarMap = M.empty,
        recordStackTraces = recordStackTraces,
        trackVariables = trackVariables,
        variablesToTrackFunction = trackerFn
    }

shouldRecordStackTraces :: AnalyserMonad Bool
shouldRecordStackTraces = gets recordStackTraces

shouldTrackVariables :: AnalyserMonad Bool
shouldTrackVariables = gets trackVariables

selectVariablesToTrack :: FreeVars body => body -> AnalyserMonad [Name]
selectVariablesToTrack body = do
    fvs <- freeVarsBoundByParentFrames body
    Just fn <- gets variablesToTrackFunction
    currentParentFrame <- gets parentFrame
    let enclosingName (Just (FunctionFrame { functionFrameFunctionName = n })) =
            Just n
        enclosingName (Just f) = enclosingName (frameParent f)
        enclosingName Nothing = Nothing

        typMap = case currentParentFrame of
                    Nothing -> M.empty
                    Just f -> typeMapForFrame f
        nvMap = [(fv, fromJust $ M.lookup fv typMap) | fv <- fvs]
    return $ fn (enclosingName currentParentFrame) nvMap

relevantVars :: Name -> AnalyserMonad [Name]
relevantVars n = do
    m <- gets relevantVarMap
    return $!
        case M.lookup n m of
            Just fvs -> fvs
            Nothing -> []

-- | The set of all variables that are relevant to the given body, and were
-- bound by a parent frame.
freeVarsBoundByParentFrames :: FreeVars body => body -> AnalyserMonad [Name]
freeVarsBoundByParentFrames body = do
    mframe <- gets parentFrame
    relevantFreeVars mframe ([] :: [TCPat]) body

analyseRelevantVars :: [TCDecl] -> AnalyserMonad a -> AnalyserMonad a
analyseRelevantVars ds prog = do
    oldMap <- gets relevantVarMap
    currentParentFrame <- gets parentFrame
    let boundByDecls = S.fromList $! boundNames ds
        isBoundByDecl n = S.member n boundByDecls
        -- The set of all variables that were bound by some function binding
        -- above (or similar).
        boundVars = S.fromList $!
                        case currentParentFrame of
                            Just frame -> varsBoundByFrame frame
                            Nothing -> []
        makeRelevant (x, y) = (x, sortedNub $ sort $ filter (flip S.member boundVars) y)
        freeVarMap = [(n, n') | d <- ds, n <- boundNames d, n' <- freeVars d,
                                n /= n']
        -- We now need to take the transitive closure of freeVarMap
        tcMap = runST $ do
                    let allVars = sortedNub $ sort $ map fst freeVarMap ++ map snd freeVarMap
                    g <- G.newGraph allVars freeVarMap
                    transitiveDeps <- G.transitiveClosure g
                    return $! M.fromList $! map makeRelevant $!
                        filter (isBoundByDecl . fst) transitiveDeps
        newMap = M.union oldMap tcMap
    modify (\ st -> st { relevantVarMap = newMap })
    a <- prog
    modify (\ st -> st { relevantVarMap = oldMap })
    return a

type AnalyserMonad = StateT AnalyserState IO

runAnalyser :: AnalyserState -> AnalyserMonad a -> IO (a, AnalyserState)
runAnalyser st prog = runStateT prog st

getState :: AnalyserMonad AnalyserState
getState = gets id

withinTimedSection :: Name -> Name -> AnalyserMonad a -> AnalyserMonad a
withinTimedSection tn fn prog = do
    oldTn <- gets timedSectionTockName
    oldFn <- gets timedSectionFunctionName
    modify (\ st -> st {
            timedSectionTockName = Just tn,
            timedSectionFunctionName = Just fn
        })
    a <- prog
    modify (\ st -> st {
            timedSectionTockName = oldTn,
            timedSectionFunctionName = oldFn
        })
    return a

maybeTimed ::
    AnalyserMonad a ->
    (Name -> Name -> AnalyserMonad a) ->
    AnalyserMonad a
maybeTimed nonTimedProg timedProg = do
    mtn <- gets timedSectionTockName
    mfn <- gets timedSectionFunctionName
    case (mtn, mfn) of
        (Just tn, Just fn) -> timedProg tn fn
        _ -> nonTimedProg

-- | A process name template'
data FrameInformation =
        FunctionFrame {
            -- | The name of the process.
            functionFrameFunctionName :: Name,
            -- | The list of patterns this function frame bound.
            functionFramePatterns :: [[TCPat]],
            -- | The list of arguments this process name template has.
            functionFrameBoundNames :: [Name],
            functionFrameBoundNameTypes :: M.Map Name Type,
            -- | The list of free variables the process depends on. This should
            -- not include names bound in the top scope of the program.
            functionFrameFreeVars :: [Name],
            -- | A unique identifier assigned to this process name.
            frameId :: !Int,
            functionFrameParent :: Maybe FrameInformation
        }
        | BuiltinFunctionFrame {
            -- | The name of the process.
            builtinFunctionFrameFunctionName :: Name,
            -- | A unique identifier assigned to this process name.
            frameId :: !Int
        }
        | LambdaFrame {
            -- | The list of patterns this function frame bound.
            lambdaFramePatterns :: [TCPat],
            -- | The list of arguments this process name template has.
            lambdaFrameBoundNames :: [Name],
            lambdaFrameBoundNameTypes :: M.Map Name Type,
            -- | The list of free variables the process depends on. This should
            -- not include names bound in the top scope of the program.
            lambdaFrameFreeVars :: [Name],
            lambdaFrameExpression :: TCExp,
            -- | A unique identifier assigned to this process name.
            frameId :: !Int,
            lambdaFrameParent :: Maybe FrameInformation
        }
        -- | Can never appear as a parent
        | PartiallyAppliedFunctionFrame {
            partiallyAppliedFunctionFrameFunctionName :: Name,
            partiallyAppliedFunctionFrameFreeVars :: [Name],
            frameId :: !Int,
            partiallyAppliedFunctionFrameParent :: Maybe FrameInformation
        }
        -- | Corresponds to an annonymous binding, like in a statement, or
        -- prefix expression.
        | VariableFrame {
            variableFrameBoundNames :: [Name],
            variableFrameBoundNameTypes :: M.Map Name Type,
            variableFramePatterns :: [TCPat],
            frameId :: !Int,
            variableFrameParent :: Maybe FrameInformation
        }

instance Eq FrameInformation where
    pnt1 == pnt2 = frameId pnt1 == frameId pnt2
instance Hashable FrameInformation where
    hashWithSalt s f = hashWithSalt s (frameId f)
instance Ord FrameInformation where
    compare pnt1 pnt2 = compare (frameId pnt1) (frameId pnt2)

frameParent :: FrameInformation -> Maybe FrameInformation
frameParent (FunctionFrame { functionFrameParent = p }) = p
frameParent (LambdaFrame { lambdaFrameParent = p }) = p
frameParent (PartiallyAppliedFunctionFrame {
        partiallyAppliedFunctionFrameParent = p
    }) = p
frameParent (VariableFrame { variableFrameParent = p }) = p
frameParent (BuiltinFunctionFrame {}) = Nothing

frameFreeVars :: FrameInformation -> [Name]
frameFreeVars (FunctionFrame { functionFrameFreeVars = fvs }) = fvs
frameFreeVars (LambdaFrame { lambdaFrameFreeVars = fvs }) = fvs
frameFreeVars (PartiallyAppliedFunctionFrame {
        partiallyAppliedFunctionFrameFreeVars = fvs
    }) = fvs
frameFreeVars _ = []

frameUniqueSupply :: IORef (Supply Int)
frameUniqueSupply = unsafePerformIO $ do
    s <- newNumSupply
    newIORef s
{-# NOINLINE frameUniqueSupply #-}

takeFrameUnique :: MonadIO m => m Int
takeFrameUnique = do
    s <- liftIO $ atomicModifyIORef frameUniqueSupply split2
    return $ supplyValue s

createBuiltinFunctionFrame :: Name -> AnalyserMonad FrameInformation
createBuiltinFunctionFrame n = do
    nid <- takeFrameUnique
    return $! BuiltinFunctionFrame {
            builtinFunctionFrameFunctionName = n,
            frameId = nid
        }

varsBoundByFrame :: FrameInformation -> [Name]
varsBoundByFrame finfo = frameBoundNames finfo ++
    case frameParent finfo of
        Nothing -> []
        Just finfo -> varsBoundByFrame finfo
    where
        frameBoundNames :: FrameInformation -> [Name]
        frameBoundNames (FunctionFrame { functionFrameBoundNames = ns }) = ns
        frameBoundNames (LambdaFrame { lambdaFrameBoundNames = ns }) = ns
        frameBoundNames (VariableFrame { variableFrameBoundNames = ns }) = ns
        frameBoundNames _ = []

typeMapForFrame :: FrameInformation -> M.Map Name Type
typeMapForFrame finfo =
    case frameParent finfo of
        Nothing -> typeMap finfo
        Just parent -> M.union (typeMap finfo) (typeMapForFrame parent)
    where
        typeMap (FunctionFrame { functionFrameBoundNameTypes = ns }) = ns
        typeMap (LambdaFrame { lambdaFrameBoundNameTypes = ns }) = ns
        typeMap (VariableFrame { variableFrameBoundNameTypes = ns }) = ns

relevantFreeVars :: (BoundNames args, FreeVars body) =>
    Maybe FrameInformation -> args -> body -> AnalyserMonad [Name]
relevantFreeVars mframe args body =
    let relevant = S.fromList $! boundNames args ++
                        case mframe of
                            Just frame -> varsBoundByFrame frame
                            Nothing -> []
        fvs = freeVars body
    in if S.null relevant then return []
        else do
            transitivelyFree <- concatMapM relevantVars fvs
            --liftIO $ putStrLn $ show $ sortedNub $ sort $
            --    filter (\ n -> S.member n relevant) (fvs++transitivelyFree)
            return $! sortedNub $ sort $
                filter (\ n -> S.member n relevant) (fvs++transitivelyFree)

createFunctionFrame :: (FreeVars body) =>
    Name -> [[TCPat]] -> body ->
    (FrameInformation -> AnalyserMonad a) ->
    AnalyserMonad a
createFunctionFrame n args body prog = do
    nid <- takeFrameUnique
    parent <- gets parentFrame
    relevant <- relevantFreeVars parent args body
    let (boundNames, boundNameTypes) = findBoundVariables args
        frame = FunctionFrame {
                functionFrameFunctionName = n,
                functionFramePatterns = args,
                functionFrameBoundNames = boundNames,
                functionFrameBoundNameTypes = boundNameTypes,
                functionFrameFreeVars = relevant,
                frameId = nid,
                functionFrameParent = parent
            }
    modify (\ st -> st { parentFrame = Just frame })
    a <- prog frame
    modify (\ st -> st { parentFrame = parent })
    return a

createPartiallyAppliedFunctionFrame :: (FreeVars body) =>
    Name -> body -> AnalyserMonad FrameInformation
createPartiallyAppliedFunctionFrame n body = do
    nid <- takeFrameUnique
    parent <- gets parentFrame
    relevant <- relevantFreeVars parent ([] :: [TCPat]) body
    return $! PartiallyAppliedFunctionFrame {
                partiallyAppliedFunctionFrameFunctionName = n,
                partiallyAppliedFunctionFrameFreeVars = relevant,
                frameId = nid,
                partiallyAppliedFunctionFrameParent = parent
            }

createVariableFrame' ::
    TCPat -> (AnalyserMonad a) -> AnalyserMonad a
createVariableFrame' arg prog = createVariableFrame [arg] (\ _ -> prog)

createVariableFrame :: 
    [TCPat] -> (FrameInformation -> AnalyserMonad a) -> AnalyserMonad a
createVariableFrame args prog = do
    nid <- takeFrameUnique
    parent <- gets parentFrame
    let (boundNames, boundNameTypes) = findBoundVariables args
        frame = VariableFrame {
                variableFrameBoundNames = boundNames,
                variableFrameBoundNameTypes = boundNameTypes,
                variableFramePatterns = args,
                frameId = nid,
                variableFrameParent = parent
            }
    modify (\ st -> st { parentFrame = Just frame })
    a <- prog frame
    modify (\ st -> st { parentFrame = parent })
    return a

createLambdaFrame :: [TCPat] -> TCExp -> 
    (FrameInformation -> AnalyserMonad a) -> AnalyserMonad a
createLambdaFrame args body prog = do
    nid <- takeFrameUnique
    parent <- gets parentFrame
    relevant <- relevantFreeVars parent ([] :: [TCPat]) body
    let (boundNames, boundNameTypes) = findBoundVariables args
        frame = LambdaFrame {
                lambdaFramePatterns = args,
                lambdaFrameBoundNames = boundNames,
                lambdaFrameBoundNameTypes = boundNameTypes,
                lambdaFrameFreeVars = relevant,
                lambdaFrameExpression = body,
                frameId = nid,
                lambdaFrameParent = parent
            }
    modify (\ st -> st { parentFrame = Just frame })
    a <- prog frame
    modify (\ st -> st { parentFrame = parent })
    return a

channelInformationForName :: Name -> AnalyserMonad DataTypeConstructor
channelInformationForName name = do
    dc <- dataTypeForName (builtInName "Events")
    case M.lookup name (dataTypeConstructors dc) of
        Just c -> return c
        Nothing -> panic $ "Could not find registered channel "++show name

dataTypeForName :: Name -> AnalyserMonad DataTypeInformation
dataTypeForName name = do
    dataTypes <- gets registeredDataTypes
    case M.lookup name dataTypes of
        Just typ -> return typ
        Nothing -> panic $ "Could not find registerd datatype "++show name

addDataTypes :: [DataTypeInformation] -> AnalyserMonad ()
addDataTypes newDataTypes = do
    oldDataTypes <- gets registeredDataTypes
    modify (\ st -> st { registeredDataTypes =
            foldr (\ c -> M.insert (dataTypeName c) c) oldDataTypes newDataTypes
        })

findBoundVariables :: Visitable a (State [(Name, Type)]) => a ->
    ([Name], M.Map Name Type)
findBoundVariables a =
    let
        visitPat (An loc (typ, _) (PVar n)) _ = modify (\ st -> (n, typ) : st)
        visitPat _ prog = prog

        visitor = defaultVisitor {
                visitPat = visitPat
            }

        nts = execState (visit visitor a) []
    in (map fst nts, M.fromList nts)
