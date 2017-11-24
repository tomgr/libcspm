{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    TypeSynonymInstances, UndecidableInstances #-}
module CSPM.Evaluator (
    evaluateExp, evaluateDecl, evaluateFile,
    maybeProcessNameToProcess,
    
    initEvaluator, runFromStateToState,
    EvaluationMonad, runEvaluator, EvaluationState,
    EvaluatorOptions(..), defaultEvaluatorOptions,
    module CSPM.Evaluator.Values,
    module CSPM.Evaluator.ValueSet,

    dumpProfilingData,
) where

import Control.Monad.State
import qualified Data.Foldable as F

import CSPM.Syntax.Names
import CSPM.Syntax.AST
import qualified CSPM.Evaluator.AnalyserMonad as A
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Environment
import CSPM.Evaluator.Expr
import CSPM.Evaluator.File
import qualified CSPM.Evaluator.Monad as E
#ifdef CSPM_PROFILING
import qualified CSPM.Evaluator.Profiler as P
#else
import Util.Exception
#endif
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import CSPM.Prelude
import CSPM.Syntax.Types
import Util.Annotated
import qualified Util.MonadicPrettyPrint as M

import qualified Data.Map as Mp
import qualified Data.Set as S

data EvaluatorOptions = EvaluatorOptions {
        recordStackTraces :: Bool,
        trackVariables :: Bool,
        variablesToTrackFunction :: Maybe (Maybe Name -> [(Name, Type)] -> [Name])
    }

defaultEvaluatorOptions :: EvaluatorOptions
defaultEvaluatorOptions = EvaluatorOptions {
        recordStackTraces = False,
        trackVariables = False,
        variablesToTrackFunction = Nothing
    }

data EvaluationState = EvaluationState {
        analyserState :: A.AnalyserState,
        evaluatorState :: E.EvaluationState
    }

type EvaluationMonad = StateT EvaluationState IO

runAnalyser :: A.AnalyserMonad a -> EvaluationMonad a
runAnalyser prog = do
    anSt <- gets analyserState
    (a, anSt') <- liftIO $ A.runAnalyser anSt prog
    modify (\st -> st { analyserState = anSt' })
    return a

runEvaluator :: E.EvaluationMonad a -> EvaluationMonad a
runEvaluator prog = do
    evSt <- gets evaluatorState
    return $! E.runEvaluator evSt prog

runFromStateToState :: EvaluationState -> EvaluationMonad a -> 
    IO (a, EvaluationState)
runFromStateToState st anProg = runStateT anProg st

-- | The environment to use initially. 
initEvaluator :: EvaluatorOptions -> IO EvaluationState
initEvaluator evOptions = do
    analyserState <- A.initialAnalyserState
        (recordStackTraces evOptions)
        (trackVariables evOptions)
        (variablesToTrackFunction evOptions)
    let initialEvState = new
        initialState = EvaluationState {
                analyserState = analyserState,
                evaluatorState = initialEvState
            }

    (_, st) <- runFromStateToState initialState $! do
        injector <- runAnalyser (injectBuiltInFunctions E.getState)
        evSt' <- runEvaluator injector
        modify (\ st -> st { evaluatorState = evSt' })
    return st

evaluateExp :: TCExp -> EvaluationMonad Value
evaluateExp e = do
    e <- runAnalyser (eval e)
    runEvaluator e

-- | Evaluates the declaration and adds it to the current environment.
evaluateDecl :: TCDecl -> EvaluationMonad ()
evaluateDecl d = do
    ms <- runAnalyser (bindDecls [d])
    addToEnvironment ms

-- | Evaluates the declaration and adds it to the current environment.
evaluateFile :: TCCSPMFile -> EvaluationMonad ()
evaluateFile ms = do
    ms <- runAnalyser (bindFile ms)
    addToEnvironment ms

addToEnvironment :: E.EvaluationMonad [(Name, E.EvaluationMonad Value)] ->
    EvaluationMonad ()
addToEnvironment bs = do
    evSt <- gets evaluatorState
    let evSt' = E.runEvaluator evSt $ do
            nds <- bs
            E.addScopeAndBindM nds E.getState
    modify (\st -> st { evaluatorState = evSt' })

checkArgument :: Value -> Bool
checkArgument (VInt i) = True
checkArgument (VChar c) = True
checkArgument (VBool b) = True
checkArgument (VTuple vs) = F.and $ fmap checkArgument vs
checkArgument (VList vs) = F.and $ fmap checkArgument vs
checkArgument (VSet s) = F.and $ fmap checkArgument (toList s)
checkArgument (VDot vs) = F.and $ fmap checkArgument vs
checkArgument (VChannel n) = True
checkArgument (VDataType n) = True
checkArgument (VFunction id _) = False
checkArgument (VProc p) = False

combineMaybe :: [Maybe a] -> Maybe [a]
combineMaybe [] = Just []
combineMaybe (Nothing : _) = Nothing
combineMaybe (Just x : xs) =
    case combineMaybe xs of
        Nothing -> Nothing
        Just xs -> Just (x:xs)

-- | Attempts to convert a process name to a process, if possible.
maybeProcessNameToProcess :: ProcName -> EvaluationMonad (Maybe Proc)
maybeProcessNameToProcess pn@(ProcName
        (InstantiatedFrame _ frame@(A.BuiltinFunctionFrame {}) _ [args])) =
    runEvaluator $ do
        if S.member (A.builtinFunctionFrameFunctionName frame) locatedBuiltins then
            return Nothing
        else do
        maybeFunc <- maybeLookupVar (A.builtinFunctionFrameFunctionName frame)
        case maybeFunc of
            Nothing -> return Nothing
            Just (VFunction _ func) ->
                if and (map checkArgument args) then do
                    v <- func args
                    return $ Just $ PProcCall pn (let VProc p = v in p)
                else return $ Nothing
maybeProcessNameToProcess pn@(ProcName
        (InstantiatedFrame _ frame@(A.FunctionFrame {
                A.functionFramePatterns = [pats] 
            }) fvs [])) = runEvaluator $ do

    maybeFunc <- maybeLookupVar (A.functionFrameFunctionName frame)
    case maybeFunc of
        Nothing -> return Nothing
        Just (VFunction _ func) -> do
            let m = Mp.fromList $ zip (A.functionFrameFreeVars frame) fvs
                reassembleArgument (An _ _ (PVar n)) | isNameDataConstructor n =
                    Nothing
                reassembleArgument (An _ _ (PVar n)) = Mp.lookup n m
            case combineMaybe $ map reassembleArgument pats of
                Just args -> 
                    if and (map checkArgument args) then do
                        v <- func args
                        return $ Just $ PProcCall pn (let VProc p = v in p)
                    else return Nothing
                Nothing -> return Nothing
maybeProcessNameToProcess _ = return Nothing

dumpProfilingData :: IO ()

#ifdef CSPM_PROFILING
dumpProfilingData = P.dumpProfilingData
#else
dumpProfilingData = panic "Profiling not supported in this build."
#endif

instance  M.MonadicPrettyPrintable E.EvaluationMonad a => 
        M.MonadicPrettyPrintable EvaluationMonad a where
    prettyPrint = runEvaluator . M.prettyPrint
