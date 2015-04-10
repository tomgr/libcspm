{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    TypeSynonymInstances, UndecidableInstances #-}
module CSPM.Evaluator (
    evaluateExp, evaluateDecl, evaluateFile,
    maybeProcessNameToProcess,
    
    initEvaluator, runFromStateToState,
    EvaluationMonad, runEvaluator, EvaluationState,
    module CSPM.Evaluator.Values,
    module CSPM.Evaluator.ValueSet,

    #ifdef CSPM_PROFILING
    dumpProfilingData,
    #endif
) where

import Control.Monad.State

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import qualified CSPM.Evaluator.AnalyserMonad as A
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Environment
import CSPM.Evaluator.Expr
import CSPM.Evaluator.File
import qualified CSPM.Evaluator.Monad as E
#ifdef CSPM_PROFILING
import qualified CSPM.Evaluator.Profiler as P
#endif
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import qualified Util.MonadicPrettyPrint as M

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
initEvaluator :: IO EvaluationState
initEvaluator = do
    analyserState <- A.initialAnalyserState
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

-- | Attempts to convert a process name to a process, if possible.
maybeProcessNameToProcess :: ProcName -> EvaluationMonad (Maybe Proc)
-- TODO
--maybeProcessNameToProcess (pn@(ProcName (SFunctionBind _ fn [args] Nothing))) =
--    runEvaluator $ do
--        -- Evaluate the function again
--        VFunction _ func <- lookupVar fn
--        let checkArgument (VInt i) = True
--            checkArgument (VChar c) = True
--            checkArgument (VBool b) = True
--            checkArgument (VTuple vs) = F.and $ fmap checkArgument vs
--            checkArgument (VList vs) = F.and $ fmap checkArgument vs
--            checkArgument (VSet s) = F.and $ fmap checkArgument (toList s)
--            checkArgument (VDot vs) = F.and $ fmap checkArgument vs
--            checkArgument (VChannel n) = True
--            checkArgument (VDataType n) = True
--            checkArgument (VFunction id _) = False
--            checkArgument (VProc p) = False
--        if and (map checkArgument args) then do
--            v <- func args
--            return $ Just $ PProcCall pn (let VProc p = v in p)
--        else return Nothing
maybeProcessNameToProcess _ = return Nothing

#ifdef CSPM_PROFILING
dumpProfilingData :: IO ()
dumpProfilingData = P.dumpProfilingData
#endif

instance  M.MonadicPrettyPrintable E.EvaluationMonad a => 
        M.MonadicPrettyPrintable EvaluationMonad a where
    prettyPrint = runEvaluator . M.prettyPrint
