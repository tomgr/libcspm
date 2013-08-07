module CSPM.Evaluator (
    evaluateExp, evaluateDecl, evaluateFile,
    addToEnvironment, maybeProcessNameToProcess,
    
    initEvaluator, runFromStateToState,
    EvaluatorOptions(..), defaultEvaluatorOptions,
    ProfilerOptions(..), defaultProfilerOptions,
    EvaluationMonad, runEvaluator, EvaluationState,
    module CSPM.Evaluator.ProcessValues,
    module CSPM.Evaluator.Values,
    module CSPM.Evaluator.ValueSet,

    ProfilingData(..), profilingData,
) where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Environment
import CSPM.Evaluator.Expr
import CSPM.Evaluator.File
import CSPM.Evaluator.Monad
import CSPM.Evaluator.ProcessValues
import CSPM.Evaluator.Profiler
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import qualified Data.Foldable as F
import Util.Annotated

data EvaluatorOptions = EvaluatorOptions {
        runtimeRangeChecks :: Bool,
        profilerOptions :: ProfilerOptions
    }

defaultEvaluatorOptions :: EvaluatorOptions
defaultEvaluatorOptions = EvaluatorOptions {
        runtimeRangeChecks = True,
        profilerOptions = defaultProfilerOptions
    }

runFromStateToState :: EvaluationState -> EvaluationMonad a -> 
    (a, EvaluationState)
runFromStateToState st prog = runEvaluator st $ do
    r <- prog
    s <- getState
    return (r, s)

-- | The environment to use initially. This uses the IO monad as 
-- the EvaluationMonad cannot be used without a valid environment.
initEvaluator :: EvaluatorOptions -> IO EvaluationState
initEvaluator options = do
    profilerState <- initialProfilerState (profilerOptions options)
    let initialState = EvaluationState {
                environment = new,
                CSPM.Evaluator.Monad.parentScopeIdentifier = Nothing,
                currentExpressionLocation = Unknown,
                timedSection = Nothing,
                profilerState = profilerState,
                doRuntimeRangeChecks = runtimeRangeChecks options
            }
    return $! runEvaluator initialState (injectBuiltInFunctions getState)

evaluateExp :: TCExp -> EvaluationMonad Value
evaluateExp e = eval e

-- | Evaluates the declaration but doesn't add it to the current environment.
evaluateDecl :: TCDecl -> EvaluationMonad [(Name, EvaluationMonad Value)]
evaluateDecl d = bindDecls [d]

-- | Evaluates the declaration but doesn't add it to the current environment.
evaluateFile :: TCCSPMFile -> EvaluationMonad [(Name, EvaluationMonad Value)]
evaluateFile ms = bindFile ms

addToEnvironment :: [(Name, EvaluationMonad Value)] ->
    EvaluationMonad EvaluationState
addToEnvironment bs = addScopeAndBindM bs getState

-- | Attempts to convert a process name to a process, if possible.
maybeProcessNameToProcess :: ProcName -> EvaluationMonad (Maybe UProc)
maybeProcessNameToProcess (pn@(ProcName (SFunctionBind fn [args] Nothing))) = do
    -- Evaluate the function again
    VFunction _ func <- lookupVar fn
    let checkArgument (VInt i) = True
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
    if and (map checkArgument args) then do
        v <- func args
        return $ Just $ PProcCall pn (let VProc p = v in p)
    else return Nothing
maybeProcessNameToProcess _ = return Nothing

profilingData :: EvaluationMonad ProfilingData
profilingData = getProfilingData
