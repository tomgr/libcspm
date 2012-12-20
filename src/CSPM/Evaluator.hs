module CSPM.Evaluator (
    evaluateExp, evaluateDecl, evaluateFile,
    addToEnvironment, maybeProcessNameToProcess,
    
    initEvaluator, runFromStateToState,
    EvaluationMonad, runEvaluator, EvaluationState,
    module CSPM.Evaluator.ProcessValues,
    module CSPM.Evaluator.Values,
    module CSPM.Evaluator.ValueSet,
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
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import Util.Annotated

runFromStateToState :: EvaluationState -> EvaluationMonad a -> 
    (a, EvaluationState)
runFromStateToState st prog = runEvaluator st $ do
    r <- prog
    s <- getState
    return (r, s)

-- | The environment to use initially. This uses the IO monad as 
-- the EvaluationMonad cannot be used without a valid environment.
initEvaluator :: EvaluationState
initEvaluator = runEvaluator (EvaluationState new Nothing Unknown) $
    injectBuiltInFunctions getState

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
    v <- func args
    return $ Just $ PProcCall pn (let VProc p = v in p)
maybeProcessNameToProcess _ = return Nothing
