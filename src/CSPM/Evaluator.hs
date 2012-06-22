module CSPM.Evaluator (
    evaluateExp, evaluateDecl, evaluateFile,
    addToEnvironment,
    
    initEvaluator, runFromStateToState,
    EvaluationMonad, runEvaluator, EvaluationState,
    module CSPM.Evaluator.Values,
    module CSPM.Evaluator.ValueSet,
) where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Environment
import CSPM.Evaluator.Expr
import CSPM.Evaluator.Module
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet

runFromStateToState :: EvaluationState -> EvaluationMonad a -> 
    (a, EvaluationState)
runFromStateToState st prog = runEvaluator st $ do
    r <- prog
    s <- getState
    return (r, s)

-- | The environment to use initially. This uses the IO monad as 
-- the EvaluationMonad cannot be used without a valid environment.
initEvaluator :: EvaluationState
initEvaluator = runEvaluator (EvaluationState new Nothing) $
    injectBuiltInFunctions getState

evaluateExp :: TCExp -> EvaluationMonad Value
evaluateExp e = eval e

-- | Evaluates the declaration but doesn't add it to the current environment.
evaluateDecl :: TCDecl -> EvaluationMonad [(Name, EvaluationMonad Value)]
evaluateDecl d = bindDecls [d]

-- | Evaluates the declaration but doesn't add it to the current environment.
evaluateFile :: [TCModule] -> EvaluationMonad [(Name, EvaluationMonad Value)]
evaluateFile ms = bindModules ms

addToEnvironment :: [(Name, EvaluationMonad Value)] -> EvaluationMonad EvaluationState
addToEnvironment bs = addScopeAndBindM bs getState
