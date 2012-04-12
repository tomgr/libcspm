module CSPM.Evaluator (
    evaluateExp, evaluateDecl, evaluateFile,
    getBoundNames, addToEnvironment,
    
    initEvaluator, runFromStateToState,
    EvaluationMonad, runEvaluator, EvaluationState,
) where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Environment
import CSPM.Evaluator.Expr
import CSPM.Evaluator.Module
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values

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

getBoundNames :: EvaluationMonad [Name]
getBoundNames = 
    getEnvironment >>= return . filter (\n -> nameType n == ExternalName) . map fst . toList

addToEnvironment :: [(Name, EvaluationMonad Value)] -> EvaluationMonad EvaluationState
addToEnvironment bs = addScopeAndBindM bs getState
