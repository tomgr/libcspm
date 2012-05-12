{-# LANGUAGE FlexibleContexts #-}
module CSPM.Evaluator (
    evaluateExp, evaluateDecl, evaluateFile,
    addToEnvironment,
    
    initEvaluator, runFromStateToState,
    EvaluationMonad, runEvaluator, EvaluationState,
    Evaluatable,
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
import Util.PrettyPrint

runFromStateToState :: EvaluationState ops -> EvaluationMonad ops a -> 
    (a, EvaluationState ops)
runFromStateToState st prog = runEvaluator st $ do
    r <- prog
    s <- getState
    return (r, s)

-- | The environment to use initially. This uses the IO monad as 
-- the EvaluationMonad cannot be used without a valid environment.
initEvaluator :: BuiltInFunctions ops => EvaluationState ops
initEvaluator = runEvaluator (EvaluationState new Nothing) $
    injectBuiltInFunctions getState

evaluateExp :: (Evaluatable (p Name) ops, PrettyPrintable (UProc ops)) =>
    TCExp p -> EvaluationMonad ops (Value ops)
evaluateExp e = eval e

-- | Evaluates the declaration but doesn't add it to the current environment.
evaluateDecl :: (Evaluatable (p Name) ops, PrettyPrintable (UProc ops)) => 
    TCDecl p -> EvaluationMonad ops [(Name, EvaluationMonad ops (Value ops))]
evaluateDecl d = bindDecls [d]

-- | Evaluates the declaration but doesn't add it to the current environment.
evaluateFile :: (Evaluatable (p Name) ops, PrettyPrintable (UProc ops)) => 
    [TCModule p] -> EvaluationMonad ops [(Name, EvaluationMonad ops (Value ops))]
evaluateFile ms = bindModules ms

addToEnvironment :: (PrettyPrintable (UProc ops)) => 
    [(Name, EvaluationMonad ops (Value ops))] -> EvaluationMonad ops (EvaluationState ops)
addToEnvironment bs = addScopeAndBindM bs getState
