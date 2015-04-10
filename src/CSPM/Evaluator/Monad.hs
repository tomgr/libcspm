{-# LANGUAGE CPP #-}
module CSPM.Evaluator.Monad where

import Control.Monad.Reader
import Prelude hiding (lookup)

import CSPM.DataStructures.Names
import CSPM.Evaluator.Environment
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Exception

type EvaluationState = Environment
type EvaluationMonad = Reader EvaluationState

gets :: (EvaluationState -> a) -> EvaluationMonad a
gets = asks

modify :: (EvaluationState -> EvaluationState) -> EvaluationMonad a -> EvaluationMonad a
modify = local

runEvaluator :: EvaluationState -> EvaluationMonad a -> a
runEvaluator st prog = runReader prog st

getState :: EvaluationMonad EvaluationState
getState = gets id
{-# INLINE getState #-}

getEnvironment :: EvaluationMonad Environment
getEnvironment = gets id
{-# INLINE getEnvironment #-}

lookupVarMaybeThunk :: Name -> EvaluationMonad Value
lookupVarMaybeThunk n = do
    -- This should never produce an error as the TC would
    -- catch it
    env <- getEnvironment
    return $ lookup env n
{-# INLINE lookupVarMaybeThunk #-}

-- | Implements non-recursive lets.
addScopeAndBind :: [(Name, Value)] -> EvaluationMonad a -> EvaluationMonad a
#ifndef CSPM_PROFILING
addScopeAndBind [] prog = prog
#endif
addScopeAndBind bs prog =
    modify (\ st -> newLayerAndBind st bs) prog

-- | Implements recursive lets.
addScopeAndBindM :: [(Name, EvaluationMonad Value)] -> EvaluationMonad a -> EvaluationMonad a
#ifndef CSPM_PROFILING
addScopeAndBindM [] prog = prog
#endif
addScopeAndBindM binds prog = do
    st <- getState
    let
        env' = newLayerAndBind st bs
        st' = env'
        bs = [(n, runEvaluator st' v) | (n, v) <- binds]
    modify (\_ -> st') prog

throwError :: ErrorMessage -> a
throwError err = throwSourceError [err]

-- TOOD: sort out stack trace
throwError' :: (Maybe InstantiatedFrame -> ErrorMessage) -> EvaluationMonad a
throwError' f = throwError (f Nothing)
