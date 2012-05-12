module CSPM.Evaluator.Monad where

import Control.Monad.Reader
import Prelude hiding (lookup)

import CSPM.Compiler.Processes
import CSPM.DataStructures.Names
import CSPM.Evaluator.Environment
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Exception

data EvaluationState ops = 
    EvaluationState {
        environment :: Environment ops,
        parentProcName :: Maybe (ProcName ops)
    }
  
type EvaluationMonad ops = Reader (EvaluationState ops)

gets :: (EvaluationState ops -> a) -> EvaluationMonad ops a
gets = asks

modify :: (EvaluationState ops -> EvaluationState ops) -> EvaluationMonad ops a -> EvaluationMonad ops a
modify = local

runEvaluator :: EvaluationState ops -> EvaluationMonad ops a -> a
runEvaluator st prog = runReader prog st

getState :: EvaluationMonad ops (EvaluationState ops)
getState = gets id
{-# INLINE getState #-}

getEnvironment :: EvaluationMonad ops (Environment ops)
getEnvironment = gets environment
{-# INLINE getEnvironment #-}

lookupVarMaybeThunk :: Name -> EvaluationMonad ops (Value ops)
lookupVarMaybeThunk n = do
    -- This should never produce an error as the TC would
    -- catch it
    env <- getEnvironment
    return $ lookup env n
{-# INLINE lookupVarMaybeThunk #-}

-- | Implements non-recursive lets.
addScopeAndBind :: [(Name, Value ops)] -> EvaluationMonad ops a -> EvaluationMonad ops a
addScopeAndBind [] prog = prog
addScopeAndBind bs prog =
    modify (\ st -> st { environment = newLayerAndBind (environment st) bs }) prog

-- | Implements recursive lets.
addScopeAndBindM :: [(Name, EvaluationMonad ops (Value ops))] -> EvaluationMonad ops a -> EvaluationMonad ops a
addScopeAndBindM [] prog = prog
addScopeAndBindM binds prog = do
    st <- getState
    let
        env' = newLayerAndBind (environment st) bs
        st' = st { environment = env' }
        bs = [(n, runEvaluator st' v) | (n, v) <- binds]
    modify (\_ -> st') prog

throwError :: ErrorMessage -> a
throwError err = throwSourceError [err]

getParentProcName :: EvaluationMonad ops (Maybe (ProcName ops))
getParentProcName = gets parentProcName

updateParentProcName :: ProcName ops -> EvaluationMonad ops a -> EvaluationMonad ops a
updateParentProcName pn prog =
    modify (\ st -> st { parentProcName = Just pn }) prog
