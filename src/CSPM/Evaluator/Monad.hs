module CSPM.Evaluator.Monad where

--import Control.Monad.State
import Prelude hiding (lookup)

import CSPM.DataStructures.Names
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Environment
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Exception

data EvaluationState = 
    EvaluationState {
        environment :: Environment
    }
    
type EvaluationMonad = LazyEvalMonad EvaluationState

newtype LazyEvalMonad s a = LazyEvalMonad { 
    -- Notice that this doesn't yield a new environment
    unLazyEvalMonad :: s -> a
}
        
runLazyEvalMonad :: s -> LazyEvalMonad s a -> a
runLazyEvalMonad st (LazyEvalMonad prog) = prog st

gets :: (s -> a) -> LazyEvalMonad s a
gets f = LazyEvalMonad (\st -> f st)

modify :: (s -> s) -> LazyEvalMonad s a -> LazyEvalMonad s a
modify f prog = LazyEvalMonad (\st -> unLazyEvalMonad prog (f st))

instance Monad (LazyEvalMonad a) where
    (LazyEvalMonad p1) >>= f = 
        LazyEvalMonad (\ st -> 
            let
                a = p1 st
                LazyEvalMonad p2 = f a
            in p2 st)
    return k = LazyEvalMonad (\ st -> k)

runEvaluator :: EvaluationState -> EvaluationMonad a -> a
runEvaluator st prog = runLazyEvalMonad st prog

getState :: EvaluationMonad EvaluationState
getState = gets id

getEnvironment :: EvaluationMonad Environment
getEnvironment = gets environment

--setEnvironment :: Environment -> EvaluationMonad ()
--setEnvironment env =
--  modify (\ st -> st { environment = env })

lookupVar :: Name -> EvaluationMonad Value
lookupVar n = do
    -- This should never produce an error as the TC would
    -- catch it
    env <- getEnvironment
    return $ lookup env n

-- | Implements non-recursive lets.
addScopeAndBind :: [(Name, Value)] -> EvaluationMonad a -> EvaluationMonad a
addScopeAndBind bs = addScopeAndBindM [(n, return v) | (n, v) <- bs]

-- | Implements recursive lets.
addScopeAndBindM :: [(Name, EvaluationMonad Value)] -> EvaluationMonad a -> EvaluationMonad a
addScopeAndBindM binds prog = do
    st <- getState
    let
        env' = newLayerAndBind (environment st) bs
        st' = st { environment = env' }
        bs = [(n, runEvaluator st' v) | v <- binds]
    modify (\_ -> st') prog

throwError :: ErrorMessage -> a
throwError err = throwSourceError [err]

-- the problem here is that in order to get st', we need env', which requires
-- bs. However, bs requires st', which is not a problem intself if something
-- is lazy, but if binds does a lookup in the environment (which it does)
-- then this will break it?

