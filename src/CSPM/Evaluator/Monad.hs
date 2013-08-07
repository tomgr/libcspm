module CSPM.Evaluator.Monad where

import Control.Monad.Reader
import Prelude hiding (lookup)

import CSPM.DataStructures.Names
import CSPM.Evaluator.Environment
import CSPM.Evaluator.ProcessValues
import {-# SOURCE #-} CSPM.Evaluator.Profiler
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Annotated
import Util.Exception

data EvaluationState = 
    EvaluationState {
        environment :: Environment,
        parentScopeIdentifier :: Maybe ScopeIdentifier,
        currentExpressionLocation :: SrcSpan,
        timedSection :: Maybe (Event -> Int, Name),
        profilerState :: ProfilerState,
        doRuntimeRangeChecks :: Bool
    }
  
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
getEnvironment = gets environment
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
addScopeAndBind [] prog = prog
addScopeAndBind bs prog =
    modify (\ st -> st { environment = newLayerAndBind (environment st) bs }) prog

-- | Implements recursive lets.
addScopeAndBindM :: [(Name, EvaluationMonad Value)] -> EvaluationMonad a -> EvaluationMonad a
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

getParentScopeIdentifier :: EvaluationMonad (Maybe ScopeIdentifier)
getParentScopeIdentifier = gets parentScopeIdentifier

updateParentScopeIdentifier :: ScopeIdentifier -> EvaluationMonad a -> EvaluationMonad a
updateParentScopeIdentifier pn prog =
    modify (\ st -> st { parentScopeIdentifier = Just pn }) prog

setCurrentExpressionLocation :: SrcSpan -> EvaluationMonad a -> EvaluationMonad a
setCurrentExpressionLocation sp prog =
    modify (\ st -> st { currentExpressionLocation = sp }) prog

getCurrentExpressionLocation :: EvaluationMonad SrcSpan
getCurrentExpressionLocation = gets currentExpressionLocation

throwError' :: (SrcSpan -> Maybe ScopeIdentifier -> ErrorMessage) -> EvaluationMonad a
throwError' f = do
    loc <- gets currentExpressionLocation
    stk <- gets parentScopeIdentifier
    throwError (f loc stk)

setTimedCSP :: Name -> (Event -> Int) -> EvaluationMonad a -> EvaluationMonad a
setTimedCSP tock func prog =
    modify (\ st -> st { timedSection = Just (func, tock) }) prog

maybeTimedCSP ::
    EvaluationMonad a ->
    (Name -> (Event -> Int) -> EvaluationMonad a) ->
    EvaluationMonad a
maybeTimedCSP nonTimedProg timedProg = do
    mfunc <- gets timedSection
    case mfunc of
        Nothing -> nonTimedProg
        Just (f, tock) -> timedProg tock f
