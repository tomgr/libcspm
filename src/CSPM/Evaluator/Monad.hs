module CSPM.Evaluator.Monad where

--import Control.Monad.State
import Prelude hiding (lookup)

import CSPM.DataStructures.Names
import CSPM.Evaluator.Environment
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Exception

data EvaluationState = 
	EvaluationState {
		environment :: Environment
--		channelMap :: Map Name [ValueSet],
--		datatypeClauseMap :: Map Name [ValueSet],
	}
	
type EvaluationMonad = LazyEvalMonad EvaluationState

newtype LazyEvalMonad s a = LazyEvalMonad { 
	-- Notice that this doesn't yield a new environment
	unLazyEvalMonad :: s -> a
}

test :: (Int, Bool, Int)
test = runLazyEvalMonad (EvaluationState new) $ do
		v <- inf 0
		let x = head v
		let y = error "A"
		let z = True
		Just vs1 <- return $ Just [0,1,2]
		Just vs2 <- return $ error "blah"
		return (x, z || y, head (vs1++vs2))
	where
		inf :: Int -> EvaluationMonad [Int]
		inf x = inf x >>= \xs -> return $ x:xs
		
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
--	modify (\ st -> st { environment = env })

lookupVar :: Name -> EvaluationMonad Value
lookupVar n = do
	-- This should never produce an error as the TC would
	-- catch it
	env <- getEnvironment
	return $ lookup env n
	
addScopeAndBind :: [(Name, Value)] -> EvaluationMonad a -> EvaluationMonad a
addScopeAndBind bs = addScopeAndBindM $ return bs

addScopeAndBindM :: EvaluationMonad [(Name, Value)] -> EvaluationMonad a -> EvaluationMonad a
addScopeAndBindM binds = 
	modify (\st -> let
			bs = runEvaluator st' binds
			env' = newLayerAndBind (environment st) bs
			st' = st { environment = env' }
		in
			st')

{-
addToScope :: [(Name, Value)] -> EvaluationMonad ()
addToScope binds = do
	env <- getEnvironment
	setEnvironment (newLayerAndBind env binds)
	return ()
-}



