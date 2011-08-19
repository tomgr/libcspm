module CSPM.TypeChecker (
	typeCheckExp, typeCheckModules, typeCheckInteractiveStmt,
	typeOfExp, dependenciesOfExp,
	
	initTypeChecker,
	TypeCheckMonad, TypeInferenceState,
	runTypeChecker, runFromStateToState,
) where

import Control.Monad.Trans

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.TypeChecker.BuiltInFunctions
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Compressor
import CSPM.TypeChecker.Dependencies
import CSPM.TypeChecker.Environment
import CSPM.TypeChecker.Expr
import CSPM.TypeChecker.InteractiveStmt
import CSPM.TypeChecker.Module
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Unification
import Util.Annotated

runFromStateToState :: TypeInferenceState -> TypeCheckMonad a -> 
			IO (a, TypeInferenceState)
runFromStateToState st prog = runTypeChecker st $ do
	r <- prog
	s <- getState
	return (r, s)

initTypeChecker :: IO TypeInferenceState
initTypeChecker = runTypeChecker newTypeInferenceState $ do
	injectBuiltInFunctions
	-- Add a blank level in the environment to allow built in functions
	-- to be overriden.
	local [] getState

typeCheckExp :: PExp -> TypeCheckMonad TCExp
typeCheckExp exp = typeCheck exp >> mcompress exp

typeCheckModules :: [PModule] -> TypeCheckMonad [TCModule]
typeCheckModules  ms = typeCheck ms >> mcompress ms

typeCheckInteractiveStmt :: 
	PInteractiveStmt -> TypeCheckMonad TCInteractiveStmt
typeCheckInteractiveStmt stmt = typeCheck stmt >> mcompress stmt

typeOfExp :: PExp -> TypeCheckMonad Type
typeOfExp exp = do
	-- See if has been type checked, if so, return type,
	-- else type check
	mt <- liftIO $ readPType (snd (annotation exp))
	case mt of 
		Just t -> evaluateDots t >>= compress
		Nothing -> typeCheckExp exp >> typeOfExp exp

-- | Returns the list of names that this expression depends on
dependenciesOfExp :: TCExp -> TypeCheckMonad [Name]
dependenciesOfExp exp = dependencies exp
