module CSPM.TypeChecker.Monad (
	readTypeRef, writeTypeRef, freshTypeVar, freshTypeVarWithConstraints,
	getType, safeGetType, setType,
	compress, compressTypeScheme, 
	
	TypeCheckMonad, runTypeChecker, 
	newTypeInferenceState, 	TypeInferenceState(..), getState,
	
	local, getEnvironment,
	ErrorContext, addErrorContext, getErrorContexts,
	getSrcSpan, setSrcSpan,
	getUnificationStack, addUnificationPair,
	getInError, setInError,
	
	raiseMessageAsError, raiseMessagesAsError, panic,
	manyErrorsIfFalse, errorIfFalseM, errorIfFalse, tryAndRecover, failM,
)
where

import Control.Monad.State
import Prelude hiding (lookup)

import CSPM.DataStructures.Names
import CSPM.DataStructures.Types
import CSPM.TypeChecker.Environment
import CSPM.TypeChecker.Exceptions
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

-- *************************************************************************
-- Type Checker Monad
-- *************************************************************************
type ErrorContext = Doc

data TypeInferenceState = TypeInferenceState {
		-- | map from names to arbitrary types
		environment :: Environment,
		-- | Next TypeVar to be allocated
		nextTypeId :: Int,
		-- | Location of the current AST element - used for error
		-- pretty printing
		srcSpan :: SrcSpan,
		-- | Error stack - provides context information for any
		-- errors that might be raised
		errorContexts :: [ErrorContext],
		-- | Errors that have occured
		errors :: [ErrorMessage],
		-- | Stack of attempted unifications - the current one
		-- is at the front. In the form (expected, actual).
		unificationStack :: [(Type, Type)],
		-- | Are we currently in an error state
		inError :: Bool
	}
	
newTypeInferenceState :: TypeInferenceState
newTypeInferenceState = TypeInferenceState {
		environment = new,
		nextTypeId = 0,
		srcSpan = Unknown,
		errorContexts = [],
		errors = [],
		unificationStack = [],
		inError = False
	}

type TypeCheckMonad = StateT TypeInferenceState IO

-- | Runs the typechecker, starting from state 'st'. If any errors are
-- encountered then a 'SourceError' will be thrown with the relevent
-- error messages.
runTypeChecker :: TypeInferenceState -> TypeCheckMonad a -> IO a
runTypeChecker st prog = do
	(a, st) <- runStateT prog st
	let errs = errors st
	case errs of
		[] -> return a
		_ -> throwSourceError errs

getState :: TypeCheckMonad TypeInferenceState
getState = gets id

getEnvironment :: TypeCheckMonad Environment
getEnvironment = gets environment

setEnvironment :: Environment -> TypeCheckMonad ()
setEnvironment env = modify (\ st -> st { environment = env })

local :: [Name] -> TypeCheckMonad a -> TypeCheckMonad a
local ns m = 
	do
		env <- getEnvironment
		newArgs <- replicateM (length ns) freshTypeVar
		setEnvironment (newLayerAndBind env (zip ns (map (ForAll []) newArgs)))
		
		res <- m
		
		env <- getEnvironment
		setEnvironment (popLayer env)

		return res

getErrorContexts :: TypeCheckMonad [ErrorContext]
getErrorContexts = gets errorContexts

addErrorContext :: ErrorContext -> TypeCheckMonad a -> TypeCheckMonad a
addErrorContext c p = do
	ctxts <- getErrorContexts
	modify (\st -> st { errorContexts = c:ctxts })
	a <- p
	modify (\st -> st { errorContexts = ctxts })
	return a
	
getErrors :: TypeCheckMonad ErrorMessages
getErrors = gets errors

addErrors :: [ErrorMessage] -> TypeCheckMonad ()
addErrors es = modify (\ st -> st { errors = es++(errors st) })

getInError :: TypeCheckMonad Bool
getInError = gets inError

setInError :: Bool -> TypeCheckMonad a -> TypeCheckMonad a
setInError b prog = do
	o <- getInError
	modify (\st -> st { inError = b })
	a <- prog
	modify (\st -> st { inError = o })
	return a

getSrcSpan :: TypeCheckMonad SrcSpan
getSrcSpan = gets srcSpan

-- | Sets the SrcSpan only within prog.
setSrcSpan :: SrcSpan -> TypeCheckMonad a -> TypeCheckMonad a
setSrcSpan loc prog = do
	oLoc <- getSrcSpan
	modify (\st -> st { srcSpan = loc })
	a <- prog
	modify (\st -> st { srcSpan = oLoc })
	return a

getUnificationStack :: TypeCheckMonad [(Type, Type)]
getUnificationStack = gets unificationStack

addUnificationPair :: (Type, Type) -> TypeCheckMonad a -> TypeCheckMonad a
addUnificationPair tp p = do
	stk <- getUnificationStack
	modify (\st -> st { unificationStack = tp:stk })
	a <- p
	modify (\ st -> st { unificationStack = stk })
	return a

-- Error handling

-- | Report the error if first parameter is False.
errorIfFalse :: Bool -> Error -> TypeCheckMonad ()
errorIfFalse b e = manyErrorsIfFalse b [e]

manyErrorsIfFalse :: Bool -> [Error] -> TypeCheckMonad ()
manyErrorsIfFalse True es = return ()
manyErrorsIfFalse False es = raiseMessagesAsError es

errorIfFalseM :: TypeCheckMonad Bool -> Error -> TypeCheckMonad ()
errorIfFalseM m e = m >>= \res -> errorIfFalse res e

failM :: TypeCheckMonad a
failM = do
	errs <- getErrors
	throwSourceError errs

raiseMessageAsError :: Error -> TypeCheckMonad a
raiseMessageAsError msg = raiseMessagesAsError [msg]

-- | Report a message as an error
raiseMessagesAsError :: [Error] -> TypeCheckMonad a
raiseMessagesAsError msgs = do
	src <- getSrcSpan
	ctxts <- getErrorContexts
	let ctxtDocs = ctxts
	let contextMsg = trimAndRenderContexts maxContexts ctxtDocs
	addErrors [mkErrorMessage src (msg $$ contextMsg) | msg <- msgs]
	failM
	where
		-- Don't print too much context
		trimAndRenderContexts 0 _ = empty
		trimAndRenderContexts n [] = empty
		trimAndRenderContexts n (doc:docs) = 
			doc $$ trimAndRenderContexts (n-1) docs
		maxContexts = 3

tryAndRecover :: 
	TypeCheckMonad a -> TypeCheckMonad a -> TypeCheckMonad a
tryAndRecover prog handler = tryM prog >>= \x ->
	case x of
		Right a -> return a
		Left ex -> case ex of
			SourceError msgs -> do
				-- The errors will be discarded as the errors propogate up.
				-- Hence, we reset them.
				setErrors msgs
				handler
			UserError -> handler
			-- An exception type we don't want to interfer with
			e			-> throwException e
	where
		setErrors :: ErrorMessages -> TypeCheckMonad ()
		setErrors es = modify (\ st -> st { errors = es })	


-- *************************************************************************
-- Type Operations
-- *************************************************************************
readTypeRef :: TypeVarRef -> TypeCheckMonad (Either (TypeVar, [Constraint]) Type)
readTypeRef (TypeVarRef tv cs ioref) = 
	do
		mtyp <- readPType ioref
		case mtyp of
			Just t	-> return (Right t)
			Nothing -> return (Left (tv, cs))

writeTypeRef :: TypeVarRef -> Type -> TypeCheckMonad ()
writeTypeRef (TypeVarRef tv cs ioref) t = setPType ioref t

freshTypeVar :: TypeCheckMonad Type
freshTypeVar = freshTypeVarWithConstraints []

freshTypeVarWithConstraints :: [Constraint] -> TypeCheckMonad Type
freshTypeVarWithConstraints cs =
	do
		nextId <- gets nextTypeId
		modify (\s -> s { nextTypeId = nextId+1 })
		ioRef <- freshPType
		return $ TVar (TypeVarRef (TypeVar nextId) cs ioRef)

-- | Get the type of 'n' and through an exception if it doesn't exist.
getType :: Name -> TypeCheckMonad TypeScheme
getType n = do
	env <- gets environment
	-- Force evaluation of lookup n
	-- If we don't do this the error is deferred until later
	case maybeLookup env n of
		Just ts -> return ts
		Nothing -> raiseMessagesAsError [varNotInScopeMessage n]

-- | Get the type of 'n' if it exists, othewise return Nothing.
safeGetType :: Name -> TypeCheckMonad (Maybe TypeScheme)
safeGetType n = do
	env <- gets environment
	return $ maybeLookup env n
	
-- | Sets the type of n to be t in the current scope only. No unification is 
-- performed.
setType :: Name -> TypeScheme -> TypeCheckMonad ()
setType n t = getEnvironment >>= (\ env -> setEnvironment (update env n t))

-- | Apply compress to the type of a type scheme.
compressTypeScheme :: TypeScheme -> TypeCheckMonad TypeScheme
compressTypeScheme (ForAll ts t) = 
	do
		t' <- compress t
		return $ ForAll ts t'

-- | Takes a type and compresses the type by reading all type variables and
-- if they point to another type, it returns that type instead.
compress :: Type -> TypeCheckMonad Type
compress (tr @ (TVar typeRef)) = do
	res <- readTypeRef typeRef
	case res of
		Left tv -> return tr
		Right t	-> compress t
compress (TFunction targs tr) = do
	targs' <- mapM compress targs
	tr' <- compress tr
	return $ TFunction targs' tr'
compress (TSeq t) = compress t >>= return . TSeq
compress (TSet t) = compress t >>= return . TSet
compress (TTuple ts) = mapM compress ts >>= return . TTuple
compress (TDotable t1 t2)= do
	t1' <- compress t1
	t2' <- compress t2
	return $ TDotable t1' t2'
compress (TDatatype n) = return $ TDatatype n
compress (TDot t1 t2) = do
	t1' <- compress t1
	t2' <- compress t2
	return $ TDot t1' t2'
compress t = return t
