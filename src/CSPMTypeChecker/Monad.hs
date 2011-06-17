module CSPMTypeChecker.Monad 
	(module Control.Monad.Error,
	TypeCheckError(..), TypeCheckMonad, readTypeRef, writeTypeRef, freshTypeVar,
	freshTypeVarWithConstraints, getType, safeGetType, setType,
	local, getEnvironment, compress, compressTypeScheme, errorIfFalseM, errorIfFalse,
	
	runTypeChecker
	)
	where

import Control.Monad.Error
import Control.Monad.State

import List (nub, (\\), intersect, group, sort)
import Text.PrettyPrint.HughesPJ

import Data.Graph
import Data.IORef
import Prelude

import CSPMDataStructures.Syntax
import CSPMDataStructures.Types
import CSPMPrettyPrinter

import Util.Annotated
import Util.PartialFunctions
import Util.Monad

-- *************************************************************************
-- Type Checker Monad
-- *************************************************************************
data TypeCheckError = 
	ErrorWithExp PExp TypeCheckError
	| ErrorWithPat PPat TypeCheckError
	| ErrorWithMatch PMatch TypeCheckError
	| ErrorWithDataTypeClause PDataTypeClause TypeCheckError
	| ErrorWithDecl PDecl TypeCheckError
	| ErrorWithModule PModule TypeCheckError
	| UnificationError (Name, Type) (Name, Type)
	| UnknownUnificationError Type Type
	| InfiniteUnificationError TypeVar Type
	| DuplicatedDefinitions [Name]	-- ns is not the duplicates - we calc these
	| IncorrectNumberOfArguments PExp Int
	| InvalidSetPattern [PPat]
	| UnknownError String
	
	| VariableNotInScope Name

instance Error TypeCheckError where
	strMsg = UnknownError

indentEveryLine = unlines . map (\l -> '\t':l) . lines 

-- TODO: improve error messages
instance Show TypeCheckError where
	show (ErrorWithPat (An srcloc _ pat) err) =
		show err++
		"in pattern:\n"++
		indentEveryLine (show (prettyPrint pat))
	show (ErrorWithDecl (An srcloc _ (FunBind (Name n) ms)) err) =
		show err++
		"in the declaration of "++n++".\n"
	show (ErrorWithDecl (An srcloc _ (PatBind p e)) err) =
		show err++
		"in the declaration of "++show (prettyPrint p)++".\n"
	show (ErrorWithMatch (An srcloc _ (Match ps _)) err) = 
		show err++ -- TODO
		"in the match "++show (map (map prettyPrint) ps)++".\n"
	show (ErrorWithDataTypeClause (An srcloc _ m) err) = show err -- TODO
	show (ErrorWithDecl (An srcloc _ d) err) =
		show err++
		"in the declaration of:"++
		show (prettyPrint d)
	show (ErrorWithExp (An srcloc _ exp) err) =
		show err++
		"in the expression at "++show srcloc++":\n"++
		indentEveryLine (show (prettyPrint exp))
	show (ErrorWithModule (An srcloc _ exp) err) = show err
	show (InfiniteUnificationError tv typ) =
		"Cannot construct the infinite type: "++
		show tv++" = "++show (prettyPrintType [] typ)++" "
	show (UnknownUnificationError t1 t2) =
		"Could not match the types:\n"++
		"\t"++show (prettyPrintType [] t1)++
		"\nand\n"++
		"\t"++show (prettyPrintType [] t2)++"\n"
	show (DuplicatedDefinitions ns) =
		"The variables: "++
			show (hsep (punctuate comma (map (\ (Name n) -> text n) dupedVars)))++
		" have multiple definitions."
		where
			dupedVars = (map head . filter (\ l -> length l > 1) . group . sort) ns
	show (IncorrectNumberOfArguments exp correct) =
		"An incorrect number (correct number: "++show correct++
		") of arguments was supplied to :\n"++
		indentEveryLine (show (prettyPrint exp))		
	show (InvalidSetPattern ps) =
		"You may only pattern match on set patterns of length 0 or 1."
	show (VariableNotInScope (Name n)) =
		"Name "++n++" is not in scope\n"
	show (UnknownError s) = 
		"An unknown error occured: "++s
		
type Environment = [PartialFunction Name TypeScheme]

data TypeInferenceState = TypeInferenceState {
		-- map from names to arbitrary types
		environment :: Environment,
		-- Next TypeVar to be allocated
		nextTypeId :: Int
	}

type TypeCheckMonad = 
	ErrorT TypeCheckError (StateT TypeInferenceState Tyger)

runTypeChecker :: TypeCheckMonad a -> Tyger a
runTypeChecker prog =
	do
		(errOrVal,state)<- 
			runStateT (runErrorT prog) (TypeInferenceState [[]] 0)
		case errOrVal of
			Left err	 -> throwError $ CSPMTypeCheckError (show err)
			Right val	 -> return val

getEnvironment :: TypeCheckMonad Environment 
getEnvironment = gets environment

errorIfFalse :: Bool -> TypeCheckError -> TypeCheckMonad ()
errorIfFalse True e = return ()
errorIfFalse False e = throwError e

errorIfFalseM :: TypeCheckMonad Bool -> TypeCheckError -> TypeCheckMonad ()
errorIfFalseM m e = 
	do
		res <- m
		errorIfFalse res e

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


safeGetType_ :: [PartialFunction Name TypeScheme] -> Name -> Maybe TypeScheme
safeGetType_ [] n = Nothing
safeGetType_ (pf:pfs) n = 
	case safeApply pf n of
		Just t	-> Just t
		Nothing -> safeGetType_ pfs n

getType :: Name -> TypeCheckMonad TypeScheme
getType name =
	do
		envs <- gets environment
		case safeGetType_ envs name of
			Just t	-> return t
			Nothing -> throwError $ VariableNotInScope name

safeGetType :: Name -> TypeCheckMonad (Maybe TypeScheme)
safeGetType n =
	do
		envs <- gets environment
		return $ safeGetType_ envs n

-- Sets the type of n to be t in the current scope only. No unification is 
-- performed.
setType :: Name -> TypeScheme -> TypeCheckMonad ()
setType n t =
	do
		res <- safeGetType n
		(env:envs) <- gets environment
		let env' = updatePF env n t
		modify (\ s -> s { environment = env':envs })

local :: [Name] -> TypeCheckMonad a -> TypeCheckMonad a
local ns m = 
	do
		env <- gets environment
		newArgs <- replicateM (length ns) freshTypeVar
		modify (\s -> s { environment = (zip ns (map (ForAll []) newArgs)):env })
		
		res <- m
		
		env <- gets environment
		modify (\ s -> s { environment = tail env })
		
		return res

compressTypeScheme :: TypeScheme -> TypeCheckMonad TypeScheme
compressTypeScheme (ForAll ts t) = 
	do
		t' <- compress t
		return $ ForAll ts t'
compress :: Type -> TypeCheckMonad Type
compress (tr @ (TVar typeRef)) = 
	do
		res <- readTypeRef typeRef
		case res of
			Left tv -> return tr
			Right t	-> compress t
compress (TFunction targs tr) = 
	do
		targs' <- mapM compress targs
		tr' <- compress tr
		return $ TFunction targs' tr'
compress (TSeq t) = 
	do
		t' <- compress t
		return $ TSeq t'
compress (TSet t) =
	do
		t' <- compress t
		return $ TSet t'
compress (TTuple ts) =
	do
		ts' <- mapM compress ts
		return $ TTuple ts'
compress (TDotable t1 t2)=
	do
		t1' <- compress t1
		t2' <- compress t2
		return $ TDotable t1' t2'
compress (TDatatype n) = return $ TDatatype n
compress (TDot t1 t2) = 
	do
		t1' <- compress t1
		t2' <- compress t2
		return $ TDot t1' t2'
compress t = return t
