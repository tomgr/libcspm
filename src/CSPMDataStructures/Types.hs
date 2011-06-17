module CSPMDataStructures.Types where

import Control.Monad.Trans
import Data.IORef
import Text.PrettyPrint.HughesPJ
import CSPMDataStructures.Syntax
import Util.PartialFunctions

-- *************************************************************************
-- Types
-- *************************************************************************
newtype TypeVar = TypeVar Int deriving (Eq, Show)

data TypeScheme =
	ForAll [(TypeVar, [Constraint])] Type
	deriving (Eq, Show)
	
data Constraint =
	Eq | Ord
	deriving (Eq, Ord, Show)

-- During Type Checking we use TDotable a b only when a is something
-- atomic. Except, during unification we start doing TDotable (TDot...)
-- and may build up large TDotable a b types.
data Type =
	TVar TypeVarRef
	| TProc
	| TInt
	| TBool
	| TEvent
	-- Something that can be extended to an event
	| TEventable
	| TSet Type
	| TSeq Type
	| TDot Type Type
	| TTuple [Type]
	-- Arguments to result type
	| TFunction [Type] Type
	-- TDotable a b means that this type can be dotted
	-- with an a to yield something of type b
	| TDotable Type Type
	| TDatatype Name
	deriving (Eq, Show)

data TypeVarRef = 
	TypeVarRef TypeVar [Constraint] PType
	deriving Eq

instance Show TypeVarRef where
	show (TypeVarRef tv cs _) = "TypeVarRef "++show tv ++ show cs

type SymbolTable = PartialFunction Name TypeScheme
type PType = IORef (Maybe Type)
type PTypeScheme = IORef (Maybe TypeScheme)
type PSymbolTable = IORef SymbolTable

readPType :: (MonadIO m) => PType -> m (Maybe Type)
readPType ioref = 
	do
		t <- liftIO $ readIORef ioref
		return t

setPType :: (MonadIO m) => PType -> Type -> m ()
setPType ioref t = liftIO $ writeIORef ioref (Just t)

freshPType :: (MonadIO m) => m PType
freshPType = liftIO $ newIORef Nothing

readPSymbolTable :: (MonadIO m) => PSymbolTable -> m SymbolTable
readPSymbolTable ioref = 
	do
		t <- liftIO $ readIORef ioref
		return t

setPSymbolTable :: (MonadIO m) => PSymbolTable -> SymbolTable -> m ()
setPSymbolTable ioref t = liftIO $ writeIORef ioref t

freshPSymbolTable :: (MonadIO m) => m PSymbolTable
freshPSymbolTable = liftIO $ newIORef []

readPTypeScheme :: (MonadIO m) => PTypeScheme -> m (Maybe TypeScheme)
readPTypeScheme ioref = 
	do
		t <- liftIO $ readIORef ioref
		return t

setPTypeScheme :: (MonadIO m) => PTypeScheme -> TypeScheme -> m ()
setPTypeScheme ioref t = liftIO $ writeIORef ioref (Just t)

freshPTypeScheme :: (MonadIO m) => m PTypeScheme
freshPTypeScheme = liftIO $ newIORef Nothing

prettyPrintTypeScheme :: TypeScheme -> Doc
prettyPrintTypeScheme (ForAll ts t) =
	(if length ts > 0 then
		text "forall" <+> hsep (punctuate comma 
							[parens (hsep (punctuate comma (map ppConstraint cs)) <+>
								char (apply vmap n)) | (TypeVar n, cs) <- ts]) 
		<+> text ":"
	else
		empty)
		 <+> prettyPrintType vmap t
	where
		ppConstraint Eq = text "Eq"
		ppConstraint Ord = text "Ord"
		vmap = zip (map (\ (TypeVar n, _) -> n) ts) ['a'..'z']
		
prettyPrintType :: PartialFunction Int Char -> Type -> Doc
prettyPrintType vmap (TVar (TypeVarRef (TypeVar n) cs ioref)) = 
	case safeApply vmap n of
		Just c	-> char c
		Nothing	-> int n
prettyPrintType vmap (TFunction targs tr) = 
	parens (hsep (punctuate comma (map (prettyPrintType vmap) targs)))
	<+> text "->" <+> prettyPrintType vmap tr
prettyPrintType vmap (TSeq t) =
	char '<' <> prettyPrintType vmap t <> char '>'
prettyPrintType vmap (TSet t) =
	char '{' <> prettyPrintType vmap t <> char '}'
prettyPrintType vmap (TTuple ts) =
	parens (hsep (punctuate comma (map (prettyPrintType vmap) ts)))
prettyPrintType vmap (TDot t1 t2) =
	(case t1 of
		TDotable _ _ -> parens (prettyPrintType vmap t1)
		_				-> prettyPrintType vmap t1
	) <> text "." <> prettyPrintType vmap t2
prettyPrintType vmap (TDotable t1 t2) =
	prettyPrintType vmap t1 <> text "=>" <> prettyPrintType vmap t2
--	text "DotableWith" <+> prettyPrintType vmap t1 <+> text "yielding" 
--	<+> prettyPrintType vmap t2
prettyPrintType vmap (TDatatype (Name n)) = text n

prettyPrintType vmap (TBool) = text "Bool"
prettyPrintType vmap (TInt) = text "Int"
prettyPrintType vmap (TProc) = text "Proc"
prettyPrintType vmap (TEvent) = text "Event"
prettyPrintType vmap (TEventable) = text "Event or Channel"
