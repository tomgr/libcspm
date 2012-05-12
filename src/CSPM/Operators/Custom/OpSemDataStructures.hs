module OpSemDataStructures where

import Data.IORef
import Util
import Text.PrettyPrint.HughesPJ

-- *************************************************************************
-- Rule input data types
-- *************************************************************************

newtype Name = Name String deriving (Eq)

instance Show Name where
	show (Name n) = show n

data Event = 
	Event Name
	| ChanEvent Name [Exp]		-- The channel must carry events only
	| Tau						-- and thus its type must be UserEvents.UserEvents...
	deriving (Eq, Show)

data Exp = 
	OperatorApp Name [Exp]
	| InductiveCase
	| Tuple [Exp]
	| Var Name

	| SigmaPrime	-- SigmaPrime = SystemEvents
	| Sigma			-- i.e. UserEvents
	| ProcArgs
	| SetComprehension [Exp] [SideCondition]
	| Set [Exp]
	| SetMinus Exp Exp
	| Union Exp Exp
	| Intersection Exp Exp
	| Powerset Exp
	
	| ReplicatedUnion Exp
	deriving (Eq, Show)
	
data ProcessRelation =
	Performs Exp Event Exp
	deriving Show

data Pattern = 
	PVar Name
	| PTuple [Pattern]
	| PSet [Pattern]
	deriving (Eq, Show)

-- A side condition should always be equal to SCGenerator p e1 e2
-- TODO: prefix side condition is true, need to sort this out in
-- the typechecker
data SideCondition =
	SCGenerator Pattern Exp
	| Formula PropositionalFormula
	deriving (Eq, Show)
	
data PropositionalFormula =
	Member Pattern Exp
	| Equals Exp Exp
	| Subset Exp Exp
	| Not PropositionalFormula
	| And PropositionalFormula PropositionalFormula
	| Or PropositionalFormula PropositionalFormula
	| PFalse
	| PTrue
	deriving (Eq, Show)
	
-- TODO: precondition can only be of the form P -> Q
-- i.e. no operators may be applied
data InductiveRule =
	InductiveRule [ProcessRelation] ProcessRelation [SideCondition]
	deriving Show

-- Always omit tau promotion rules
data InputOperator = 
	InputOperator {
		iopFriendlyName :: Name,
		iopArgs :: [(Name, ProcessSubtype)],
		iopRules :: [InductiveRule],
		iopReplicatedOperator :: Maybe InputReplicatedOperator,
		iopParsingInformation :: Maybe OperatorSyntax
	}
	deriving Show

data InputReplicatedOperator =
	InputReplicatedOperator {
		irepOpArgs :: [Name],
		irepOpBaseCase :: ([Pattern], Exp),
		-- List of vars for this case, list of vars for recursive case
		-- lengths should all be equal to the args length
		irepOpInductiveCase :: ([Name], Exp),
		irepOpParsingInformation :: Maybe OperatorSyntax
	}
	deriving Show
	
data InputOpSemDefinition = 
	InputOpSemDefinition {
		inputOperators :: [InputOperator],
		inputChannels :: [Channel]
	}
	deriving Show
	
data Assoc = 
	AssocLeft | AssocRight | AssocNone
	deriving Show

data OperatorSyntax =
	InfixOp [ParseComponent] Integer Assoc	-- Int refers to precedence
	| PrefixOp [ParseComponent] Integer	
	| PostfixOp [ParseComponent] Integer
	deriving Show

data ParseComponent =
	String String
	| Argument Integer
	deriving Show

-- *************************************************************************
-- Intermediate Rule Data Types (post type inference)
-- *************************************************************************
newtype TypeVar = TypeVar Int deriving (Eq, Show)

-- We use typerefs so that when we return a type we don't have to worry about
-- passing substitutions around
data TypeVarRef = 
	TypeVarRef TypeVar (IORef (Maybe Type))
	deriving Eq

instance Show TypeVarRef where
	show (TypeVarRef tv _) = "TypeVarRef "++show tv

data ProcessSubtype =
	InfinitelyRecursive
	| FinitelyRecursive
	| NotRecursive
	
	| Unknown
	deriving (Eq, Show)

data Type =
	TVar TypeVarRef
	| TEvent
	| TProcArg
	| TOnProcess ProcessSubtype
	| TOffProcess ProcessSubtype
	| TSet Type			-- Only sets of events are supported currently
	| TChannel [Type]
	| TTuple [Type]
	| TOperator [Type]
	deriving (Eq)

instance Show Type where
	show (TVar (TypeVarRef tv _)) = show tv
	show TEvent = "Event"
	show TProcArg = "ProcArg"
	show (TOnProcess st) = "OnProc "++show st
	show (TOffProcess st) = "OffProcess "++show st
	show (TSet t) = "{"++show t++"}"
	show (TChannel ts) = "Channel "++show ts
	show (TTuple ts) = 
		show (parens (hsep (punctuate comma (map (text . show) ts))))
	show (TOperator ts) = "Operator "++show ts

typeIsProc :: Type -> Bool
typeIsProc (TOnProcess _) = True
typeIsProc (TOffProcess _) = True
typeIsProc _ = False

-- Always omit tau promotion rules
data Operator = 
	Operator {
		opFriendlyName :: Name,
		opArgs :: [(Name, Type)],
		opRules :: [InductiveRule],
		opParsingInformation :: Maybe OperatorSyntax
	}
	| ReplicatedOperator {
		opFriendlyName :: Name,
		opArgs :: [(Name, Type)],
		repOpBaseCase :: ([Pattern], Exp),
		-- List of vars for this case, list of vars for recursive case
		-- lengths should all be equal to the args length
		repOpInductiveCase :: ([Name], Exp),
		opParsingInformation :: Maybe OperatorSyntax
	}
	deriving Show

data Channel = Channel Name [Exp]
	deriving Show

data OpSemDefinition = OpSemDefinition {
		operators :: [Operator],
		channels :: [Channel]
	}
	deriving Show

-- *************************************************************************
-- Rule Output Data Types (post compilation)
-- *************************************************************************
type ProcId = Int

data CompiledOp =
	CompiledOp {
		copFriendlyName :: String,
		copArgs :: [(Name, Type)],
		copRules :: [CompiledRule],
		copDiscards :: [ProcId]
	}
	deriving Show

data CompiledRule =
	CompiledRule (PartialFunction ProcId Event) Event (String, [Exp])
		(PartialFunction ProcId ProcId) (PartialFunction ProcId ProcId) 
		(PartialFunction ProcId ProcId) [ProcId] [Name] [Stmt]
	deriving Show

data Stmt = 
	Generator Pattern Exp
	| PropFormula PropositionalFormula
	deriving (Show)
