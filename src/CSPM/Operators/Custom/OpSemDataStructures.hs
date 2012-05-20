module CSPM.Operators.Custom.OpSemDataStructures where

import CSPM.DataStructures.Names
import Data.IORef
import Util.PartialFunctions
import Util.PrettyPrint

-- *************************************************************************
-- Rule input data types
-- *************************************************************************

data Event id = 
    Event id
    | ChanEvent id [Exp id]      -- The channel must carry events only
    | Tau                       -- and thus its type must be UserEvents.UserEvents...
    deriving (Eq, Show)

data Exp id =
    OperatorApp id [Exp id]
    | InductiveCase
    | Tuple [Exp id]
    | Var id

    | SigmaPrime    -- SigmaPrime = SystemEvents
    | Sigma         -- i.e. UserEvents
    | SetComprehension [Exp id] [SideCondition id]
    | Set [Exp id]
    | SetMinus (Exp id) (Exp id)
    | Union (Exp id) (Exp id)
    | Intersection (Exp id) (Exp id)
    | Powerset (Exp id)
    
    | ReplicatedUnion (Exp id)
    deriving (Eq, Show)
    
data ProcessRelation id =
    Performs (Exp id) (Event id) (Exp id)
    deriving Show

data Pat id = 
    PVar id 
    | PTuple [Pat id]
    | PSet [Pat id]
    deriving (Eq, Show)

-- A side condition should always be equal to SCGenerator p e1 e2
-- TODO: prefix side condition is true, need to sort this out in
-- the typechecker
data SideCondition id =
    SCGenerator (Pat id) (Exp id)
    | Formula (Formula id)
    deriving (Eq, Show)
    
data Formula id =
    Member (Exp id) (Exp id)
    | Equals (Exp id) (Exp id)
    | Subset (Exp id) (Exp id)
    | Not (Formula id)
    | And (Formula id) (Formula id)
    | Or (Formula id) (Formula id)
    | PFalse
    | PTrue
    deriving (Eq, Show)
    
-- TODO: precondition can only be of the form P -> Q
-- i.e. no operators may be applied
data InductiveRule id =
    InductiveRule [ProcessRelation id] (ProcessRelation id) [SideCondition id]
    deriving Show

-- Always omit tau promotion rules
data InputOperator id = 
    InputOperator {
        iopFriendlyName :: id,
        iopArgs :: [(id, ProcessSubtype)],
        iopRules :: [InductiveRule id],
        iopReplicatedOperator :: Maybe (InputReplicatedOperator id),
        iopParsingInformation :: Maybe OperatorSyntax
    }
    deriving Show

data InputReplicatedOperator id =
    InputReplicatedOperator {
        irepOpArgs :: [id],
        irepOpBaseCase :: ([Pat id], (Exp id)),
        -- List of vars for this case, list of vars for recursive case
        -- lengths should all be equal to the args length
        irepOpInductiveCase :: ([id], (Exp id)),
        irepOpParsingInformation :: Maybe OperatorSyntax
    }
    deriving Show
    
data InputOpSemDefinition id = 
    InputOpSemDefinition {
        inputOperators :: [InputOperator id],
        inputChannels :: [Channel id]
    }
    deriving Show
    
data Assoc = 
    AssocLeft | AssocRight | AssocNone
    deriving Show

data OperatorSyntax =
    InfixOp [ParseComponent] Integer Assoc  -- Int refers to precedence
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
    | TSet Type         -- Only sets of events are supported currently
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
        opRules :: [InductiveRule Name],
        opParsingInformation :: Maybe OperatorSyntax
    }
    | ReplicatedOperator {
        opFriendlyName :: Name,
        opArgs :: [(Name, Type)],
        repOpBaseCase :: ([Pat Name], (Exp Name)),
        -- List of vars for this case, list of vars for recursive case
        -- lengths should all be equal to the args length
        repOpInductiveCase :: ([Name], (Exp Name)),
        opParsingInformation :: Maybe OperatorSyntax
    }
    deriving Show

data Channel id = Channel {
        channelName :: id,
        channelFields :: [Exp id]
    }
    deriving Show

data OpSemDefinition = OpSemDefinition {
        operators :: [Operator],
        channels :: [Channel Name]
    }
    deriving Show

-- *************************************************************************
-- Rule Output Data Types (post compilation)
-- *************************************************************************
type ProcId = Int

data CompiledOp =
    CompiledOp {
        copFriendlyName :: Name,
        copArgs :: [(Name, Type)],
        copRules :: [CompiledRule],
        copDiscards :: [ProcId]
    }
    deriving Show

data CompiledRule =
    CompiledRule {
        crComponentEvents :: PartialFunction ProcId (Event Name),
        crResultEvent :: Event Name,
        crResultingOperatorName :: (Name, [Exp Name]),
        crF :: PartialFunction ProcId ProcId,
        crPsi :: PartialFunction ProcId ProcId,
        crChi :: PartialFunction ProcId ProcId,
        crDiscards :: [ProcId],
        crBoundVars :: [Name],
        crGenerators :: [SideCondition Name]
    }
    deriving Show
