module CSPM.DataStructures.Syntax where

import CSPM.DataStructures.Types
import CSPM.DataStructures.Names
import Util.Annotated

-- P = post parsing, TC = post typechecking, An = annotated
type AnModule = Annotated () Module
-- Declarations may bind multiple names
type AnDecl = Annotated PSymbolTable Decl
type AnMatch = Annotated () Match
type AnPat = Annotated () Pat
type AnExp = Annotated PType Exp
type AnField = Annotated () Field
type AnStmt = Annotated () Stmt
type AnDataTypeClause = Annotated () DataTypeClause
type AnAssertion = Annotated () Assertion
type AnInteractiveStmt = Annotated () InteractiveStmt

type PModule = AnModule
type PDecl = AnDecl
type PMatch = AnMatch
type PPat = AnPat
type PExp = AnExp
type PStmt = AnStmt
type PField = AnField
type PDataTypeClause = AnDataTypeClause
type PAssertion = AnAssertion
type PInteractiveStmt = AnInteractiveStmt

type TCModule = AnModule
type TCDecl = AnDecl
type TCMatch = AnMatch
type TCPat = AnPat
type TCExp = AnExp
type TCField = AnField
type TCStmt = AnStmt
type TCDataTypeClause = AnDataTypeClause
type TCAssertion = AnAssertion
type TCInteractiveStmt = AnInteractiveStmt

-- *************************************************************************
-- Basic Components
-- *************************************************************************
data Literal = 
	Int Integer
	| Bool Bool
	deriving (Eq, Show)

-- *************************************************************************
-- Modules
-- *************************************************************************
data Module = 
	GlobalModule [AnDecl]
	deriving (Eq, Show)

-- *************************************************************************
-- Expressions
-- *************************************************************************
data BinaryBooleanOp =
	And 
	| Or 
	| Equals 
	| NotEquals 
	| LessThan 
	| GreaterThan 
	| LessThanEq 
	| GreaterThanEq
	deriving (Eq, Show)
	
data UnaryBooleanOp =
	Not
	deriving (Eq, Show)

data UnaryMathsOp = 
	Negate
	deriving (Eq, Show)

data BinaryMathsOp = 
	Divide | Minus | Mod | Plus | Times
	deriving (Eq, Show)

data Exp =
	App AnExp [AnExp]
	| BooleanBinaryOp BinaryBooleanOp AnExp AnExp
	| BooleanUnaryOp UnaryBooleanOp AnExp
	| Concat AnExp AnExp
	| DotApp AnExp AnExp
	| If AnExp AnExp AnExp
	| Lambda AnPat AnExp
	| Let [AnDecl] AnExp
	| Lit Literal
	| List [AnExp]
	| ListComp [AnExp] [AnStmt]
	| ListEnumFrom AnExp
	| ListEnumFromTo AnExp AnExp
	-- TODO: ListEnumFrom and ListEnumTO - test in FDR
	-- TODO: compare with official CSPM syntax
	| ListLength AnExp
	| MathsBinaryOp BinaryMathsOp AnExp AnExp
	| MathsUnaryOp UnaryMathsOp AnExp
	| Paren AnExp
	| Set [AnExp]
	| SetComp [AnExp] [AnStmt]
	| SetEnum [AnExp]			-- {| |}
	| SetEnumComp [AnExp] [AnStmt]	-- {|c.x | x <- xs|}
	| SetEnumFrom AnExp
	| SetEnumFromTo AnExp AnExp
	| Tuple [AnExp]
	| Var QualifiedName

	-- Processes
	| AlphaParallel AnExp AnExp AnExp AnExp -- Proc Alpha Proc Alpha
	| Exception AnExp AnExp AnExp -- Proc Alpha Proc
	| ExternalChoice AnExp AnExp
	| GenParallel AnExp AnExp AnExp -- Proc Alpha Proc 
	| GuardedExp AnExp AnExp			-- b & P
	| Hiding AnExp AnExp
	| InternalChoice AnExp AnExp
	| Interrupt AnExp AnExp
	| Interleave AnExp AnExp
	| LinkParallel AnExp [(AnExp, AnExp)] [AnStmt] AnExp -- Exp, tied chans, generators, second
	| Prefix AnExp [AnField] AnExp
	| Rename AnExp [(AnExp, AnExp)] [AnStmt]
	| SequentialComp AnExp AnExp -- P; Q
	| SlidingChoice AnExp AnExp

	-- Replicated Operators
	| ReplicatedAlphaParallel [AnStmt] AnExp AnExp -- alpha exp is second
	| ReplicatedInterleave [AnStmt] AnExp 
	| ReplicatedExternalChoice [AnStmt] AnExp
	| ReplicatedInternalChoice [AnStmt] AnExp
	| ReplicatedParallel AnExp [AnStmt] AnExp -- alpha exp is first
	| ReplicatedLinkParallel [(AnExp, AnExp)] [AnStmt] AnExp
	
	-- Used only for parsing
	| ExpPatWildCard
	| ExpPatDoublePattern AnExp AnExp
	
	deriving (Eq, Show)

data Field = 
	Output AnExp -- !x
	| Input AnPat (Maybe AnExp)    -- ?x:A
	| NonDetInput AnPat (Maybe AnExp) -- $x:A (see P395 UCS)
	deriving (Eq, Show)
	
data Stmt = 
	Generator AnPat AnExp
	| Qualifier AnExp
	deriving (Eq, Show)

-- A statement in an interactive session
data InteractiveStmt =
	Evaluate AnExp
	| Bind AnDecl
	deriving Show
	
-- *************************************************************************
-- Declarations
-- *************************************************************************
data Decl = 
	-- Third argument is the annotated type
	FunBind Name [AnMatch]
	| PatBind AnPat AnExp
	| Assert Assertion
	| External [Name]
	| Transparent [Name]
	-- The expression in the following three definitions means a type expression
	-- and therefore dots and commas have special meanings. See TPC P529 for
	-- details (or the typechecker or evaluator).
	| Channel [Name] (Maybe AnExp)
	| DataType Name [AnDataTypeClause]
	| NameType Name AnExp
	deriving (Eq, Show)

-- TODO: annotate
data Assertion = 
	Refinement AnExp Model AnExp [ModelOption]
 	| PropertyCheck AnExp SemanticProperty (Maybe Model)
	| BoolAssertion AnExp
	| ASNot Assertion
	deriving (Eq, Show)
		
data Model = 
	Traces 
	| Failures 
	| FailuresDivergences 
	| Refusals
	| RefusalsDivergences
	| Revivals
	| RevivalsDivergences
	deriving (Eq, Show)
	
data ModelOption = 
   	TauPriority AnExp
	deriving (Eq, Show)
	   	
data SemanticProperty = 
 	DeadlockFreedom
 	| Deterministic 	
 	| LivelockFreedom
	deriving (Eq, Show)
	
-- TODO: annotate
data DataTypeClause =
	DataTypeClause Name (Maybe AnExp)
	deriving (Eq, Show)
	
data Match =
	Match [[AnPat]] AnExp
	deriving (Eq, Show)

data Pat =
	PConcat AnPat AnPat
	| PDotApp AnPat AnPat
	| PDoublePattern AnPat AnPat
	| PList [AnPat]
	| PLit Literal
	| PParen AnPat
	| PSet [AnPat]
	| PTuple [AnPat]
	| PVar Name
	| PWildCard
	
	-- In all compiled patterns we store the original pattern
	-- Because of the fact that you can write patterns such as:
	--  f(<x,y>^xs^<z,p>)
	--  f(<x,y>)
	--  f(xs^<x,y>)
	-- we need an easy may of matching them. Thus, we compile
	-- the patterns to a PCompList instead.
	-- PCompList ps (Just (p, ps')) corresponds to a list
	-- where it starts with ps (where each p in ps matches exactly one
	-- component, has a middle of p and and end matching exactly ps'
	| PCompList [AnPat] (Maybe (AnPat, [AnPat])) Pat
	-- Recall the longest match rule when evaluating this
	-- How about:
	-- channel c : A.Int.A
	-- datatype A = B.Bool
	-- func(c.B.true.x) =
	-- func(c.B.false.0.B.x) =
	| PCompDot [AnPat] Pat

	deriving (Eq, Show)
