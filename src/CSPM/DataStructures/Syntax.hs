-- | This module represents the abstract syntax tree of machine CSP.
-- Most of the datatypes are parameterised over the type of variables that they
-- contain. Before renaming (by 'CSPM.Renamer') the variables are of type 
-- 'UnRenamedName', wheras after renaming they are of type 'Name' (and are
-- hence associated with their bindings instances). Furthermore, nearly all
-- pieces of syntax are annoated with their location in the source code, and
-- (sometimes) with their type (but only after type checking). This is done 
-- using the 'Annotated' datatype.
module CSPM.DataStructures.Syntax (
    -- * Modules
    Module(..),
    -- * Declarations
    Decl(..), Match(..),
    -- ** Assertions
    Assertion(..), Model(..), ModelOption(..), SemanticProperty(..),
    -- ** Data Type Clauses
    DataTypeClause(..),
    -- * Expressions
    Exp(..), BinaryMathsOp(..), BinaryBooleanOp(..), UnaryMathsOp(..), 
    UnaryBooleanOp(..),
    -- ** Statements
    -- | Statements occur on the right hand side of a list comprehension, or
    -- in the context of replicated operators. For example, in
    -- @<... | x <- y, func(b)>@, @x <- y@ and @func(b)@ are both statements,
    -- of type 'Generator' and 'Qualifier' respectively.
    Stmt(..),
    -- * Patterns
    -- | Patterns match against values and may bind some components of the
    -- values to variables.
    Pat(..),
    -- * Interactive Statements
    -- | Interactive statements are intended to be input by an interactive
    -- editor.
    InteractiveStmt(..),
    -- * Type Synonyms
    -- | As the types are parameterised over the type of names it can be
    -- laborious to type the names. Therefore, some shortcuts are provided.
    AnModule(..), AnDecl(..), AnMatch(..), AnPat(..), AnExp(..),
    AnStmt(..), AnDataTypeClause(..), AnAssertion(..), AnInteractiveStmt(..),
    -- ** Pre-Renaming Types
    PModule(..), PDecl(..), PMatch(..), PPat(..), PExp(..),
    PStmt(..), PDataTypeClause(..), PAssertion(..), PInteractiveStmt(..),
    -- ** Post-Renaming Types
    TCModule(..), TCDecl(..), TCMatch(..), TCPat(..), TCExp(..),
    TCStmt(..), TCDataTypeClause(..), TCAssertion(..), TCInteractiveStmt(..),
    -- * Helpers
    getType, getSymbolTable,
) where

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Types
import Util.Annotated
import Util.Exception

-- P = post parsing, TC = post typechecking, An = annotated
type AnModule id p = Annotated () (Module id p)
-- Declarations may bind multiple names
type AnDecl id p = Annotated (Maybe SymbolTable, PSymbolTable) (Decl id p)
type AnMatch id p = Annotated () (Match id p)
type AnPat id = Annotated () (Pat id)
type AnExp id p = Annotated (Maybe Type, PType) (Exp id p)
type AnStmt id p = Annotated () (Stmt id p)
type AnDataTypeClause id p = Annotated () (DataTypeClause id p)
type AnAssertion id p = Annotated () (Assertion id p)
type AnInteractiveStmt id p = Annotated () (InteractiveStmt id p)

getType :: Annotated (Maybe Type, PType) a -> Type
getType an = case fst (annotation an) of
    Just t -> t
    Nothing -> panic "Cannot get the type of something that is not typechecked"

getSymbolTable :: Annotated (Maybe SymbolTable, PSymbolTable) a -> SymbolTable
getSymbolTable an = case fst (annotation an) of
    Just t -> t
    Nothing -> panic "Cannot get the symbol table of something that is not typechecked"

type PModule p = AnModule UnRenamedName p
type PDecl p = AnDecl UnRenamedName p
type PMatch p = AnMatch UnRenamedName p
type PPat = AnPat UnRenamedName
type PExp p = AnExp UnRenamedName p
type PStmt p = AnStmt UnRenamedName p
type PDataTypeClause p = AnDataTypeClause UnRenamedName p
type PAssertion p = AnAssertion UnRenamedName p
type PInteractiveStmt p = AnInteractiveStmt UnRenamedName p

type TCModule p = AnModule Name p
type TCDecl p = AnDecl Name p
type TCMatch p = AnMatch Name p
type TCPat = AnPat Name
type TCExp p = AnExp Name p
type TCStmt p = AnStmt Name p
type TCDataTypeClause p = AnDataTypeClause Name p
type TCAssertion p = AnAssertion Name p
type TCInteractiveStmt p = AnInteractiveStmt Name p

-- *************************************************************************
-- Modules
-- *************************************************************************
data Module id p = 
    GlobalModule [AnDecl id p]
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

-- | An expression.
data Exp id p =
    -- | Function application.
    App {
        -- | The function.
        appFunction :: AnExp id p,
        -- | The arguments applied to the function
        appArguments :: [AnExp id p]
    }
    -- | Application of a binary boolean operator.
    | BooleanBinaryOp {
        booleanBinaryOpOperator :: BinaryBooleanOp,
        booleanBinaryOpLeftExpression :: AnExp id p,
        booleanBinaryOpRightExpression :: AnExp id p
    }
    -- | Application of a unary boolean operator.
    | BooleanUnaryOp {
        unaryBooleanOpOperator :: UnaryBooleanOp,
        unaryBooleanExpression :: AnExp id p
    }
    -- | List concatenation, e.g. @x^y@.
    | Concat {
        concatLeftList :: AnExp id p,
        concatRightList :: AnExp id p
    }
    -- | Dot operator application, e.g. @c.x@.
    | DotApp {
        dotAppLeftArgument :: AnExp id p,
        dotAppRighArgument :: AnExp id p
    }
    -- | If statements, e.g. @if cond then e1 else e2@.
    | If {
        -- | The condition of the if.
        ifCondition :: AnExp id p,
        -- | The then branch.
        ifThenBranch :: AnExp id p,
        -- The else branch.
        ifElseBranch :: AnExp id p
    }
    -- | Lambda functions, e.g. @\(x,y) \@ e(x,y)@.
    | Lambda {
        lambdaBindingPattern :: AnPat id,
        lambdaRightHandSide :: AnExp id p
    }
    -- | Let declarations, e.g. @let func = e1 within e2@.
    | Let {
        letDeclarations :: [AnDecl id p],
        letExpression :: AnExp id p
    }
    -- | Literals, e.g. @true@ or @1@.
    | Lit {
        litLiteral :: Literal
    }
    -- | List literals, e.g. @<1,2,3>@.
    | List {
        listItems :: [AnExp id p]
    }
    -- | List comprehensions, e.g. @<x,y | (x,y) <- e>@.
    | ListComp {
        listCompItems :: [AnExp id p],
        listCompStatements :: [AnStmt id p]
    }
    -- | Infinite list of integers from the given value, e.g. @<1..>@.
    | ListEnumFrom {
        listEnumFromLowerBound :: AnExp id p
    }
    -- | Bounded list of integers between the given values, e.g. @<1..3>@.
    | ListEnumFromTo {
        listEnumFromToLowerBound :: AnExp id p,
        listEnumFromToUpperBound :: AnExp id p
    }
    -- | The length of the list, e.g. @#list@.
    | ListLength {
        listLengthExpression :: AnExp id p
    }
    -- | Application of binary maths operator, e.g. @x+y@.
    | MathsBinaryOp {
        mathsBinaryOpOperator :: BinaryMathsOp,
        mathsBinaryOpLeftExpression :: AnExp id p,
        mathsBinaryOpRightExpression :: AnExp id p
    }
    -- | Application of unary maths operator, e.g. @-x@.
    | MathsUnaryOp {
        mathsUnaryOpOperator :: UnaryMathsOp,
        mathsUnaryOpExpression :: AnExp id p
    }
    -- | A user provided bracket, e.g. @(e)@.
    | Paren {
        parenExpression :: AnExp id p
    }
    -- | Set literals, e.g. @{1,2,3}@.
    | Set {
        setItems :: [AnExp id p]
    }
    -- | Set comprehensions, e.g. @{x,y | (x,y) <- e}@.
    | SetComp {
        setCompItems :: [AnExp id p],
        setCompStatements :: [AnStmt id p]
    }
    -- | Enumerated Sets, i.e. sets that complete the events, e.g. @{| c.x |}@.
    | SetEnum {
        setEnumItems :: [AnExp id p]
    }
    -- | Set comprehension version of 'SetEnum', e.g. @{| c.x | x <- xs |}@.
    | SetEnumComp {
        setEnumCompItems :: [AnExp id p],
        setEnumCompStatements :: [AnStmt id p]
    }
    -- | The infinite set of integers from the given value, e.g. @{5..}@.
    | SetEnumFrom {
        setEnumFromLowerBound :: AnExp id p
    }
    -- | The bounded set of integers between the two given values, e.g. 
    -- @{5..6}@.
    | SetEnumFromTo {
        -- | The lower bound.
        setEnumFromToLowerBound :: AnExp id p,
        -- | The upper bound.
        setEnumFromToUpperBound :: AnExp id p
    }
    -- | Tuples, e.g. @(1,2)@.
    | Tuple {
        tupleItems :: [AnExp id p]
    }
    -- | Variables, e.g. @x@.
    | Var {
        varIdentity :: id
    }
    -- Processes
    | Process (p id)
    -- | Used only for parsing - never appears in an AST.
    | ExpPatWildCard
    -- | Used only for parsing - never appears in an AST.
    | ExpPatDoublePattern (AnExp id p) (AnExp id p)
    deriving (Eq, Show)
    
data Stmt id p = 
    Generator (AnPat id) (AnExp id p)
    | Qualifier (AnExp id p)
    deriving (Eq, Show)

-- | A statement in an interactive session.
data InteractiveStmt id p =
    Evaluate (AnExp id p)
    | Bind (AnDecl id p)
    | RunAssertion (Assertion id p)
    deriving Show
    
-- *************************************************************************
-- Declarations
-- *************************************************************************

data Decl id p = 
    -- | A function binding, e.g. @func(x,y)(z) = 0@.
    FunBind id [AnMatch id p]
    -- | The binding of a pattern to an expression, e.g. @(p,q) = e@.
    | PatBind (AnPat id) (AnExp id p)
    -- | An assertion in a file, e.g. @assert P [T= Q@.
    | Assert (Assertion id p)
    -- | An import of an external function, e.g. @external test@,
    | External {
        externalImportedNames :: [id]
    }
    -- | An import of a transparent function, e.g. @transparent normal@.
    | Transparent {
        transparentImportedNames :: [id]
    }
    -- | A channel declaration, e.g. @channel c, d : {0..1}.{0..1}@.
    | Channel [id] (Maybe (AnExp id p))
    -- | A datatype declaration, e.g. @datatype T = Clause1 | Clause2@.
    | DataType id [AnDataTypeClause id p]
    -- | A nametype declaration, e.g. @nametype T2 = T.T@.
    | NameType id (AnExp id p)
    deriving (Eq, Show)

data Assertion id p = 
    -- | A refinement assertion, e.g. @assert P [F= Q@.
    Refinement {
        refinementSpecification :: AnExp id p,
        refinementModel :: Model,
        refinementImplementation :: AnExp id p,
        refinementModelOptions :: [ModelOption id p]
    }
    -- | A check of property, like deadlock freedom, e.g. 
    -- @assert P :[deadlock free [F]]@.
    | PropertyCheck {
        propertyCheckProcess :: AnExp id p,
        propertyCheckProperty :: SemanticProperty,
        propertyCheckModel :: Maybe Model
    }
    -- | A boolean assertion, not currently supported.
    | BoolAssertion (AnExp id p)
    -- | The negation of an assertion, not currently supported.
    | ASNot (Assertion id p)
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
    
data ModelOption id p = 
    TauPriority (AnExp id p)
    deriving (Eq, Show)
        
data SemanticProperty = 
    DeadlockFreedom
    | Deterministic     
    | LivelockFreedom
    deriving (Eq, Show)
    
-- TODO: annotate
-- | The clause of a datatype, e.g. if a datatype declaration was:
--
-- > datatype T = A.Int.Bool | B.Bool | C
--
-- Then T would have three datatype clauses, one for each of its tags (i.e.
-- @A@, @B@ and @C@).
data DataTypeClause id p =
    DataTypeClause {
        -- | The name of the datatype clause.
        dataTypeClauseName :: id,
        -- | The expression that gives the set of values that can be dotted
        -- with this clause. For example, in the above example the datatype
        -- clause for A would have "Int.Bool" as its type expression.
        dataTypeClauseTypeExpression :: Maybe (AnExp id p)
    }
    deriving (Eq, Show)

-- | Matches occur on the left hand side of a function declaration and there
-- is one 'Match' for each clause of the declaration. For example, given the
-- declaration:
--
-- @
--      f(<>) = 0
--      f(<x>^xs) = 1+f(xs)
-- @
--
-- there would be two matches.
data Match id p =
    Match {
        -- | The patterns that need to be matched. This is a list of lists as
        -- functions may be curried, like @f(x,y)(z) = ...@.
        matchPatterns :: [[AnPat id]],
        -- | The expression to be evaluated if the match succeeds.
        matchRightHandSide :: AnExp id p
    }
    deriving (Eq, Show)

data Pat id =
    -- | The concatenation of two patterns, e.g. @p1^p2@.
    PConcat {
        pConcatLeftPat :: AnPat id,
        pConcatRightPat :: AnPat id
    }
    -- | The dot of two patterns, e.g. @p1.p2@.
    | PDotApp {
        pDotLeftPat :: AnPat id,
        pDotRightPat :: AnPat id
    }
    -- | A double pattern match, e.g. @p1\@\@p2@.
    | PDoublePattern {
        pDoublePatLeftPat :: AnPat id,
        pDoublePatRightPat :: AnPat id
    }
    -- | A literal pattern list, e.g. @<p1,p2,p3>@.
    | PList {
        pListItems :: [AnPat id]
    }
    -- | A literal pattern, e.g. @true@, or @0@.
    | PLit {
        pLitLiteral :: Literal
    }
    -- | A user supplied parenthesis in a pattern.
    | PParen {
        pParenPattern :: AnPat id
    }
    -- | A set pattern. Only singleton patterns, or zero patterns are supported.
    -- This is checked by the desugarer. For example, @{p1,p2}@ is not allowed,
    -- but @{p1}@ and @{}@ are allowed.
    | PSet {
        pSetItems :: [AnPat id]
    }
    -- | A tuple pattern, e.g. @(p1,p2,p3)@.
    | PTuple {
        pTupleItems :: [AnPat id]
    }
    -- | A variable pattern, e.g. @x@, or @A@ where @A@ is a datatype clause. 
    -- If the variable is a datatype clause then it only matches that datatype
    -- tag, whereas for anything else it matches anything.
    | PVar {
        pVarIdentity :: id
    }
    -- | Matches anything but does not bind it.
    | PWildCard
    
    -- | Since you can write list patterns such as:
    -- 
    -- > f(<x,y>^xs^<z,p>^<9,0>)
    -- > f(<x,y>)
    -- > f(xs^<x,y>)
    --
    -- we need an easy may of matching them. Thus, we compile
    -- the patterns to a @PCompList@ instead.
    -- 
    -- @PCompList ps (Just (p, ps'))@ corresponds to a list
    -- where it starts with ps (where each p in ps matches exactly one
    -- list element, has a middle of p (which must be a variable pattern, 
    -- or a wildcard) and and end matching exactly ps' (again, where each p
    -- in ps matches exactly one list component).
    | PCompList {
        pListStartItems :: [AnPat id],
        pListMiddleEndItems :: Maybe (AnPat id, [AnPat id]),
        pListOriginalPattern :: Pat id
    }
    -- | Like with a 'PCompList' we flatten nested dot patterns to make it
    -- easier to evaluate.
    | PCompDot {
        pDotItems :: [AnPat id],
        pDotOriginalpattern :: Pat id
    }

    deriving (Eq, Show)
