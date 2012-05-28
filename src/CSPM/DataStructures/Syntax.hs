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
    -- ** Fields
    -- | Fields occur within prefix statements. For example, if the prefix
    -- was @c$x?y!z@ then there would be three fields, of type 'NonDetInput',
    -- 'Input' and 'Output' respectively.
    Field(..),
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
    AnModule(..), AnDecl(..), AnMatch(..), AnPat(..), AnExp(..), AnField(..),
    AnStmt(..), AnDataTypeClause(..), AnAssertion(..), AnInteractiveStmt(..),
    -- ** Pre-Renaming Types
    PModule(..), PDecl(..), PMatch(..), PPat(..), PExp(..), PField(..),
    PStmt(..), PDataTypeClause(..), PAssertion(..), PInteractiveStmt(..),
    -- ** Post-Renaming Types
    TCModule(..), TCDecl(..), TCMatch(..), TCPat(..), TCExp(..), TCField(..),
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
type AnModule id = Annotated () (Module id)
-- Declarations may bind multiple names
type AnDecl id = Annotated (Maybe SymbolTable, PSymbolTable) (Decl id)
type AnMatch id = Annotated () (Match id)
type AnPat id = Annotated () (Pat id)
type AnExp id = Annotated (Maybe Type, PType) (Exp id)
type AnField id = Annotated () (Field id)
type AnStmt id = Annotated () (Stmt id)
type AnDataTypeClause id = Annotated () (DataTypeClause id)
type AnAssertion id = Annotated () (Assertion id)
type AnInteractiveStmt id = Annotated () (InteractiveStmt id)

getType :: Annotated (Maybe Type, PType) a -> Type
getType an = case fst (annotation an) of
    Just t -> t
    Nothing -> panic "Cannot get the type of something that is not typechecked"

getSymbolTable :: Annotated (Maybe SymbolTable, PSymbolTable) a -> SymbolTable
getSymbolTable an = case fst (annotation an) of
    Just t -> t
    Nothing -> panic "Cannot get the symbol table of something that is not typechecked"

type PModule = AnModule UnRenamedName
type PDecl = AnDecl UnRenamedName
type PMatch = AnMatch UnRenamedName
type PPat = AnPat UnRenamedName
type PExp = AnExp UnRenamedName
type PStmt = AnStmt UnRenamedName
type PField = AnField UnRenamedName
type PDataTypeClause = AnDataTypeClause UnRenamedName
type PAssertion = AnAssertion UnRenamedName
type PInteractiveStmt = AnInteractiveStmt UnRenamedName

type TCModule = AnModule Name
type TCDecl = AnDecl Name
type TCMatch = AnMatch Name
type TCPat = AnPat Name
type TCExp = AnExp Name
type TCField = AnField Name
type TCStmt = AnStmt Name
type TCDataTypeClause = AnDataTypeClause Name
type TCAssertion = AnAssertion Name
type TCInteractiveStmt = AnInteractiveStmt Name

-- *************************************************************************
-- Modules
-- *************************************************************************
data Module id = 
    GlobalModule [AnDecl id]
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
data Exp id =
    -- | Function application.
    App {
        -- | The function.
        appFunction :: AnExp id,
        -- | The arguments applied to the function
        appArguments :: [AnExp id]
    }
    -- | Application of a binary boolean operator.
    | BooleanBinaryOp {
        booleanBinaryOpOperator :: BinaryBooleanOp,
        booleanBinaryOpLeftExpression :: AnExp id,
        booleanBinaryOpRightExpression :: AnExp id
    }
    -- | Application of a unary boolean operator.
    | BooleanUnaryOp {
        unaryBooleanOpOperator :: UnaryBooleanOp,
        unaryBooleanExpression :: AnExp id
    }
    -- | List concatenation, e.g. @x^y@.
    | Concat {
        concatLeftList :: AnExp id,
        concatRightList :: AnExp id
    }
    -- | Dot operator application, e.g. @c.x@.
    | DotApp {
        dotAppLeftArgument :: AnExp id,
        dotAppRighArgument :: AnExp id
    }
    -- | If statements, e.g. @if cond then e1 else e2@.
    | If {
        -- | The condition of the if.
        ifCondition :: AnExp id,
        -- | The then branch.
        ifThenBranch :: AnExp id,
        -- The else branch.
        ifElseBranch :: AnExp id
    }
    -- | Lambda functions, e.g. @\(x,y) \@ e(x,y)@.
    | Lambda {
        lambdaBindingPattern :: AnPat id,
        lambdaRightHandSide :: AnExp id
    }
    -- | Let declarations, e.g. @let func = e1 within e2@.
    | Let {
        letDeclarations :: [AnDecl id],
        letExpression :: AnExp id
    }
    -- | Literals, e.g. @true@ or @1@.
    | Lit {
        litLiteral :: Literal
    }
    -- | List literals, e.g. @<1,2,3>@.
    | List {
        listItems :: [AnExp id]
    }
    -- | List comprehensions, e.g. @<x,y | (x,y) <- e>@.
    | ListComp {
        listCompItems :: [AnExp id],
        listCompStatements :: [AnStmt id]
    }
    -- | Infinite list of integers from the given value, e.g. @<1..>@.
    | ListEnumFrom {
        listEnumFromLowerBound :: AnExp id
    }
    -- | Bounded list of integers between the given values, e.g. @<1..3>@.
    | ListEnumFromTo {
        listEnumFromToLowerBound :: AnExp id,
        listEnumFromToUpperBound :: AnExp id
    }
    -- | The length of the list, e.g. @#list@.
    | ListLength {
        listLengthExpression :: AnExp id
    }
    -- | Application of binary maths operator, e.g. @x+y@.
    | MathsBinaryOp {
        mathsBinaryOpOperator :: BinaryMathsOp,
        mathsBinaryOpLeftExpression :: AnExp id,
        mathsBinaryOpRightExpression :: AnExp id
    }
    -- | Application of unary maths operator, e.g. @-x@.
    | MathsUnaryOp {
        mathsUnaryOpOperator :: UnaryMathsOp,
        mathsUnaryOpExpression :: AnExp id
    }
    -- | A user provided bracket, e.g. @(e)@.
    | Paren {
        parenExpression :: AnExp id
    }
    -- | Set literals, e.g. @{1,2,3}@.
    | Set {
        setItems :: [AnExp id]
    }
    -- | Set comprehensions, e.g. @{x,y | (x,y) <- e}@.
    | SetComp {
        setCompItems :: [AnExp id],
        setCompStatements :: [AnStmt id]
    }
    -- | Enumerated Sets, i.e. sets that complete the events, e.g. @{| c.x |}@.
    | SetEnum {
        setEnumItems :: [AnExp id]
    }
    -- | Set comprehension version of 'SetEnum', e.g. @{| c.x | x <- xs |}@.
    | SetEnumComp {
        setEnumCompItems :: [AnExp id],
        setEnumCompStatements :: [AnStmt id]
    }
    -- | The infinite set of integers from the given value, e.g. @{5..}@.
    | SetEnumFrom {
        setEnumFromLowerBound :: AnExp id
    }
    -- | The bounded set of integers between the two given values, e.g. 
    -- @{5..6}@.
    | SetEnumFromTo {
        -- | The lower bound.
        setEnumFromToLowerBound :: AnExp id,
        -- | The upper bound.
        setEnumFromToUpperBound :: AnExp id
    }
    -- | Tuples, e.g. @(1,2)@.
    | Tuple {
        tupleItems :: [AnExp id]
    }
    -- | Variables, e.g. @x@.
    | Var {
        varIdentity :: id
    }

    -- Processes

    -- | Alphabetised parallel, e.g. @P [A || B] Q@.
    | AlphaParallel {
        -- | Process 1.
        alphaParLeftProcess :: AnExp id,
        -- | Alphabet of process 1.
        alphaParAlphabetLeftProcess :: AnExp id,
        -- | Alphabet of process 2.
        alphaParAlphabetRightProcess :: AnExp id,
        -- | Process 2.
        alphaParRightProcess :: AnExp id
    }
    -- | Exception operator, e.g. @P [| A |> Q@.
    | Exception {
        exceptionLeftProcess :: AnExp id,
        exceptionAlphabet :: AnExp id,
        exceptionRightProcess :: AnExp id
    }
    -- | External choice, e.g. @P [] Q@.
    | ExternalChoice {
        extChoiceLeftProcess :: AnExp id,
        extChoiceRightOperator :: AnExp id
    }
    -- | Generalised parallel, e.g. @P [| A |] Q@.
    | GenParallel {
        genParallelLeftProcess :: AnExp id,
        genParallelAlphabet :: AnExp id,
        genParallelRightProcess :: AnExp id
    }
    -- | Guarded expressions, e.g. @b & P@ where @b@ is a boolean expression.
    -- This is equivalent to @if b then P else STOP@.
    | GuardedExp {
        guardedExpCondition :: AnExp id,
        guardedExpProcess :: AnExp id
    }
    -- | Hiding of events, e.g. @P \ A@.
    | Hiding {
        -- | The process the hiding is applied to.
        hidingProcess :: AnExp id,
        -- | The set of events to be hidden.
        hidingAlphabet :: AnExp id
    }
    -- | Internal choice, e.g. @P |~| Q@.
    | InternalChoice {
        intChoiceLeftProcess :: AnExp id,
        intChoiceRightProcess :: AnExp id
    }
    -- | Interrupt (where the left process is turned off once the right process
    -- performs an event), e.g. @P /\ Q@.
    | Interrupt {
        interruptLeftProcess :: AnExp id,
        interruptRightProcess :: AnExp id
    }
    -- | Interleaving of processes, e.g. @P ||| Q@.
    | Interleave {
        interleaveLeftProcess :: AnExp id,
        interleaveRightProcess :: AnExp id
    }
    -- Linked parallel, e.g. @P [a.x <- b.x | x <- X] Q@.
    | LinkParallel {
        linkParLeftProcess :: AnExp id,
        linkParTiedEvents :: [(AnExp id, AnExp id)],
        linkParTieStatements :: [AnStmt id],
        linkParRightProcess :: AnExp id
    }
    -- | Event prefixing, e.g. @c$x?y!z -> P@.
    | Prefix {
        prefixChannel :: AnExp id,
        prefixFields :: [AnField id],
        prefixProcess :: AnExp id
    }
    -- | Event renaming, e.g. @P [[ a.x <- b.x | x <- X ]]@.
    | Rename {
        -- | The process that is renamed.
        renameProcess :: AnExp id,
        -- | The events that are renamed, in the format of @(old, new)@.
        renameTiedEvents :: [(AnExp id, AnExp id)],
        -- | The statements for the ties.
        renameTieStatements :: [AnStmt id]
    }
    -- | Sequential composition, e.g. @P; Q@.
    | SequentialComp {
        seqCompLeftProcess :: AnExp id,
        seqCompRightProcess :: AnExp id
    }
    -- | Sliding choice, e.g. @P |> Q@.
    | SlidingChoice {
        slidingChoiceLeftProcess :: AnExp id,
        slidingChoiceRightProcess :: AnExp id
    }

    -- Replicated Operators
    -- | Replicated alphabetised parallel, e.g. @|| x : X \@ [| A(x) |] P(x)@.
    | ReplicatedAlphaParallel {
        repAlphaParReplicatedStatements :: [AnStmt id],
        repAlphaParAlphabet :: AnExp id,
        repAlphaParProcess :: AnExp id
    }
    -- | Replicated external choice, e.g. @[] x : X \@ P(x)@.
    | ReplicatedExternalChoice {
        repExtChoiceReplicatedStatements :: [AnStmt id],
        repExtChoiceProcess :: AnExp id
    }
    -- | Replicated interleave, e.g. @||| x : X \@ P(x)@.
    | ReplicatedInterleave {
        repInterleaveReplicatedStatements :: [AnStmt id],
        repInterleaveProcess :: AnExp id
    }
    -- | Replicated internal choice, e.g. @|~| x : X \@ P(x)@.
    | ReplicatedInternalChoice {
        repIntChoiceReplicatedStatements :: [AnStmt id],
        repIntChoiceProcess :: AnExp id
    }
    -- | Replicated link parallel, e.g. 
    -- @[a.x <- b.x | x <- X(y)] y : Y \@ P(y)@.
    | ReplicatedLinkParallel {
        -- | The tied events.
        repLinkParTiedChannels :: [(AnExp id, AnExp id)],
        -- | The statements for the ties.
        repLinkParTieStatements :: [AnStmt id],
        -- | The 'Stmt's - the process (and ties) are evaluated once for each 
        -- value generated by these.
        repLinkParReplicatedStatements :: [AnStmt id],
        -- | The process
        repLinkParProcess :: AnExp id
    }
    -- | Replicated parallel, e.g. @[| A |] x : X \@ P(x)@.
    | ReplicatedParallel {
        repParAlphabet :: AnExp id,
        repParReplicatedStatements :: [AnStmt id],
        repParProcess :: AnExp id
    }
    
    -- | Used only for parsing - never appears in an AST.
    | ExpPatWildCard
    -- | Used only for parsing - never appears in an AST.
    | ExpPatDoublePattern (AnExp id) (AnExp id)
    
    deriving (Eq, Show)

data Field id = 
    -- | @!x@
    Output (AnExp id)
    -- | @?x:A@
    | Input (AnPat id) (Maybe (AnExp id))
    -- | @$x:A@ (see P395 UCS)
    | NonDetInput (AnPat id) (Maybe (AnExp id))
    deriving (Eq, Show)
    
data Stmt id = 
    Generator (AnPat id) (AnExp id)
    | Qualifier (AnExp id)
    deriving (Eq, Show)

-- | A statement in an interactive session.
data InteractiveStmt id =
    Evaluate (AnExp id)
    | Bind (AnDecl id)
    | RunAssertion (Assertion id)
    deriving Show
    
-- *************************************************************************
-- Declarations
-- *************************************************************************

data Decl id = 
    -- | A function binding, e.g. @func(x,y)(z) = 0@.
    FunBind id [AnMatch id]
    -- | The binding of a pattern to an expression, e.g. @(p,q) = e@.
    | PatBind (AnPat id) (AnExp id)
    -- | An assertion in a file, e.g. @assert P [T= Q@.
    | Assert (Assertion id)
    -- | An import of an external function, e.g. @external test@,
    | External {
        externalImportedNames :: [id]
    }
    -- | An import of a transparent function, e.g. @transparent normal@.
    | Transparent {
        transparentImportedNames :: [id]
    }
    -- | A channel declaration, e.g. @channel c, d : {0..1}.{0..1}@.
    | Channel [id] (Maybe (AnExp id))
    -- | A datatype declaration, e.g. @datatype T = Clause1 | Clause2@.
    | DataType id [AnDataTypeClause id]
    -- | A nametype declaration, e.g. @nametype T2 = T.T@.
    | NameType id (AnExp id)
    deriving (Eq, Show)

data Assertion id = 
    -- | A refinement assertion, e.g. @assert P [F= Q@.
    Refinement {
        refinementSpecification :: AnExp id,
        refinementModel :: Model,
        refinementImplementation :: AnExp id,
        refinementModelOptions :: [ModelOption id]
    }
    -- | A check of property, like deadlock freedom, e.g. 
    -- @assert P :[deadlock free [F]]@.
    | PropertyCheck {
        propertyCheckProcess :: AnExp id,
        propertyCheckProperty :: SemanticProperty,
        propertyCheckModel :: Maybe Model
    }
    -- | The negation of an assertion, not currently supported.
    | ASNot (Assertion id)
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
    
data ModelOption id = 
    TauPriority (AnExp id)
    deriving (Eq, Show)
        
data SemanticProperty = 
    DeadlockFreedom
    | Deterministic     
    | LivelockFreedom
    deriving (Eq, Show)
    
-- | The clause of a datatype, e.g. if a datatype declaration was:
--
-- > datatype T = A.Int.Bool | B.Bool | C
--
-- Then T would have three datatype clauses, one for each of its tags (i.e.
-- @A@, @B@ and @C@).
data DataTypeClause id =
    DataTypeClause {
        -- | The name of the datatype clause.
        dataTypeClauseName :: id,
        -- | The expression that gives the set of values that can be dotted
        -- with this clause. For example, in the above example the datatype
        -- clause for A would have "Int.Bool" as its type expression.
        dataTypeClauseTypeExpression :: Maybe (AnExp id)
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
data Match id =
    Match {
        -- | The patterns that need to be matched. This is a list of lists as
        -- functions may be curried, like @f(x,y)(z) = ...@.
        matchPatterns :: [[AnPat id]],
        -- | The expression to be evaluated if the match succeeds.
        matchRightHandSide :: AnExp id
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
