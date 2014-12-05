{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
    TypeSynonymInstances #-}
module CSPM.DataStructures.Types (
    -- * Data Structures
    TypeVar, TypeScheme(..), Constraint(..), Type(..), TypeVarRef(..),
    prettyPrintTypes, isRigid, constraintImpliedBy, reduceConstraints,
    collectConstraints, prettyPrintTypeSchemes, VariableRepresentationMap,
    prettyPrintTypesWithMap, prettyPrintTypeSchemesWithMap, augamentTypeScheme,
    isPolymorphic,

    -- * Creation of Types
    freshTypeVar, freshTypeVarWithConstraints, freshTypeVarRef,
    freshRigidTypeVarWithConstraints,

    -- * Symbol Tables
    SymbolTable, PSymbolTable, freshPSymbolTable, readPSymbolTable, 
    setPSymbolTable,

    -- * Type Pointers
    PType, freshPType, readPType, setPType,
) where

import Control.Monad.Reader
import Data.IORef
import Data.List
import qualified Data.Map as Mp
import Data.Supply
import System.IO.Unsafe

import CSPM.DataStructures.Names
import qualified Util.MonadicPrettyPrint as M
import Util.Exception
import Util.PartialFunctions
import Util.Precedence
import Util.Prelude
import Util.PrettyPrint

-- *************************************************************************
-- Types
-- *************************************************************************
newtype TypeVar = TypeVar Int deriving (Eq, Ord, Show)

instance PrettyPrintable TypeVar where
    prettyPrint (TypeVar x) = int x

data TypeScheme =
    ForAll {
        typeSchemeVariables :: [(TypeVar, [Constraint])],
        typeSchemeType :: Type
    }
    deriving (Eq, Show)
    
data Constraint =
    -- | Comparable for equality
    CEq
    -- | Orderable
    | COrd 
    -- | Can be input on a channel
    | CInputable
    -- | Can form sets of the type.
    | CSet
    -- | Is something that can be yielded on the right hand side of =>.
    | CYieldable
    deriving (Eq, Ord, Show)

constraintImpliedBy :: Constraint -> Constraint -> Bool
constraintImpliedBy c1 c2 | c1 == c2 = True
constraintImpliedBy CSet CEq = True
constraintImpliedBy _ _ = False

reduceConstraints :: [Constraint] -> [Constraint]
reduceConstraints cs = 
    case [c | c <- cs, c' <- cs, c /= c', constraintImpliedBy c c'] of
        [] -> cs
        (c:_) -> reduceConstraints (cs \\ [c])

-- During Type Checking we use TDotable a b only when a is something
-- atomic. Except, during unification we start doing TDotable (TDot...)
-- and may build up large TDotable a b types.
data Type =
    TVar TypeVarRef
    | TProc
    | TInt
    | TBool
    | TChar
    | TEvent
    -- | Something that can be extended via some means to a given type.
    --
    -- After type-checking, the TypeVarRef will simply be a variable that
    -- contains Nothing. This means that it can be converted into an explicit
    -- TDotable via some unknown means, or directly to the return type.
    --
    -- The variable argument here has a slightly special role during
    -- unification. If it contains Nothing then this is extendable via some
    -- unknown means to the specified type. If it contains a TDotable argt rt,
    -- then we know that one of the arguments is argt, and the remaining
    -- arguments are rt, which must either be a Dotable or a variable. The
    -- meaning in the former sense is recursive, the meaning in the latter case
    -- is clear. If it contains  TVar tvref, then this means tvref has replaced
    -- this argument variable.
    --
    -- We need to do the above as we may have multiple things that are
    -- extendable in the same way, so we need to sync the arguments together.
    | TExtendable {
        extendableUltimateType :: Type,
        extendableArgument :: TypeVarRef
    }
    -- | This type is used only during type-checking, and is guaranteed to only
    -- ever appear at the top-level of the left-hand side of a TExtendable.
    | TExtendableEmptyDotList
    | TSet Type
    | TSeq Type
    | TDot Type Type
    | TMap {
        mapKeyType :: Type,
        mapValueType :: Type
    }
    | TTuple [Type]
    -- Arguments to result type
    | TFunction [Type] Type
    -- TDotable a b means that this type can be dotted
    -- with an a to yield something of type b
    | TDotable Type Type
    | TDatatype Name
    deriving (Eq, Ord, Show)

data TypeVarRef = 
    TypeVarRef {
        typeVar :: TypeVar,
        constraints :: [Constraint],
        typePointer :: PType
    }
    | RigidTypeVarRef {
        typeVar :: TypeVar,
        constraints :: [Constraint],
        rigidName :: Name
    }

instance Eq TypeVarRef where
    tvref1 == tvref2 = typeVar tvref1 == typeVar tvref2
instance Ord TypeVarRef where
    compare tvref1 tvref2 = compare (typeVar tvref1) (typeVar tvref2)
instance Show TypeVarRef where
    show (TypeVarRef tv cs _) = "TypeVarRef "++show tv++" "++show cs
    show (RigidTypeVarRef tv cs n) =
        "RigidTypeVarRef "++show tv ++" "++show cs++" "++show n

isRigid :: TypeVarRef -> Bool
isRigid (RigidTypeVarRef _ _ _) = True
isRigid _ = False 

typeVarSupply :: IORef (Supply Int)
typeVarSupply = unsafePerformIO (do
    s <- newNumSupply
    newIORef s)
{-# NOINLINE typeVarSupply #-}

takeTypeVarFromSupply :: MonadIO m => m TypeVar
takeTypeVarFromSupply = do
    s <- liftIO $ atomicModifyIORef typeVarSupply split2
    return $ TypeVar $ supplyValue s

freshTypeVar :: MonadIO m => m Type
freshTypeVar = freshTypeVarWithConstraints []

freshTypeVarRef :: MonadIO m => [Constraint] -> m TypeVarRef
freshTypeVarRef cs = do
    tv <- takeTypeVarFromSupply
    ioRef <- freshPType
    return $ TypeVarRef tv cs ioRef

freshTypeVarWithConstraints :: MonadIO m => [Constraint] -> m Type
freshTypeVarWithConstraints cs = freshTypeVarRef cs >>= return . TVar

freshRigidTypeVarWithConstraints :: MonadIO m => Name -> [Constraint] -> m Type
freshRigidTypeVarWithConstraints n cs = do
    tv <- takeTypeVarFromSupply
    return $ TVar (RigidTypeVarRef tv cs n)

type SymbolTable = PartialFunction Name TypeScheme
type PType = IORef (Maybe Type)
type PSymbolTable = IORef SymbolTable

readPType :: (MonadIO m) => PType -> m (Maybe Type)
readPType ioref = liftIO $ readIORef ioref

setPType :: (MonadIO m) => PType -> Type -> m ()
setPType ioref t = liftIO $ writeIORef ioref (Just t)

freshPType :: (MonadIO m) => m PType
freshPType = liftIO $ newIORef Nothing

readPSymbolTable :: (MonadIO m) => PSymbolTable -> m SymbolTable
readPSymbolTable ioref = liftIO $ readIORef ioref

setPSymbolTable :: (MonadIO m) => PSymbolTable -> SymbolTable -> m ()
setPSymbolTable ioref t = liftIO $ writeIORef ioref t

freshPSymbolTable :: (MonadIO m) => m PSymbolTable
freshPSymbolTable = liftIO $ newIORef []

isPolymorphic :: Type -> Bool
isPolymorphic (TVar tvref) = True
isPolymorphic (TFunction targs tr) = or (map isPolymorphic (tr:targs))
isPolymorphic (TSeq t) = isPolymorphic t
isPolymorphic (TSet t) = isPolymorphic t
isPolymorphic (TTuple ts) = or (map isPolymorphic ts)
isPolymorphic (TDot t1 t2) = isPolymorphic t1 || isPolymorphic t2
isPolymorphic (TDotable t1 t2) = isPolymorphic t1 || isPolymorphic t2
isPolymorphic (TMap k v) = isPolymorphic k || isPolymorphic v
isPolymorphic (TExtendable t tvref) = isPolymorphic t
isPolymorphic TProc = False
isPolymorphic TInt = False
isPolymorphic TBool = False
isPolymorphic TChar = False
isPolymorphic TEvent = False
isPolymorphic TExtendableEmptyDotList = False
isPolymorphic (TDatatype _) = False

instance PrettyPrintable Constraint where
    prettyPrint CEq = text "Eq"
    prettyPrint COrd = text "Ord"
    prettyPrint CInputable = text "Inputable"
    prettyPrint CSet = text "Set"
    prettyPrint CYieldable = text "Yieldable"

-- | Pretty prints several types using the same variable substitutions
prettyPrintTypes :: [Type] -> [Doc]
prettyPrintTypes ts = fst (prettyPrintTypesWithMap Mp.empty ts)

prettyPrintTypesWithMap :: VariableRepresentationMap -> [Type] ->
    ([Doc], VariableRepresentationMap)
prettyPrintTypesWithMap vmap ts =
        (map (flip runReader vmap' . M.prettyPrint) ts, vmap')
    where
        vs = (nub . map fst . concatMap collectConstraints) ts
        -- | Map from int to letter to improve presentation
        vmap' = variableMapForTypeVars vmap vs

prettyPrintTypeSchemes :: [TypeScheme] -> [Doc]
prettyPrintTypeSchemes ts = fst (prettyPrintTypeSchemesWithMap Mp.empty ts)

-- | Pretty prints the type-schemes, using the map to pretty-print variable
-- names. If any new variable names are found, they are added to a new
-- variable map, which is also returned.
prettyPrintTypeSchemesWithMap :: VariableRepresentationMap -> [TypeScheme] ->
    ([Doc], VariableRepresentationMap)
prettyPrintTypeSchemesWithMap vmap ts =
        (map (flip runReader vmap' . M.prettyPrint) ts, vmap')
    where
        vs = (nub . map fst . concatMap (collectConstraints . typeSchemeType)) ts
        -- | Map from int to letter to improve presentation
        vmap' = variableMapForTypeVars vmap vs

-- | Given a type scheme, adds any extra constraints it finds in the type
-- variables contained with the type-scheem.
augamentTypeScheme :: TypeScheme -> TypeScheme
augamentTypeScheme (ForAll cs t) = ForAll (nub $ sort $ cs ++ cs') t
    where cs' = [(typeVar tv, c) | (tv, c) <- collectConstraints t]

type VariableRepresentationMap = Mp.Map TypeVar Doc

variableMapForTypeVars :: VariableRepresentationMap -> [TypeVarRef] ->
    VariableRepresentationMap
variableMapForTypeVars existingMap tvs = 
    let
        (rigid, nonRigid) = partition isRigid $
            filter (\ tv -> not (Mp.member (typeVar tv) existingMap)) tvs

        extract (RigidTypeVarRef _ _ n) = [show n]
        usedCharacterStrings =
            map (show . snd) (Mp.toList existingMap) ++ concatMap extract rigid

        gen :: Int -> [Char] -> [Char] -> [Doc]
        gen n xs [] = gen (n+1) xs xs
        gen 1 xs (y:ys) = char y : gen 1 xs ys
        gen n xs (y:ys) = text (y:show (n-1)) : gen n xs ys

        notUsed xs = not (show xs `elem` usedCharacterStrings)
        initialGen = if length rigid > 0 then 1 else 0
        availableStrings = filter notUsed (gen initialGen ['a'..'z'] [])

        vs = map (\ (RigidTypeVarRef tv _ n) -> (tv, prettyPrint n)) rigid
            ++ zip (map typeVar nonRigid) availableStrings
    in Mp.union existingMap (Mp.fromList vs)

instance PrettyPrintable Type where
    prettyPrint t = prettyPrint (ForAll ts t)
        where ts = [(typeVar tv, cs) | (tv, cs) <- collectConstraints t]

instance Precedence Type where
    precedence (TDotable _ _) = 2
    precedence (TExtendable _ _) = 2
    precedence (TDot _ _) = 1
    precedence (TMap _ _) = 1

    precedence (TVar _) = 0
    precedence (TFunction _ _) = 0
    precedence (TSeq _) = 0
    precedence (TSet _) = 0
    precedence (TTuple _) = 0
    precedence (TDatatype _) = 0
    precedence TBool = 0
    precedence TInt = 0
    precedence TProc = 0
    precedence TEvent = 0
    precedence TChar = 0

    associativity (TDotable _ _) = AssocRight
    associativity (TExtendable _ _) = AssocRight
    associativity (TDot _ _) = AssocLeft
    associativity _ = AssocNone

    sameOperator (TDotable _ _) (TDotable _ _) = True
    sameOperator (TExtendable _ _) (TExtendable _ _) = True
    sameOperator (TDot _ _) (TDot _ _) = True
    sameOperator (TMap _ _) (TMap _ _) = True
    sameOperator (TVar _) (TVar _) = True
    sameOperator (TFunction _ _) (TFunction _ _) = True
    sameOperator (TSeq _) (TSeq _) = True
    sameOperator (TSet _) (TSet _) = True
    sameOperator (TTuple _) (TTuple _) = True
    sameOperator (TDatatype _) (TDatatype _) = True
    sameOperator TBool TBool = True
    sameOperator TInt TInt = True
    sameOperator TProc TProc = True
    sameOperator TEvent TEvent = True
    sameOperator TChar TChar = True
    sameOperator _ _ = False

instance M.MonadicPrettyPrintable (Reader VariableRepresentationMap) Type where
    prettyPrint (TVar tvref) = do
        st <- ask
        return $! case Mp.lookup (typeVar tvref) st of
            Just c  -> c
            Nothing -> prettyPrint (typeVar tvref)
    prettyPrint (TFunction targs tr) =
        M.parens (M.list (mapM M.prettyPrint targs))
        M.<+> M.text "->" M.<+> M.prettyPrint tr
    prettyPrint (TSeq t) =
        M.char '<' M.<> M.prettyPrint t M.<> M.char '>'
    prettyPrint (TSet t) =
        M.braces (M.prettyPrint t)
    prettyPrint (TTuple ts) =
        M.parens (M.list (mapM M.prettyPrint ts))
    prettyPrint (TMap k v) =
        M.text "(|" M.<+> M.prettyPrint k M.<+> M.text "=>"
        M.<+> M.prettyPrint v M.<+> M.text "|)"
    prettyPrint (op@(TDot t1 t2)) =
        M.ppBinaryOp' op (M.char '.') t1 t2
    prettyPrint (op@(TDotable t1 t2)) = M.ppBinaryOp' op (M.text "=>") t1 t2
    prettyPrint (TDatatype n) = M.prettyPrint n
    prettyPrint TBool = M.text "Bool"
    prettyPrint TInt = M.text "Int"
    prettyPrint TProc = M.text "Proc"
    prettyPrint TEvent = M.text "Event"
    prettyPrint TChar = M.text "Char"
    prettyPrint (op@(TExtendable t tvref)) =
        M.ppBinaryOp' op (M.text "=>*") (TVar tvref) t

instance M.MonadicPrettyPrintable (Reader VariableRepresentationMap) TypeScheme where
    prettyPrint (ForAll ts t) = do
        vmap <- ask
        let
            -- | Vars with constraints
            varsWithCs = [(v, c) | (v, cs) <- nub (sortConstraints ts),
                                    c <- reduceConstraints cs, cs /= []]
            compareConstraints (tv1, cs1) (tv2, cs2) =
                compare (show $ Mp.lookup tv1 vmap) (show $ Mp.lookup tv2 vmap)
                `thenCmp` compare cs1 cs2
            sortConstraints = sortBy compareConstraints
            constraintsText = M.list $ mapM (\ (tv, c) ->
                    return (prettyPrint c) M.<+>
                        case Mp.lookup tv vmap of
                            Just x -> return x
                            Nothing -> panic "Could not pretty print type"
                    ) varsWithCs
        M.sep $ sequence [
            case varsWithCs of
                [] -> M.empty
                [x] -> constraintsText M.<+> M.text "=>"
                _ -> M.parens constraintsText M.<+> M.text "=>",
            M.prettyPrint t]

instance PrettyPrintable TypeScheme where
    prettyPrint ts = head $ prettyPrintTypeSchemes [ts]

collectConstraints :: Type -> [(TypeVarRef, [Constraint])]
collectConstraints = combine . collect
    where
        combine :: [(TypeVarRef, [Constraint])] -> [(TypeVarRef, [Constraint])]
        combine xs = 
            map (\ ys -> (head (map fst ys), nub (concat (map snd ys))))
                (groupBy (\ (v1, _) (v2, _) -> v1 == v2) xs)

        collect :: Type -> [(TypeVarRef, [Constraint])]
        collect (TVar tvref) = [(tvref, constraints tvref)]
        collect (TFunction targs tr) = 
            concatMap collect targs ++ collect tr
        collect (TSeq t) = collect t
        collect (TSet t) = collect t
        collect (TTuple ts) = concatMap collect ts
        collect (TDot t1 t2) = collect t1 ++ collect t2
        collect (TDotable t1 t2) = collect t1 ++ collect t2
        collect (TMap k v) = collect k ++ collect v
        collect (TDatatype _) = []
        collect TBool = []
        collect TInt = []
        collect TProc = []
        collect TEvent = []
        collect TChar = []
        collect (TExtendable t tvref) =
            (tvref, constraints tvref) : collect t
