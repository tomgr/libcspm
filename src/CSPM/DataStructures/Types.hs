module CSPM.DataStructures.Types (
    -- * Data Structures
    TypeVar, TypeScheme(..), Constraint(..), Type(..), TypeVarRef(..),
    prettyPrintTypes,
    constraintImpliedBy, reduceConstraints,
    -- * Creation of Types
    freshTypeVar, freshTypeVarWithConstraints,

    -- * Symbol Tables
    SymbolTable, PSymbolTable, freshPSymbolTable, readPSymbolTable, 
    setPSymbolTable,

    -- * Type Pointers
    PType, freshPType, readPType, setPType,
) where

import Control.Monad.Trans
import Data.IORef
import Data.List
import Data.Supply
import System.IO.Unsafe

import CSPM.DataStructures.Names
import Util.PartialFunctions
import Util.PrettyPrint

-- *************************************************************************
-- Types
-- *************************************************************************
newtype TypeVar = TypeVar Int deriving (Eq, Show)

data TypeScheme =
    ForAll [(TypeVar, [Constraint])] Type
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
    | TEvent
    -- Something that can be extended to the given type
    | TExtendable Type
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

instance Eq TypeVarRef where
    (TypeVarRef tv1 _ _) == (TypeVarRef tv2 _ _) = tv1 == tv2

instance Show TypeVarRef where
    show (TypeVarRef tv cs _) = "TypeVarRef "++show tv ++ show cs

typeVarSupply :: IORef (Supply Int)
typeVarSupply = unsafePerformIO (do
    s <- newNumSupply
    newIORef s)

takeTypeVarFromSupply :: MonadIO m => m TypeVar
takeTypeVarFromSupply = do
    s <- liftIO $ readIORef typeVarSupply
    let (s1, s2) = split2 s
    liftIO $ writeIORef typeVarSupply s2
    return $ TypeVar $ supplyValue s1

freshTypeVar :: MonadIO m => m Type
freshTypeVar = freshTypeVarWithConstraints []

freshTypeVarWithConstraints :: MonadIO m => [Constraint] -> m Type
freshTypeVarWithConstraints cs = do
    tv <- takeTypeVarFromSupply
    ioRef <- freshPType
    return $ TVar (TypeVarRef tv cs ioRef)

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

instance PrettyPrintable Constraint where
    prettyPrint CEq = text "Eq"
    prettyPrint COrd = text "Ord"
    prettyPrint CInputable = text "Inputable"
    prettyPrint CSet = text "Set"

-- | Pretty prints several types using the same variable substitutions
prettyPrintTypes :: [Type] -> [Doc]
prettyPrintTypes ts = map (prettyPrintType vmap) ts
    where
        vs = (nub . map fst . concatMap collectConstraints) ts
        -- | Map from int to letter to improve presentation
        vmap = zip (map (\ (TypeVar n) -> n) vs) ['a'..'z']

instance PrettyPrintable Type where
    prettyPrint t = prettyPrint (ForAll (collectConstraints t) t)

instance PrettyPrintable TypeScheme where
    prettyPrint (ForAll ts t) =
        (if length varsWithCs > 0 then 
            (if length varsWithCs > 1 then parens constraintsText 
            else constraintsText)
            <+> text "=> "
        else empty)
        <> prettyPrintType vmap t
        where
            -- | Map from int to letter to improve presentation
            vmap = zip (map (\ (TypeVar n, _) -> n) ts) ['a'..'z']
            -- | Vars with constraints
            varsWithCs = [(v, c) | (v, cs) <- ts, c <- reduceConstraints cs, cs /= []]

            constraintsText = 
                hsep (
                    punctuate comma [
                        prettyPrint c <+> char (apply vmap n)
                     | (TypeVar n, c) <- varsWithCs]
                )

prettyPrintType :: PartialFunction Int Char -> Type -> Doc
prettyPrintType vmap (TVar (TypeVarRef (TypeVar n) _ _)) = 
    case safeApply vmap n of
        Just c  -> char c
        Nothing -> int n
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
        _               -> prettyPrintType vmap t1
    ) <> text "." <> prettyPrintType vmap t2
prettyPrintType vmap (TDotable t1 t2) =
    prettyPrintType vmap t1 <> text "=>" <> prettyPrintType vmap t2
prettyPrintType _ (TDatatype n) = prettyPrint n
prettyPrintType _ TBool = text "Bool"
prettyPrintType _ TInt = text "Int"
prettyPrintType _ TProc = text "Proc"
prettyPrintType _ TEvent = text "Event"
prettyPrintType vmap (TExtendable t) = text "Extenable to"<+>prettyPrintType vmap t

collectConstraints :: Type -> [(TypeVar, [Constraint])]
collectConstraints = combine . collect
    where
        combine :: [(TypeVar, [Constraint])] -> [(TypeVar, [Constraint])]
        combine xs = 
            map (\ ys -> (head (map fst ys), nub (concat (map snd ys))))
                (groupBy (\ (v1, _) (v2, _) -> v1 == v2) xs)
        collect :: Type -> [(TypeVar, [Constraint])]
        collect (TVar (TypeVarRef v cs _)) = [(v, cs)]
        collect (TFunction targs tr) = 
            concatMap collect targs ++ collect tr
        collect (TSeq t) = collect t
        collect (TSet t) = collect t
        collect (TTuple ts) = concatMap collect ts
        collect (TDot t1 t2) = collect t1 ++ collect t2
        collect (TDotable t1 t2) = collect t1 ++ collect t2
        collect (TDatatype _) = []
        collect TBool = []
        collect TInt = []
        collect TProc = []
        collect TEvent = []
        collect (TExtendable t) = collect t
