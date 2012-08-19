module CSPM.Evaluator.Values (
    Value(..), UProc, Proc(..), CSPOperator(..), ProcOperator(..), Event(..),
    ScopeIdentifier(..), FunctionIdentifier(..),
    compareValues,
    procName, scopeId, annonymousScopeId,
    valueEventToEvent,
    noSave, maybeSave, removeThunk, lookupVar,
    tupleFromList,
    trimValueForProcessName,
    module Data.Array,
) where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.Evaluator.Monad
import CSPM.Evaluator.ProcessValues
import {-# SOURCE #-} qualified CSPM.Evaluator.ValueSet as S
import Data.Array
import qualified Data.Foldable as F
import Data.Hashable
import Prelude hiding (lookup)
import Util.Exception
import Util.Prelude

type UProc = UnCompiledProc

data Value =
    VInt Int
    | VBool Bool
    | VTuple (Array Int Value)
    -- | If A is a datatype clause that has 3 fields a b c then a runtime
    -- instantiation of this would be VDot [VDataType "A", a, b, c] where a,b
    -- and c can contain other VDots.
    | VDot [Value]
    -- The following two never appear on their own, they are always part of a 
    -- VDot (even if the VDot has no values).
    | VChannel Name
    | VDataType Name
    | VList [Value]
    | VSet S.ValueSet
    | VFunction FunctionIdentifier ([Value] -> EvaluationMonad Value)
    | VProc UProc
    | VThunk (EvaluationMonad Value)

-- | A disambiguator between different occurences of either processes or
-- functions. This works by storing the values that are bound (i.e. the free
-- variables the inner `thing` may depend on). This is used as a 'ProcName' and
-- for 'FunctionIdentifier's.
data ScopeIdentifier =
    SFunctionBind {
        scopeFunctionName :: Name,
        scopeFunctionArguments :: [[Value]],
        parentScopeIdentifier :: Maybe ScopeIdentifier
    }
    | SVariableBind {
        variablesBound :: [Value],
        parentScopeIdentifier :: Maybe ScopeIdentifier
    }

instance Eq ScopeIdentifier where
    SFunctionBind n1 vss1 p1 == SFunctionBind n2 vss2 p2 =
        n1 == n2 && vss1 == vss2 && p1 == p2
    SVariableBind vs1 p1 == SVariableBind vs2 p2 =
        vs1 == vs2 && p1 == p2

instance Hashable ScopeIdentifier where
    hash (SFunctionBind n1 args1 p1) =
        combine 1 (combine (hash n1) (combine (hash args1) (hash p1)))
    hash (SVariableBind vs1 p1) = combine 2 (combine (hash vs1) (hash p1))

instance Ord ScopeIdentifier where
    compare (SFunctionBind n1 vss1 p1) (SFunctionBind n2 vss2 p2) =
        compare n1 n2 `thenCmp` compare vss1 vss2 `thenCmp` compare p1 p2
    compare (SFunctionBind _ _ _) _ = LT
    compare _ (SFunctionBind _ _ _) = GT
    compare (SVariableBind vs1 p1) (SVariableBind vs2 p2) =
        compare vs1 vs2 `thenCmp` compare p1 p2

data FunctionIdentifier = 
    FBuiltInFunction {
        functionName :: Name,
        arguments :: [Value]
    }
    | FLambda {
        lambdaExpression :: Exp Name,
        parentFunctionIdentifier :: Maybe ScopeIdentifier
    }
    | FMatchBind {
        functionName :: Name,
        argumentGroups :: [[Value]],
        -- | The free variables this is bound in
        scopeIdentifier :: Maybe ScopeIdentifier
    }

instance Eq FunctionIdentifier where
    FBuiltInFunction n1 vs1 == FBuiltInFunction n2 vs2 =
        n1 == n2 && vs1 == vs2
    FLambda e1 parent1 == FLambda e2 parent2 =
        e1 == e2 && parent1 == parent2
    FMatchBind n1 args1 parent1 == FMatchBind n2 args2 parent2 =
        n1 == n2 && args1 == args2 && parent1 == parent2
    _ == _ = False

instance Hashable FunctionIdentifier where
    hash (FBuiltInFunction n1 vs) = combine 2 (combine (hash n1) (hash vs))
    hash (FLambda expr parent) =
        combine 3 (combine (hash (show expr)) (hash parent))
    hash (FMatchBind n vs parent) =
        combine 4 (combine (hash n) (combine (hash vs) (hash parent)))

instance Ord FunctionIdentifier where
    compare (FBuiltInFunction n1 args1) (FBuiltInFunction n2 args2) =
        compare n1 n2 `thenCmp` compare args1 args2
    compare (FBuiltInFunction _ _) _ = LT
    compare _ (FBuiltInFunction _ _) = GT
    compare (FLambda e1 parent1) (FLambda e2 parent2) =
        compare parent1 parent2 `thenCmp` compare e1 e2
    compare (FLambda _ _) _ = LT
    compare _ (FLambda _ _) = GT
    compare (FMatchBind n1 vs1 parent1) (FMatchBind n2 vs2 parent2) =
        compare n1 n2 `thenCmp` compare parent1 parent2 `thenCmp` compare vs1 vs2

tupleFromList :: [Value] -> Value
tupleFromList vs = VTuple $! listArray (0, length vs - 1) vs

-- | Given a program that yields a value, returns a second program that can be
-- inserted into the environment, but will cause the environment not to save
-- the actual value, but to recompute it everytime. This is useful for cheap,
-- to compute, but high cost in terms of memory, computations (like named
-- processes).
noSave :: EvaluationMonad Value -> EvaluationMonad Value
noSave prog = do
    pn <- getParentScopeIdentifier
    return $ VThunk $ case pn of
                        Just x -> updateParentScopeIdentifier x prog
                        Nothing -> prog

maybeSave :: Type -> EvaluationMonad Value -> EvaluationMonad Value
maybeSave TProc prog = noSave prog
maybeSave _ prog = prog

removeThunk :: Value -> EvaluationMonad Value
removeThunk (VThunk p) = p
removeThunk v = return v

lookupVar :: Name -> EvaluationMonad Value
lookupVar n = lookupVarMaybeThunk n >>= removeThunk

instance (Ix i, Hashable a) => Hashable (Array i a) where
    hash arr = F.foldr combine 0 (fmap hash arr)

instance Hashable Value where
    hash (VInt i) = combine 1 (hash i)
    hash (VBool b) = combine 2 (hash b)
    hash (VTuple vs) = combine 5 (hash vs)
    hash (VDot vs) = combine 6 (hash vs)
    hash (VChannel n) = combine 7 (hash n)
    hash (VDataType n) = combine 8 (hash n)
    hash (VList vs) = combine 9 (hash vs)
    hash (VSet vset) = combine 10 (hash vset)
    -- We identify all functions (for process names) - see comment below in Eq.
    hash (VFunction id _) = combine 11 (hash id)
    hash (VProc p) = combine 12 (hash p)

instance Eq Value where
    VInt i1 == VInt i2 = i1 == i2
    VBool b1 == VBool b2 = b1 == b2
    VTuple vs1 == VTuple vs2 = vs1 == vs2
    VDot vs1 == VDot vs2 = vs1 == vs2
    VChannel n1 == VChannel n2 = n1 == n2
    VDataType n1 == VDataType n2 = n1 == n2
    VList vs1 == VList vs2 = vs1 == vs2
    VSet s1 == VSet s2 = s1 == s2
    VProc p1 == VProc p2 = p1 == p2
    VFunction id1 _ == VFunction id2 _ = id1 == id2
    
    v1 == v2 = False
    
-- | Implements CSPM comparisons (note that Ord Value does not).
compareValues :: Value -> Value -> Maybe Ordering
-- The following are all orderable and comparable
compareValues (VInt i1) (VInt i2) = Just (compare i1 i2)
compareValues (VBool b1) (VBool b2) = Just (compare b1 b2)
compareValues (VTuple vs1) (VTuple vs2) =
    -- Tuples must be same length by type checking
    -- Tuples are ordered lexiographically
    let
        (l, u) = bounds vs1
        cmp ix | ix > u = EQ
        cmp ix = compare (vs1!ix) (vs2!ix) `thenCmp` cmp (ix+1)
    in Just (cmp 0)
compareValues (VList vs1) (VList vs2) =
    let
        -- for lists comparing means comparing prefixes
        cmp [] [] = Just EQ
        cmp [] (y:ys) = Just LT
        cmp (x:xs) [] = Just GT
        cmp (x:xs) (y:ys) | x == y = cmp xs ys
        cmp (x:xs) (y:ys) = 
            -- x != y, hence neither can be a prefix of the other
            Nothing
    in cmp vs1 vs2
compareValues (VSet s1) (VSet s2) = S.compareValueSets s1 s2

-- The following can only be compared for equality, hence if they are not
-- equal we return Nothing.
compareValues (VChannel n1) (VChannel n2) = 
    if n1 == n2 then Just EQ else Nothing
compareValues (VDataType n1) (VDataType n2) = 
    if n1 == n2 then Just EQ else Nothing
compareValues (VDot vs1) (VDot vs2) =
    if vs1 == vs2 then Just EQ else Nothing

-- Every other possibility is invalid
compareValues v1 v2 = panic $ "Cannot compare two values"

instance Ord Value where
    -- This implementation is used for various internal measures, but not
    -- for implementing actual comparisons in CSPM.
    compare (VInt i1) (VInt i2) = compare i1 i2
    compare (VBool b1) (VBool b2) = compare b1 b2
    compare (VTuple vs1) (VTuple vs2) = compare vs1 vs2
    compare (VList vs1) (VList vs2) = compare vs1 vs2
    compare (VSet s1) (VSet s2) = compare s1 s2    
    -- These are only ever used for the internal set implementation
    compare (VDot vs1) (VDot vs2) = compare vs1 vs2
    compare (VChannel n) (VChannel n') = compare n n'
    compare (VDataType n) (VDataType n') = compare n n'
    compare (VProc p1) (VProc p2) = compare p1 p2
    compare (VFunction id1 _) (VFunction id2 _) = compare id1 id2

    compare v1 v2 = panic $
        -- Must be as a result of a mixed set of values, which cannot happen
        -- as a result of type checking.
        "Internal sets - cannot order "
      
procName :: ScopeIdentifier -> ProcName
procName = ProcName

scopeId :: Name -> [[Value]] -> Maybe ScopeIdentifier -> ScopeIdentifier
scopeId n vss pn = SFunctionBind n (map (map trimValueForProcessName) vss) pn

annonymousScopeId :: [Value] -> Maybe ScopeIdentifier -> ScopeIdentifier
annonymousScopeId vss pn = SVariableBind (map trimValueForProcessName vss) pn

-- | This assumes that the value is a VDot with the left is a VChannel
valueEventToEvent :: Value -> Event
valueEventToEvent = UserEvent

errorThunk = panic "Trimmed value function evaluated"

trimFunctionIdentifier :: FunctionIdentifier -> FunctionIdentifier
trimFunctionIdentifier (FBuiltInFunction n args) =
    FBuiltInFunction n (map trimValueForProcessName args)
trimFunctionIdentifier (FLambda e p) = FLambda e p
trimFunctionIdentifier (FMatchBind n args p) =
    FMatchBind n (map (map trimValueForProcessName) args) p

trimValueForProcessName :: Value -> Value
trimValueForProcessName (VInt i) = VInt i
trimValueForProcessName (VBool b) = VBool b
trimValueForProcessName (VTuple vs) = VTuple (fmap trimValueForProcessName vs)
trimValueForProcessName (VList vs) = VList (map trimValueForProcessName vs)
trimValueForProcessName (VSet s) =
    VSet $ S.fromList $ map trimValueForProcessName $ S.toList s
trimValueForProcessName (VDot vs) = VDot $ map trimValueForProcessName vs
trimValueForProcessName (VChannel n) = VChannel n
trimValueForProcessName (VDataType n) = VDataType n
trimValueForProcessName (VFunction id _) =
    VFunction (trimFunctionIdentifier id) errorThunk
trimValueForProcessName (VProc p) = VProc (trimProcess p)
