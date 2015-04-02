module CSPM.Evaluator.Values (
    Value(..), UProc, UProcOperator, Proc(..), CSPOperator(..),
    ProcOperator(..), Event(..), EventSet,
    ScopeIdentifier(..), FunctionIdentifier(..),
    compareValues,
    procName, scopeId, annonymousScopeId,
    builtInFunction, lambdaFunction, matchBindFunction,
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
import qualified Data.Map as M
import Prelude hiding (lookup)
import Util.Annotated
import Util.Exception
import Util.Prelude

type UProc = UnCompiledProc
type UProcOperator = UnCompiledProcOperator

data Value =
    VInt Int
    | VChar Char
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
    | VMap (M.Map Value Value)
    | VSet S.ValueSet
    | VFunction FunctionIdentifier ([Value] -> EvaluationMonad Value)
    | VProc UProc
    | VLoc SrcSpan
    | VThunk (EvaluationMonad Value)

-- | A disambiguator between different occurences of either processes or
-- functions. This works by storing the values that are bound (i.e. the free
-- variables the inner `thing` may depend on). This is used as a 'ProcName' and
-- for 'FunctionIdentifier's.
data ScopeIdentifier =
    SFunctionBind {
        scopeIdentifierCachedHash :: Int,
        scopeFunctionName :: Name,
        scopeFunctionArguments :: [[Value]],
        parentScopeIdentifier :: Maybe ScopeIdentifier
    }
    | SVariableBind {
        scopeIdentifierCachedHash :: Int,
        variablesBound :: [Value],
        parentScopeIdentifier :: Maybe ScopeIdentifier
    }

instance Eq ScopeIdentifier where
    SFunctionBind h1 n1 vss1 p1 == SFunctionBind h2 n2 vss2 p2 =
        h1 == h2 && n1 == n2 && vss1 == vss2 && p1 == p2
    SVariableBind h1 vs1 p1 == SVariableBind h2 vs2 p2 =
        h1 == h2 && vs1 == vs2 && p1 == p2

instance Hashable ScopeIdentifier where
    hashWithSalt s = hashWithSalt s . scopeIdentifierCachedHash

instance Ord ScopeIdentifier where
    compare (SFunctionBind h1 n1 vss1 p1) (SFunctionBind h2 n2 vss2 p2) =
        compare n1 n2 `thenCmp` compare vss1 vss2 `thenCmp` compare p1 p2
    compare (SFunctionBind _ _ _ _) _ = LT
    compare _ (SFunctionBind _ _ _ _) = GT
    compare (SVariableBind h1 vs1 p1) (SVariableBind h2 vs2 p2) =
        compare vs1 vs2 `thenCmp` compare p1 p2

data FunctionIdentifier = 
    FBuiltInFunction {
        functionIdentifierCachedHash :: Int,
        functionName :: Name,
        argumentGroups :: [[Value]]
    }
    | FLambda {
        functionIdentifierCachedHash :: Int,
        lambdaExpression :: Exp Name,
        parentFunctionIdentifier :: Maybe ScopeIdentifier
    }
    | FMatchBind {
        functionIdentifierCachedHash :: Int,
        functionName :: Name,
        argumentGroups :: [[Value]],
        -- | The free variables this is bound in
        scopeIdentifier :: Maybe ScopeIdentifier
    }

builtInFunction :: Name -> [[Value]] -> FunctionIdentifier
builtInFunction n vs = FBuiltInFunction h n vs
    where h = (2 :: Int) `hashWithSalt` n `hashWithSalt` vs

lambdaFunction :: Exp Name -> Maybe ScopeIdentifier -> FunctionIdentifier
lambdaFunction expr parent = FLambda h expr parent
    where h = (3 :: Int) `hashWithSalt` (show expr) `hashWithSalt` parent

matchBindFunction :: Name -> [[Value]] -> Maybe ScopeIdentifier -> FunctionIdentifier
matchBindFunction n vs parent = FMatchBind h n vs parent
    where h = (4 :: Int) `hashWithSalt` n `hashWithSalt` vs `hashWithSalt` parent

instance Eq FunctionIdentifier where
    FBuiltInFunction h1 n1 vs1 == FBuiltInFunction h2 n2 vs2 =
        h1 == h2 && n1 == n2 && vs1 == vs2
    FLambda h1 e1 parent1 == FLambda h2 e2 parent2 =
        h1 == h2 && e1 == e2 && parent1 == parent2
    FMatchBind h1 n1 args1 parent1 == FMatchBind h2 n2 args2 parent2 =
        h1 == h2 && n1 == n2 && args1 == args2 && parent1 == parent2
    _ == _ = False

instance Hashable FunctionIdentifier where
    hashWithSalt s = hashWithSalt s . functionIdentifierCachedHash

instance Ord FunctionIdentifier where
    compare (FBuiltInFunction h1 n1 args1) (FBuiltInFunction h2 n2 args2) =
        compare n1 n2 `thenCmp` compare args1 args2
    compare (FBuiltInFunction _ _ _) _ = LT
    compare _ (FBuiltInFunction _ _ _) = GT
    compare (FLambda h1 e1 parent1) (FLambda h2 e2 parent2) =
        compare parent1 parent2 `thenCmp` compare e1 e2
    compare (FLambda _ _ _) _ = LT
    compare _ (FLambda _ _ _) = GT
    compare (FMatchBind h1 n1 vs1 parent1) (FMatchBind h2 n2 vs2 parent2) =
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
    return $ VThunk $ modify (\ st -> st {
            CSPM.Evaluator.Monad.parentScopeIdentifier = pn
        }) prog

maybeSave :: Type -> EvaluationMonad Value -> EvaluationMonad Value
maybeSave TProc prog = noSave prog
maybeSave _ prog = prog

removeThunk :: Value -> EvaluationMonad Value
removeThunk (VThunk p) = p
removeThunk v = return v

lookupVar :: Name -> EvaluationMonad Value
lookupVar n = lookupVarMaybeThunk n >>= removeThunk

instance (Ix i, Hashable a) => Hashable (Array i a) where
    hashWithSalt s arr = F.foldr hashWithSalt s (fmap hash arr)

instance Hashable Value where
    hashWithSalt s (VInt i) = s `hashWithSalt` (1 :: Int) `hashWithSalt` i
    hashWithSalt s (VBool b) = s `hashWithSalt` (2 :: Int) `hashWithSalt` b
    hashWithSalt s (VChar c) = s `hashWithSalt` (3 :: Int) `hashWithSalt` c
    hashWithSalt s (VTuple vs) = s `hashWithSalt` (5 :: Int) `hashWithSalt` vs
    hashWithSalt s (VDot vs) = s `hashWithSalt` (6 :: Int) `hashWithSalt` vs
    hashWithSalt s (VChannel n) = s `hashWithSalt` (7 :: Int) `hashWithSalt` n
    hashWithSalt s (VDataType n) = s `hashWithSalt` (8 :: Int) `hashWithSalt` n
    hashWithSalt s (VList vs) = s `hashWithSalt` (9 :: Int) `hashWithSalt` vs
    hashWithSalt s (VSet vset) = s `hashWithSalt` (10 :: Int) `hashWithSalt` vset
    -- We identify all functions (for process names) - see comment below in Eq.
    hashWithSalt s (VFunction id _) = s `hashWithSalt` (11 :: Int) `hashWithSalt` id
    hashWithSalt s (VProc p) = s `hashWithSalt` (12 :: Int) `hashWithSalt` p
    hashWithSalt s (VMap m) = s `hashWithSalt` (13 :: Int) `hashWithSalt` (M.toList m)
    hashWithSalt s (VLoc l) = s `hashWithSalt` (14 :: Int) `hashWithSalt` l

instance Eq Value where
    VInt i1 == VInt i2 = i1 == i2
    VBool b1 == VBool b2 = b1 == b2
    VChar c1 == VChar c2 = c1 == c2
    VTuple vs1 == VTuple vs2 = vs1 == vs2
    VDot vs1 == VDot vs2 = vs1 == vs2
    VChannel n1 == VChannel n2 = n1 == n2
    VDataType n1 == VDataType n2 = n1 == n2
    VList vs1 == VList vs2 = vs1 == vs2
    VSet s1 == VSet s2 = s1 == s2
    VProc p1 == VProc p2 = p1 == p2
    VFunction id1 _ == VFunction id2 _ = id1 == id2
    VMap m1 == VMap m2 = m1 == m2
    VLoc l1 == VLoc l2 = l1 == l2
    
    v1 == v2 = False
    
-- | Implements CSPM comparisons (note that Ord Value does not).
compareValues :: Value -> Value -> Maybe Ordering
-- The following are all orderable and comparable
compareValues (VInt i1) (VInt i2) = Just (compare i1 i2)
compareValues (VChar c1) (VChar c2) = Just (compare c1 c2)
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
compareValues (VMap m1) (VMap m2) =
    let cmp v1 v2 =
            case compareValues v1 v2 of
                Just LT -> True
                Just EQ -> True
                _ -> False
    in if m1 == m2 then Just EQ
    else if M.isSubmapOfBy cmp m1 m2 then Just LT
    else if M.isSubmapOfBy cmp m2 m1 then Just GT
    else Nothing

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
    compare (VChar c1) (VChar c2) = compare c1 c2
    compare (VBool b1) (VBool b2) = compare b1 b2
    compare (VTuple vs1) (VTuple vs2) = compare vs1 vs2
    compare (VList vs1) (VList vs2) = compare vs1 vs2
    compare (VSet s1) (VSet s2) = compare s1 s2
    compare (VMap m1) (VMap m2) = compare m1 m2
    -- These are only ever used for the internal set implementation
    compare (VDot vs1) (VDot vs2) = compare vs1 vs2
    compare (VChannel n) (VChannel n') = compare n n'
    compare (VDataType n) (VDataType n') = compare n n'
    compare (VProc p1) (VProc p2) = compare p1 p2
    compare (VFunction id1 _) (VFunction id2 _) = compare id1 id2
    compare (VLoc l1) (VLoc l2) = compare l1 l2

    compare v1 v2 = panic $
        -- Must be as a result of a mixed set of values, which cannot happen
        -- as a result of type checking.
        "Internal sets - cannot order "
      
procName :: ScopeIdentifier -> ProcName
procName = ProcName

scopeId :: Name -> [[Value]] -> Maybe ScopeIdentifier -> ScopeIdentifier
scopeId n vss pn = SFunctionBind h n args pn
    where
        h = (1 :: Int) `hashWithSalt` n `hashWithSalt` args `hashWithSalt` pn
        args = map (map trimValueForProcessName) vss

annonymousScopeId :: [Value] -> Maybe ScopeIdentifier -> ScopeIdentifier
annonymousScopeId vss pn = SVariableBind h args pn
    where
        h = (2 :: Int) `hashWithSalt` args `hashWithSalt` pn
        args = map trimValueForProcessName vss

-- | This assumes that the value is a VDot with the left is a VChannel
valueEventToEvent :: Value -> Event
valueEventToEvent = UserEvent

errorThunk = panic "Trimmed value function evaluated"

trimFunctionIdentifier :: FunctionIdentifier -> FunctionIdentifier
trimFunctionIdentifier (FBuiltInFunction h n args) =
    FBuiltInFunction h n (map (map trimValueForProcessName) args)
trimFunctionIdentifier (FLambda h e p) = FLambda h e p
trimFunctionIdentifier (FMatchBind h n args p) =
    FMatchBind h n (map (map trimValueForProcessName) args) p

trimValueForProcessName :: Value -> Value
trimValueForProcessName (VInt i) = VInt i
trimValueForProcessName (VChar c) = VChar c
trimValueForProcessName (VBool b) = VBool b
trimValueForProcessName (VLoc l) = VLoc l
trimValueForProcessName (VTuple vs) = VTuple (fmap trimValueForProcessName vs)
trimValueForProcessName (VList vs) = VList (map trimValueForProcessName vs)
trimValueForProcessName (VSet s) =
    VSet $ S.fromList $ map trimValueForProcessName $ S.toList s
trimValueForProcessName (VMap m) = VMap $ M.fromList $
    map (\ (v1, v2) -> (trimValueForProcessName v1, trimValueForProcessName v2))
        (M.toList m)
trimValueForProcessName (VDot vs) = VDot $ map trimValueForProcessName vs
trimValueForProcessName (VChannel n) = VChannel n
trimValueForProcessName (VDataType n) = VDataType n
trimValueForProcessName (VFunction id _) =
    VFunction (trimFunctionIdentifier id) errorThunk
trimValueForProcessName (VProc p) = VProc (trimProcess p)
