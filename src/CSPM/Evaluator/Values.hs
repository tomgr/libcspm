{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module CSPM.Evaluator.Values (
    Value(..),  compareValues,
    trueValue, falseValue, makeBoolValue,

    InstantiatedFrame(..), makeProcessName, procName, instantiateFrame,
    instantiateFrameWithArguments, instantiateBuiltinFrameWithArguments,

    valueEventToEvent, createFunction,
    noSave, maybeSave, removeThunk, lookupVar, maybeLookupVar,
    tupleFromList,
    module Data.Array,

    -- * Events
    Event(..), EventSet, eventSetFromList, EventMap,
    -- * Processes
    Proc(..), CSPOperator(..), ProcOperator(..),  ProcName(..),
    operator, components, splitProcIntoComponents,
) where

import Data.Array
import qualified Data.Foldable as F
import Data.Hashable
import qualified Data.Set as St
import qualified Data.Map as M
import GHC.Generics (Generic)
import Prelude hiding (lookup)

import CSPM.Evaluator.AnalyserMonad
import CSPM.Evaluator.Monad
import {-# SOURCE #-} qualified CSPM.Evaluator.ValueSet as S
import CSPM.Syntax.Names
import CSPM.Syntax.Types
import Util.Annotated
import Util.Exception
import Util.Prelude
import Util.UnsafePointerEquality

data Value =
    VInt {-# UNPACK #-} !Int
    | VChar {-# UNPACK #-} !Char
    | VBool Bool
    | VTuple (Array Int Value)
    -- | If A is a datatype clause that has 3 fields a b c then a runtime
    -- instantiation of this would be VDot [VDataType "A", a, b, c] where a,b
    -- and c can contain other VDots.
    | VDot [Value]
    -- The following two never appear on their own, they are always part of a 
    -- VDot (even if the VDot has no values).
    | VChannel {-# UNPACK #-} !Name
    | VDataType {-# UNPACK #-} !Name
    | VList [Value]
    | VMap (M.Map Value Value)
    | VSet S.ValueSet
    | VFunction InstantiatedFrame ([Value] -> EvaluationMonad Value)
    | VProc Proc
    | VLoc SrcSpan
    | VThunk (EvaluationMonad Value)

trueValue, falseValue :: Value
trueValue = VBool True
falseValue = VBool False

makeBoolValue :: Bool -> Value
makeBoolValue True = trueValue
makeBoolValue False = falseValue

-- | A disambiguator between different occurences of either processes or
-- functions. This works by storing the values that are bound (i.e. the free
-- variables the inner `thing` may depend on). This is used as a 'ProcName' and
-- for 'FunctionIdentifier's.
data InstantiatedFrame =
    InstantiatedFrame {
        instantiatedFrameCachedHash :: Int,
        instantiatedFrameFrame :: FrameInformation,
        instantiatedFrameFreeVars :: [Value],
        instantiatedFrameMaybeArguments :: [[Value]]
    }

instance Eq InstantiatedFrame where
    if1@(InstantiatedFrame h1 f1 vs1 args1) == if2@(InstantiatedFrame h2 f2 vs2 args2) =
        unsafePointerEquality if1 if2
        || (h1 == h2 && f1 == f2 && vs1 == vs2 && args1 == args2)
instance Hashable InstantiatedFrame where
    hashWithSalt s = hashWithSalt s . instantiatedFrameCachedHash
instance Ord InstantiatedFrame where
    compare (InstantiatedFrame h1 i1 vs1 args1)
            (InstantiatedFrame h2 i2 vs2 args2) =
        compare h1 h2 `thenCmp` compare i1 i2 `thenCmp` compare vs1 vs2
        `thenCmp` compare args1 args2

createFunction :: InstantiatedFrame ->
    ([Value] -> EvaluationMonad Value) ->
    EvaluationMonad Value
createFunction frame fn = do
    st <- gets id
    return $! VFunction frame $! \ args -> return $! runEvaluator st (fn args)

defaultSalt :: Int
defaultSalt = 17

instantiateFrame :: FrameInformation -> EvaluationMonad InstantiatedFrame
instantiateFrame frame = do
    vs <- mapM lookupVar (frameFreeVars frame)
    let h = defaultSalt `hashWithSalt` frame `hashWithSalt` vs
            `hashWithSalt` ([] :: [[Value]])
    return $! InstantiatedFrame h frame vs []

makeProcessName frame = instantiateFrame frame >>= return . procName

instantiateFrameWithArguments :: FrameInformation -> [[Value]] ->
    EvaluationMonad InstantiatedFrame
instantiateFrameWithArguments frame args = do
    vs <- mapM lookupVar (frameFreeVars frame)
    let h = defaultSalt `hashWithSalt` frame `hashWithSalt` vs
                `hashWithSalt` args
    return $! InstantiatedFrame h frame vs args

instantiateBuiltinFrameWithArguments :: FrameInformation -> [[Value]] ->
    InstantiatedFrame
instantiateBuiltinFrameWithArguments frame args =
    let h = (1 :: Int) `hashWithSalt` frame
                `hashWithSalt` ([] :: [Value]) `hashWithSalt` args
    in InstantiatedFrame h frame [] args 

procName :: InstantiatedFrame -> ProcName
procName = ProcName

tupleFromList :: [Value] -> Value
tupleFromList vs = VTuple $! listArray (0, length vs - 1) vs

-- | Given a program that yields a value, returns a second program that can be
-- inserted into the environment, but will cause the environment not to save
-- the actual value, but to recompute it everytime. This is useful for cheap,
-- to compute, but high cost in terms of memory, computations (like named
-- processes).
noSave :: EvaluationMonad Value -> EvaluationMonad Value
noSave prog = do
    --pn <- getParentScopeIdentifier
    return $ VThunk prog 
        -- $ modify 
        --(\ st -> st {
            --CSPM.Evaluator.Monad.parentScopeIdentifier = pn
        -- }) prog

maybeSave :: Type -> EvaluationMonad Value -> EvaluationMonad Value
maybeSave TProc prog = noSave prog
maybeSave _ prog = prog

removeThunk :: Value -> EvaluationMonad Value
removeThunk (VThunk p) = p
removeThunk v = return v

lookupVar :: Name -> EvaluationMonad Value
lookupVar n = lookupVarMaybeThunk n >>= removeThunk

maybeLookupVar :: Name -> EvaluationMonad (Maybe Value)
maybeLookupVar n = do
    v <- maybeLookupVarMaybeThunk n
    case v of
        Just v -> removeThunk v >>= return . Just
        Nothing -> return Nothing

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
    hashWithSalt s (VFunction id _) = s `hashWithSalt` (11 :: Int) `hashWithSalt` id
    hashWithSalt s (VProc p) = s `hashWithSalt` (12 :: Int) `hashWithSalt` p
    hashWithSalt s (VMap m) = s `hashWithSalt` (13 :: Int) `hashWithSalt` (M.toList m)
    hashWithSalt s (VLoc l) = s `hashWithSalt` (14 :: Int) `hashWithSalt` l

valuesAreEqual :: Value -> Value -> Bool
valuesAreEqual (VInt i1) (VInt i2) = i1 == i2
valuesAreEqual (VBool b1) (VBool b2) = b1 == b2
valuesAreEqual (VChar c1) (VChar c2) = c1 == c2
valuesAreEqual (VTuple vs1) (VTuple vs2) = vs1 == vs2
valuesAreEqual (VDot vs1) (VDot vs2) = vs1 == vs2
valuesAreEqual (VChannel n1) (VChannel n2) = n1 == n2
valuesAreEqual (VDataType n1) (VDataType n2) = n1 == n2
valuesAreEqual (VList vs1) (VList vs2) = vs1 == vs2
valuesAreEqual (VSet s1) (VSet s2) = s1 == s2
valuesAreEqual (VProc p1) (VProc p2) = p1 == p2
valuesAreEqual (VFunction id1 _) (VFunction id2 _) = id1 == id2
valuesAreEqual (VMap m1) (VMap m2) = m1 == m2
valuesAreEqual (VLoc l1) (VLoc l2) = l1 == l2
valuesAreEqual _ _ = False

instance Eq Value where
    v1 == v2 = unsafePointerEquality v1 v2 || valuesAreEqual v1 v2
    
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

-- | This assumes that the value is a VDot with the left is a VChannel
valueEventToEvent :: Value -> Event
valueEventToEvent = UserEvent

-- * Process Values

-- | Events, as represented in the LTS.
data Event = 
    -- | The internal special event tau.
    Tau 
    -- | The internal event tick, representing termination.
    | Tick 
    -- | Any event defined in a channel definition.
    | UserEvent Value
    deriving (Eq, Generic, Ord)

instance Hashable Event

type EventMap = [(Event, Event)]
type EventSet = [Event]

eventSetFromList :: [Event] -> EventSet
eventSetFromList x = x 

-- | ProcNames uniquely identify processes.
newtype ProcName = ProcName InstantiatedFrame deriving (Eq, Hashable, Ord)

-- | An operator that can be applied to processes.
data ProcOperator =
    Chase Bool
    | DelayBisim
    | Determinise
    | Diamond
    | Explicate Bool
    | FailureWatchdog EventSet Event
    | Normalize Bool
    | ModelCompress
    | Prioritise Bool [EventSet]
    | PartialOrderPrioritise [(Event, Event)]
    | StrongBisim
    | TauLoopFactor
    | TraceWatchdog EventSet Event
    | WeakBisim
    deriving (Eq, Generic, Ord)

instance Hashable ProcOperator

data CSPOperator =
    PAlphaParallel [EventSet]
    | PChaos EventSet
    | PException EventSet
    | PExternalChoice
    | PGenParallel EventSet
    | PHide EventSet
    | PInternalChoice
    | PInterrupt
    | PInterleave
    -- Map from event of left process, to event of right that it synchronises
    -- with. (Left being p1, Right being p2 ps ps).
    | PLinkParallel EventMap
    | POperator ProcOperator
    | PPrefix Event
    | PPrefixEventSet EventSet
    | PProject EventSet
    -- Map from Old -> New event
    | PRename EventMap
    | PRun EventSet
    | PSequentialComp
    | PSlidingChoice
    | PSynchronisingExternalChoice EventSet
    | PSynchronisingInterrupt EventSet
    deriving (Eq, Generic, Ord)

instance Hashable CSPOperator

-- | A compiled process. Note this is an infinite data structure (due to
-- PProcCall) as this makes compilation easy (we can easily chase
-- dependencies).
data Proc = 
    PUnaryOp CSPOperator Proc
    | PBinaryOp CSPOperator Proc Proc
    | POp CSPOperator [Proc]
    -- | Labels the process this contains. This allows infinite loops to be
    -- spotted.
    | PProcCall ProcName Proc

instance Eq Proc where
    (PProcCall pn1 _) == (PProcCall pn2 _) = pn1 == pn2
    (PUnaryOp op1 p1) == (PUnaryOp op2 p2) = op1 == op2 && p1 == p2
    (PBinaryOp op1 p1 p2) == (PBinaryOp op2 r1 r2) =
        op1 == op2 && p1 == r1 && p2 == r2
    (POp op1 ps1) == (POp op2 ps2) = op1 == op2 && ps1 == ps2
    _ == _ = False

instance Hashable Proc where
    hashWithSalt s (PProcCall pn1 _) = s `hashWithSalt` (1 :: Int) `hashWithSalt` pn1
    hashWithSalt s (PUnaryOp op1 p1) = s `hashWithSalt` (2 :: Int) `hashWithSalt` op1 `hashWithSalt` p1
    hashWithSalt s (PBinaryOp op1 p1 p2) =
        s `hashWithSalt` (3 :: Int) `hashWithSalt` op1 `hashWithSalt` p1 `hashWithSalt` p2
    hashWithSalt s (POp op ps) =
        s `hashWithSalt` (4 :: Int) `hashWithSalt` op `hashWithSalt` ps

instance Ord Proc where
    compare (PProcCall pn1 _) (PProcCall pn2 _) = compare pn1 pn2
    compare (PProcCall _ _) _ = LT
    compare _ (PProcCall _ _) = GT
    compare (PUnaryOp op1 p1) (PUnaryOp op2 p2) =
        compare op1 op2 `thenCmp` compare p1 p2
    compare (PUnaryOp _ _) _ = LT
    compare _ (PUnaryOp _ _) = GT
    compare (PBinaryOp op1 p1 p2) (PBinaryOp op2 r1 r2) =
        compare op1 op2 `thenCmp` compare p1 r1 `thenCmp` compare p2 r2
    compare (PBinaryOp _ _ _) _ = LT
    compare _ (PBinaryOp _ _ _) = GT
    compare (POp op1 ps1) (POp op2 ps2) =
        compare op1 op2 `thenCmp` compare ps1 ps2
-- | Gives the operator of a process. If the process is a ProcCall an error is
-- thrown.
operator :: Proc -> CSPOperator
operator (PUnaryOp op _) = op
operator (PBinaryOp op _ _)=  op
operator (POp op _) = op

-- | Returns the components of a given process.
components :: Proc -> [Proc]
components (PBinaryOp _ p1 p2) = [p1, p2]
components (POp _ ps) = ps
components (PUnaryOp _ p1) = [p1]

-- | Given a process, returns the initial process and all processes that it
-- calls.
splitProcIntoComponents :: Proc -> (Proc, [(ProcName, Proc)])
splitProcIntoComponents p =
    let
        explored s n = St.member n s

        exploreAll s pns [] = (s, pns)
        exploreAll s pns (p:ps) = exploreAll s' pns' ps
            where (s', pns') = explore s pns p

        explore s pns (PUnaryOp _ p) = explore s pns p
        explore s pns (PBinaryOp _ p1 p2) = exploreAll s pns [p1,p2]
        explore s pns (POp _ ps) = exploreAll s pns (F.toList ps)
        explore s pns (PProcCall n p) =
            if explored s n then (s, pns)
            else explore (St.insert n s) ((n, p):pns) p
    in (p, snd $ explore St.empty [] p)
