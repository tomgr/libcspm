{-# LANGUAGE OverloadedStrings #-}
module CSPM.Evaluator.Values (
    Value(..), UProc, Proc(..), CSPOperator(..), ProcOperator(..), Event(..),
    compareValues,
    procId, annonymousProcId,
    valueEventToEvent,
    noSave, maybeSave, removeThunk, lookupVar,
) where

import Data.Hashable

import CSPM.Compiler.Events hiding (fromList)
import CSPM.Compiler.Processes
import CSPM.DataStructures.Names
import CSPM.DataStructures.Types
import CSPM.Evaluator.Monad
import {-# SOURCE #-} qualified CSPM.Evaluator.ValueSet as S
import CSPM.PrettyPrinter
import Util.Exception
import Util.Prelude
import Util.PrettyPrint

type UProc = UnCompiledProc

data Value =
    VInt Int
    | VBool Bool
    | VTuple [Value]
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
    | VFunction ([Value] -> EvaluationMonad Value)
    | VProc UProc
    | VThunk (EvaluationMonad Value)

-- | Given a program that yields a value, returns a second program that can be
-- inserted into the environment, but will cause the environment not to save
-- the actual value, but to recompute it everytime. This is useful for cheap,
-- to compute, but high cost in terms of memory, computations (like named
-- processes).
noSave :: EvaluationMonad Value -> EvaluationMonad Value
noSave prog = do
    pn <- getParentProcName
    return $ VThunk $ case pn of
                        Just x -> updateParentProcName x prog
                        Nothing -> prog

maybeSave :: Type -> EvaluationMonad Value -> EvaluationMonad Value
maybeSave TProc prog = noSave prog
maybeSave _ prog = prog

removeThunk :: Value -> EvaluationMonad Value
removeThunk (VThunk p) = p
removeThunk v = return v

lookupVar :: Name -> EvaluationMonad Value
lookupVar n = lookupVarMaybeThunk n >>= removeThunk

instance Hashable Value where
    hash (VInt i) = combine 1 (hash i)
    hash (VBool b) = combine 2 (hash b)
    hash (VTuple vs) = combine 5 (hash vs)
    hash (VDot vs) = combine 6 (hash vs)
    hash (VChannel n) = combine 7 (hash n)
    hash (VDataType n) = combine 8 (hash n)
    hash (VList vs) = combine 9 (hash vs)
    hash (VSet vset) = combine 10 (hash vset)
    hash (VFunction f) = panic "Cannot hash a function"
    hash (VProc (PProcCall n _)) = combine 12 (hash n)
    hash (VProc _) = panic "Cannot hash a process"

instance Eq Value where
    VInt i1 == VInt i2 = i1 == i2
    VBool b1 == VBool b2 = b1 == b2
    VTuple vs1 == VTuple vs2 = vs1 == vs2
    VDot vs1 == VDot vs2 = vs1 == vs2
    VChannel n1 == VChannel n2 = n1 == n2
    VDataType n1 == VDataType n2 = n1 == n2
    VList vs1 == VList vs2 = vs1 == vs2
    VSet s1 == VSet s2 = s1 == s2
    VProc (PProcCall n _) == VProc (PProcCall n' _) = n == n'
    
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
        cmp [] [] = EQ
        cmp (x:xs) (y:ys) = compare x y `thenCmp` cmp xs ys
    in Just (cmp vs1 vs2)
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
compareValues v1 v2 = panic $
    "Cannot compare "++show v1++" "++show v2

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

    compare v1 v2 = panic $
        -- Must be as a result of a mixed set of values, which cannot happen
        -- as a result of type checking.
        "Internal sets - cannot order "++show v1++" "++show v2

instance PrettyPrintable Value where
    prettyPrint (VInt i) = int i
    prettyPrint (VBool True) = text "true"
    prettyPrint (VBool False) = text "false"
    prettyPrint (VTuple vs) = parens (list $ map prettyPrint vs)
    prettyPrint (VDot vs) = dotSep (map prettyPrint vs)
    prettyPrint (VChannel n) = prettyPrint n
    prettyPrint (VDataType n) = prettyPrint n
    prettyPrint (VList vs) = angles (list $ map prettyPrint vs)
    prettyPrint (VSet s) = prettyPrint s
    prettyPrint (VFunction _) = text "<function>"
    prettyPrint (VProc p) = prettyPrint p
    prettyPrint (VThunk th) = text "<thunk>"

instance Show Value where
    show v = show (prettyPrint v)
      
procId :: Name -> [[Value]] -> Maybe ProcName -> ProcName
procId n vss pn = ProcName n vss pn

annonymousProcId :: [[Value]] -> Maybe ProcName -> ProcName
annonymousProcId vss pn = AnnonymousProcName vss pn

-- | This assumes that the value is a VDot with the left is a VChannel
valueEventToEvent :: Value -> Event
valueEventToEvent = UserEvent
