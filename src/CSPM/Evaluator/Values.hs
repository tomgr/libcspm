module CSPM.Evaluator.Values (
    Value(..), Proc(..), ProcOperator(..), Event(..),
    compareValues,
    procId,
    valueEventToEvent,
    combineDots,
) where

import CSPM.Compiler.Events
import CSPM.Compiler.Processes
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import {-# SOURCE #-} CSPM.Evaluator.ValueSet
import CSPM.PrettyPrinter
import Util.Prelude
import Util.PrettyPrint

data Value =
    VInt Integer
    | VBool Bool
    | VTuple [Value]
    -- | The values in a VDot are never other VDots, they are always other 
    -- items.
    | VDot [Value]
    | VChannel Name -- [Value]
    | VDataType Name -- [Value]
    | VList [Value]
    | VSet ValueSet
    | VFunction ([Value] -> EvaluationMonad Value)
    | VProc Proc

instance Eq Value where
    VInt i1 == VInt i2 = i1 == i2
    VBool b1 == VBool b2 = b1 == b2
    VTuple vs1 == VTuple vs2 = vs1 == vs2
    VDot vs1 == VDot vs2 = vs1 == vs2
    VChannel n1 == VChannel n2 = n1 == n2
    VDataType n1 == VDataType n2 = n1 == n2
    VList vs1 == VList vs2 = vs1 == vs2
    VSet s1 == VSet s2 = s1 == s2
    
    v1 == v2 = throwError $ typeCheckerFailureMessage $ show $
        text "Cannot compare the following for equality:"
        $$ tabIndent (prettyPrint v1 $$ prettyPrint v2)
    
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
compareValues (VSet s1) (VSet s2) = compareValueSets s1 s2

-- The following can only be compared for equality, hence if they are not
-- equal we return Nothing.
compareValues (VChannel n1) (VChannel n2) = 
    if n1 == n2 then Just EQ else Nothing
compareValues (VDataType n1) (VDataType n2) = 
    if n1 == n2 then Just EQ else Nothing
compareValues (VDot vs1) (VDot vs2) =
    if vs1 == vs2 then Just EQ else Nothing
-- Every other possibility is invalid
compareValues v1 v2 = throwError $ typeCheckerFailureMessage $
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

    compare v1 v2 = throwError $ typeCheckerFailureMessage $
        -- Must be as a result of a mixed set of values, which cannot happen
        -- as a result of type checking.
        "Internal sets - cannot order "++show v1++" "++show v2

instance PrettyPrintable Value where
    prettyPrint (VInt i) = integer i
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

instance Show Value where
    show v = show (prettyPrint v)

-- | Takes two values and returns a VDot. If the two arguments are both VDots
-- then this combines them into a single one.
combineDots :: Value -> Value -> Value
combineDots (VDot vs1) (VDot vs2) = VDot (vs1++vs2)
combineDots (VDot vs) y = VDot (vs++[y])
combineDots x (VDot vs) = VDot (x:vs)
combineDots v1 v2 = VDot [v1, v2]

-- TODO take acount of let within statements
procId :: Name -> [[Value]] -> String
procId n vss = show $
    prettyPrint n <> hcat (map (parens . list) (map (map prettyPrint) vss))

-- | This assumes that the value is a VDot with the left is a VChannel
valueEventToEvent :: Value -> Event
valueEventToEvent v = UserEvent (show (prettyPrint v))
