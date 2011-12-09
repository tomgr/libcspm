module CSPM.Evaluator.Values (
    Value(..), Proc(..), ProcOperator(..), Event(..),
    procId,
    valueEventToEvent,
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
    -- TODO: the following one may be completely incorrect, needs 
    -- testing
    | VDot [Value]
    | VEvent Name [Value]
    | VDataType Name [Value]
    | VList [Value]
    | VSet ValueSet
    | VFunction ([Value] -> EvaluationMonad Value)
    | VProc Proc

instance Eq Value where
    VInt i1 == VInt i2 = i1 == i2
    VBool b1 == VBool b2 = b1 == b2
    VTuple vs1 == VTuple vs2 = vs1 == vs2
    VDot vs1 == VDot vs2 = vs1 == vs2
    VEvent n1 vs1 == VEvent n2 vs2 = n1 == n2 && vs1 == vs2
    VDataType n1 vs1 == VDataType n2 vs2 = n1 == n2 && vs1 == vs2
    VList vs1 == VList vs2 = vs1 == vs2
    VSet s1 == VSet s2 = s1 == s2
    
    v1 == v2 = throwError $ typeCheckerFailureMessage "Cannot compare for eq"
    
-- TODO: compare this to bill's book, some of this loos wrong
instance Ord Value where
    compare (VInt i1) (VInt i2) = compare i1 i2
    compare (VBool b1) (VBool b2) = compare b1 b2
    compare (VTuple vs1) (VTuple vs2) = compare vs1 vs2
    compare (VList vs1) (VList vs2) = compare vs1 vs2
    compare (VSet s1) (VSet s2) = compare s1 s2
    
    -- These are only ever used for the internal set implementation
    compare (VDot vs1) (VDot vs2) = compare vs1 vs2
    compare (VEvent n vs1) (VEvent n' vs2) =
        compare n n' `thenCmp` compare vs1 vs2
    compare (VDataType n vs1) (VDataType n' vs2) = 
        compare n n' `thenCmp` compare vs1 vs2

    compare v1 v2 = throwError $ typeCheckerFailureMessage $
        "Cannot order "++show v1++" "++show v2

instance PrettyPrintable Value where
    prettyPrint (VInt i) = integer i
    prettyPrint (VBool True) = text "true"
    prettyPrint (VBool False) = text "false"
    prettyPrint (VTuple vs) = parens (list $ map prettyPrint vs)
    prettyPrint (VDot vs) = dotSep (map prettyPrint vs)
    prettyPrint (VEvent n vs) = dotSep (prettyPrint n:map prettyPrint vs)
    prettyPrint (VDataType n vs) = dotSep (prettyPrint n:map prettyPrint vs)
    prettyPrint (VList vs) = angles (list $ map prettyPrint vs)
    prettyPrint (VSet s) = prettyPrint s
    prettyPrint (VFunction _) = text "<function>"
    prettyPrint (VProc p) = prettyPrint p

instance Show Value where
    show v = show (prettyPrint v)

-- TODO take acount of let within statements
procId :: Name -> [[Value]] -> String
procId n vss = show $
    prettyPrint n <> hcat (map (parens . list) (map (map prettyPrint) vss))

valueEventToEvent :: Value -> Event
valueEventToEvent (ev@(VEvent _ _)) = UserEvent (show (prettyPrint ev))
