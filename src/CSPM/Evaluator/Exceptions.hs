{-# LANGUAGE FlexibleContexts #-}
module CSPM.Evaluator.Exceptions where

import Data.Typeable
import Prelude

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.PrettyPrinter
import CSPM.Evaluator.Values
import {-# SOURCE #-} CSPM.Evaluator.ValueSet
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

patternMatchFailureMessage :: PrettyPrintable (UProc ops) => 
    SrcSpan -> TCPat -> Value ops -> ErrorMessage
patternMatchFailureMessage l pat v =
    mkErrorMessage l $ 
        hang (hang (text "Pattern match failure: Value") tabWidth
                (prettyPrint v))
            tabWidth (text "does not match the pattern" <+> prettyPrint pat)

headEmptyListMessage :: ErrorMessage
headEmptyListMessage = mkErrorMessage Unknown $ 
    text "Attempt to take head of empty list."

tailEmptyListMessage :: ErrorMessage
tailEmptyListMessage = mkErrorMessage Unknown $ 
    text "Attempt to take tail of empty list."

funBindPatternMatchFailureMessage :: PrettyPrintable (UProc ops) => 
    SrcSpan -> Name -> [[Value ops]] -> ErrorMessage
funBindPatternMatchFailureMessage l n vss = mkErrorMessage l $
    hang (text "Pattern match failure whilst attempting to evaluate:") tabWidth
        (prettyPrint n <> 
            hcat (map (\ vs -> parens (list (map prettyPrint vs))) vss))

replicatedInternalChoiceOverEmptySetMessage :: 
    PrettyPrintable (p Name) => SrcSpan -> Exp Name p -> ErrorMessage
replicatedInternalChoiceOverEmptySetMessage l p = mkErrorMessage l $
    hang (
        hang (text "The set expression in"<>colon) tabWidth 
            (prettyPrint p)
    ) tabWidth
    (text "evaluated to the empty set. However, replicated internal choice is not defined for the empty set.")

typeCheckerFailureMessage :: String -> ErrorMessage
typeCheckerFailureMessage s = mkErrorMessage Unknown $
    hang (text "The program caused a runtime error that should have been caught by the typechecker:")
        tabWidth (text s)

cannotConvertIntegersToListMessage :: ErrorMessage
cannotConvertIntegersToListMessage = mkErrorMessage Unknown $
    text "Cannot convert the set of all integers into a list."

cannotConvertProcessesToListMessage :: ErrorMessage
cannotConvertProcessesToListMessage = mkErrorMessage Unknown $
    text "Cannot convert the set of all processes (i.e. Proc) into a list."

cannotCheckSetMembershipError :: Value ops -> ValueSet ops -> ErrorMessage
cannotCheckSetMembershipError v vs = mkErrorMessage Unknown $
    text "Cannot check for set membership as the supplied set is infinite."

cardOfInfiniteSetMessage :: ValueSet ops -> ErrorMessage
cardOfInfiniteSetMessage vs = mkErrorMessage Unknown $
    text "Attempt to take the cardinatlity of an infinite set."

cannotUnionSetsMessage :: ValueSet ops -> ValueSet ops -> ErrorMessage
cannotUnionSetsMessage vs1 vs2 = mkErrorMessage Unknown $
    text "Cannot union the supplied sets."

cannotIntersectSetsMessage :: ValueSet ops -> ValueSet ops -> ErrorMessage
cannotIntersectSetsMessage vs1 vs2 = mkErrorMessage Unknown $
    text "Cannot intersect the supplied sets."

cannotDifferenceSetsMessage :: ValueSet ops -> ValueSet ops -> ErrorMessage
cannotDifferenceSetsMessage vs1 vs2 = mkErrorMessage Unknown $
    text "Cannot difference the supplied sets."

eventIsNotValidMessage :: PrettyPrintable (UProc ops) => Value ops -> ErrorMessage
eventIsNotValidMessage (v@(VDot (VChannel n:_))) = mkErrorMessage Unknown $
    text "The event" 
    <+> prettyPrint v 
    <+> text "is not a member of its channel"
    <+> prettyPrint n
    <+> text "and therefore the event is not valid."
