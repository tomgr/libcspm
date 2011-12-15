module CSPM.Evaluator.Exceptions
where

import Data.Typeable
import Prelude

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.PrettyPrinter
import {-# SOURCE #-} CSPM.Evaluator.Values
import {-# SOURCE #-} CSPM.Evaluator.ValueSet
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

patternMatchFailureMessage :: SrcSpan -> AnPat -> Value -> ErrorMessage
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

funBindPatternMatchFailureMessage :: SrcSpan -> Name -> [[Value]] -> ErrorMessage
funBindPatternMatchFailureMessage l n vss = mkErrorMessage l $
    hang (text "Pattern match failure whilst attempting to evaluate:") tabWidth
        (prettyPrint n <> 
            hcat (map (\ vs -> parens (list (map prettyPrint vs))) vss))

replicatedInternalChoiceOverEmptySetMessage :: SrcSpan -> Exp -> ErrorMessage
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

cannotCheckSetMembershipError :: Value -> ValueSet -> ErrorMessage
cannotCheckSetMembershipError v vs = mkErrorMessage Unknown $
    text "Cannot check for set membership as the supplied set is infinite."

cardOfInfiniteSetMessage :: ValueSet -> ErrorMessage
cardOfInfiniteSetMessage vs = mkErrorMessage Unknown $
    text "Attempt to take the cardinatlity of an infinite set."

cannotUnionSetsMessage :: ValueSet -> ValueSet -> ErrorMessage
cannotUnionSetsMessage vs1 vs2 = mkErrorMessage Unknown $
    text "Cannot union the supplied sets."

cannotIntersectSetsMessage :: ValueSet -> ValueSet -> ErrorMessage
cannotIntersectSetsMessage vs1 vs2 = mkErrorMessage Unknown $
    text "Cannot intersect the supplied sets."

cannotDifferenceSetsMessage :: ValueSet -> ValueSet -> ErrorMessage
cannotDifferenceSetsMessage vs1 vs2 = mkErrorMessage Unknown $
    text "Cannot difference the supplied sets."
