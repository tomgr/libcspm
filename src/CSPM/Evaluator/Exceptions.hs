module CSPM.Evaluator.Exceptions
where

import Prelude

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.PrettyPrinter
import {-# SOURCE #-} CSPM.Evaluator.ValuePrettyPrinter ()
import CSPM.Evaluator.Values
import {-# SOURCE #-} CSPM.Evaluator.ValueSet
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

patternMatchFailureMessage :: SrcSpan -> TCPat -> Value -> ErrorMessage
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

replicatedInternalChoiceOverEmptySetMessage :: SrcSpan -> Exp Name -> ErrorMessage
replicatedInternalChoiceOverEmptySetMessage l p = mkErrorMessage l $
    hang (
        hang (text "The set expression in"<>colon) tabWidth 
            (prettyPrint p)
    ) tabWidth
    (text "evaluated to the empty set. However, replicated internal choice is not defined for the empty set.")

replicatedInternalChoiceOverEmptySetMessage' :: SrcSpan -> Pat Name -> ErrorMessage
replicatedInternalChoiceOverEmptySetMessage' l p = mkErrorMessage l $
    hang (
        hang (text "The pattern"<>colon) tabWidth (prettyPrint p)
    ) tabWidth
    (text "matched no elements of the channel set. However, replicated internal choice is not defined for the empty set.")

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

cannotDifferenceSetsMessage :: ValueSet -> ValueSet -> ErrorMessage
cannotDifferenceSetsMessage vs1 vs2 = mkErrorMessage Unknown $
    text "Cannot difference the supplied sets."

dotIsNotValidMessage :: Value -> Int -> Value -> ValueSet -> ErrorMessage
dotIsNotValidMessage (value@(VDot (h:_))) field fieldValue fieldOptions = mkErrorMessage Unknown $
    hang (text "The value:") tabWidth (prettyPrint value)
    $$ text "is invalid as it is not within the set of values defined for" <+>
        case h of
            VChannel n -> text "the channel" <+> prettyPrint n <> char '.'
            VDataType n -> text "the data constructor" <+> prettyPrint n <> char '.'
    $$ hang (text "In particular the" <+> speakNth (field+1) <+> text "field:") tabWidth (prettyPrint fieldValue)
    $$ if isFinitePrintable fieldOptions then
            hang (text "is not a member of the set") tabWidth (prettyPrint fieldOptions)
        else text "is not a member of the required set."

setNotRectangularErrorMessage :: ValueSet -> ValueSet -> ErrorMessage
setNotRectangularErrorMessage s1 s2 = mkErrorMessage Unknown $
    hang (text "The set:") tabWidth (prettyPrint s1)
    $$ text "cannot be decomposed into a cartesian product (i.e. it is not rectangular)."
    $$ hang (text "The cartesian product is equal to:") tabWidth 
        -- Force evaluation to a proper set, not a cart product.
        (prettyPrint (fromList (toList s2)))
    $$ hang (text "and thus the following values are missing:") tabWidth
        (prettyPrint (difference s2 s1))
