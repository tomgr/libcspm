module CSPM.Evaluator.Exceptions
where

import Prelude

import CSPM.Syntax.Names
import CSPM.Syntax.AST
import CSPM.PrettyPrinter
import CSPM.Evaluator.Environment
import CSPM.Evaluator.ValuePrettyPrinter ()
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet hiding (empty)
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

printCallStack :: StackTrace -> Doc
printCallStack [] =
    text "Lexical call stack: none available (set cspm.evaluator.record_stack_traces to on to enable stack tracing)"
printCallStack fs =
    let ppFrame _ [] = empty
        ppFrame i (StackFrame frame loc:fs) =
            int i <> colon <+> prettyPrint frame <+> text "at" <+> prettyPrint loc
            $$ ppFrame (i+1) fs
    in text "Lexical call stack:" $$ tabIndent (ppFrame 1 fs)


patternMatchFailureMessage :: SrcSpan -> TCPat -> Value -> ErrorMessage
patternMatchFailureMessage l pat v =
    mkErrorMessage l $ 
        hang (hang (text "Pattern match failure: Value") tabWidth
                (prettyPrint v))
            tabWidth (text "does not match the pattern" <+> prettyPrint pat)

patternMatchesFailureMessage :: SrcSpan -> [TCPat] -> [Value] -> ErrorMessage
patternMatchesFailureMessage l pat v =
    mkErrorMessage l $ 
        hang (hang (text "Pattern match failure: ") tabWidth
                (list (map prettyPrint v)))
            tabWidth (text "do not match the patterns" <+>
                list (map prettyPrint pat))

headEmptyListMessage :: SrcSpan -> StackTrace -> ErrorMessage
headEmptyListMessage loc scope = mkErrorMessage loc $ 
    text "Attempt to take head of empty list."
    $$ printCallStack scope

prioritiseEmptyListMessage :: SrcSpan -> StackTrace -> ErrorMessage
prioritiseEmptyListMessage loc scope = mkErrorMessage loc $ 
    text "Prioritise must be called with a non-empty list."
    $$ printCallStack scope

tailEmptyListMessage :: SrcSpan -> StackTrace -> ErrorMessage
tailEmptyListMessage loc scope = mkErrorMessage loc $ 
    text "Attempt to take tail of empty list."
    $$ printCallStack scope

explicitErrorMessage :: Value -> SrcSpan -> StackTrace -> ErrorMessage
explicitErrorMessage err loc scope = mkErrorMessage loc $
    text "Error:" <+> prettyPrint err
    $$ printCallStack scope

divideByZeroMessage :: SrcSpan -> StackTrace -> ErrorMessage
divideByZeroMessage loc scope = mkErrorMessage loc $
    text "Attempt to divide by zero"
    $$ printCallStack scope

keyNotInDomainOfMapMessage :: SrcSpan -> StackTrace -> ErrorMessage
keyNotInDomainOfMapMessage loc scope = mkErrorMessage loc $
    text "Lookup called on a key that is not in the domain of the map."
    $$ printCallStack scope

funBindPatternMatchFailureMessage :: SrcSpan -> Name -> [[Value]] -> ErrorMessage
funBindPatternMatchFailureMessage l n vss = mkErrorMessage l $
    hang (text "Pattern match failure whilst attempting to evaluate:") tabWidth
        (prettyPrint n <> 
            hcat (map (\ vs -> parens (list (map prettyPrint vs))) vss))

replicatedLinkParallelOverEmptySeqMessage :: Exp Name -> SrcSpan -> StackTrace -> ErrorMessage
replicatedLinkParallelOverEmptySeqMessage p l scope = mkErrorMessage l $
    hang (
        hang (text "The sequence expression in"<>colon) tabWidth 
            (prettyPrint p)
    ) tabWidth
    (text "evaluated to the empty sequence. However, replicated linked parallel is not defined for the empty sequence.")
    $$ printCallStack scope

replicatedInternalChoiceOverEmptySetMessage :: TCExp -> StackTrace -> ErrorMessage
replicatedInternalChoiceOverEmptySetMessage p scope = mkErrorMessage (loc p) $
    hang (
        hang (text "The set expression in"<>colon) tabWidth 
            (prettyPrint p)
    ) tabWidth
    (text "evaluated to the empty set. However, replicated internal choice is not defined for the empty set.")
    $$ printCallStack scope

replicatedInternalChoiceOverEmptySetMessage' :: TCPat -> StackTrace -> ErrorMessage
replicatedInternalChoiceOverEmptySetMessage' p scope = mkErrorMessage (loc p) $
    hang (
        hang (text "The pattern"<>colon) tabWidth (prettyPrint p)
    ) tabWidth
    (text "matched no elements of the channel set. However, replicated internal choice is not defined for the empty set.")
    $$ printCallStack scope

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
    text "Cannot check for set membership as the supplied set is infinite:"
    $$ tabIndent (prettyPrint vs)

cardOfInfiniteSetMessage :: ValueSet -> ErrorMessage
cardOfInfiniteSetMessage vs = mkErrorMessage Unknown $
    text "Attempt to take the cardinatlity of an infinite set:"
    $$ tabIndent (prettyPrint vs)

cannotDifferenceSetsMessage :: ValueSet -> ValueSet -> ErrorMessage
cannotDifferenceSetsMessage vs1 vs2 = mkErrorMessage Unknown $
    text "Cannot difference the supplied sets."

dotIsNotValidMessage :: Value -> Int -> Value -> ValueSet -> SrcSpan -> StackTrace -> ErrorMessage
dotIsNotValidMessage (value@(VDot (h:_))) field fieldValue fieldOptions loc scope =
    mkErrorMessage loc $
        hang (text "The value:") tabWidth (prettyPrint value)
        $$ text "is invalid as it is not within the set of values defined for" <+>
            case h of
                VChannel n -> text "the channel" <+> prettyPrint n <> char '.'
                VDataType n -> text "the data constructor" <+> prettyPrint n <> char '.'
        $$ hang (text "In particular the" <+> speakNth (field+1) <+> text "field:") tabWidth (prettyPrint fieldValue)
        $$ hang (text "is not a member of the set") tabWidth (prettyPrint fieldOptions)
        $$ printCallStack scope

setNotRectangularErrorMessage :: SrcSpan -> ValueSet -> Maybe ValueSet -> ErrorMessage
setNotRectangularErrorMessage loc s1 ms2 = mkErrorMessage loc $
    hang (text "The set:") tabWidth (prettyPrint s1)
    $$ text "cannot be decomposed into a cartesian product (i.e. it is not rectangular)."
    $$ case ms2 of
        Just s2 -> 
            hang (text "The cartesian product is equal to:") tabWidth 
                -- Force evaluation to a proper set, not a cart product.
                (prettyPrint (fromList (toList s2)))
            $$ hang (text "and thus the following values are missing:") tabWidth
                (prettyPrint (difference s2 s1))
        Nothing -> empty

prioritisePartialOrderCyclicOrder :: [Event] -> SrcSpan -> StackTrace -> ErrorMessage
prioritisePartialOrderCyclicOrder scc loc scid = mkErrorMessage loc $
    text "The partial order specified for priortisepo contains the following cycle:"
    $$ tabIndent (list (map prettyPrint scc))

prioritiseNonMaximalElement :: Event -> SrcSpan -> StackTrace -> ErrorMessage
prioritiseNonMaximalElement event loc scid = mkErrorMessage loc $
    text "The event:" <+> prettyPrint event
    <+> text "is declared as maximal, but is not maximal in the order."

prioritisePartialOrderEventsMissing :: [Event] -> [Event] -> SrcSpan -> StackTrace -> ErrorMessage
prioritisePartialOrderEventsMissing allEvents missingEvents loc scid = mkErrorMessage loc $
    text "The events:"
    $$ tabIndent (list (map prettyPrint missingEvents))
    $$ text "appear in the partial order, or in the set of maximal events, but are"
    $$ text "not in the set of all prioritised events:"
    $$ tabIndent (list (map prettyPrint allEvents))

linkParallelAmbiguous :: Event -> SrcSpan -> StackTrace -> ErrorMessage
linkParallelAmbiguous event loc scid = mkErrorMessage loc $
    text "The event:" <+> prettyPrint event
    $$ text "was erroneously mentioned several times in a linked-parallel expression;"
    $$ text "each event may appear at most once."

bufferEventAmbiguous :: Event -> SrcSpan -> StackTrace -> ErrorMessage
bufferEventAmbiguous event loc scid = mkErrorMessage loc $
    text "The event:" <+> prettyPrint event
    $$ text "was erroneously mentioned several times in call to a buffer;"
    $$ text "each event may appear as either an input event, an output event, or (in the case of"
    $$ text "an exploding buffer) an explosion event."

bufferCapacityInsufficient :: Int -> SrcSpan -> StackTrace -> ErrorMessage
bufferCapacityInsufficient cap loc scid = mkErrorMessage loc $
    text "The supplied buffer capacity" <+> int cap <+> text "was insufficient; it must be at least 1."
