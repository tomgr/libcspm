module CSPM.Evaluator.Exceptions where

import Prelude hiding ((<>))

import CSPM.Syntax.Names
import CSPM.Syntax.AST
import CSPM.Evaluator.Environment
import {-# SOURCE #-} CSPM.Evaluator.Values
import {-# SOURCE #-} CSPM.Evaluator.ValueSet
import Util.Annotated
import Util.Exception

patternMatchFailureMessage :: SrcSpan -> TCPat -> Value -> ErrorMessage
headEmptyListMessage :: SrcSpan -> StackTrace -> ErrorMessage
tailEmptyListMessage :: SrcSpan -> StackTrace -> ErrorMessage
funBindPatternMatchFailureMessage :: SrcSpan -> Name -> [[Value]] -> ErrorMessage
replicatedInternalChoiceOverEmptySetMessage :: TCExp -> StackTrace -> ErrorMessage
replicatedInternalChoiceOverEmptySetMessage' :: TCPat -> StackTrace -> ErrorMessage
typeCheckerFailureMessage :: String -> ErrorMessage
cannotConvertIntegersToListMessage :: ErrorMessage
cannotConvertProcessesToListMessage :: ErrorMessage
cannotCheckSetMembershipError :: Value -> ValueSet -> ErrorMessage
cardOfInfiniteSetMessage :: ValueSet -> ErrorMessage
cannotDifferenceSetsMessage :: ValueSet -> ValueSet -> ErrorMessage
dotIsNotValidMessage :: Value -> Int -> Value -> ValueSet -> SrcSpan -> StackTrace -> ErrorMessage
setNotRectangularErrorMessage :: SrcSpan -> ValueSet -> Maybe ValueSet -> ErrorMessage
