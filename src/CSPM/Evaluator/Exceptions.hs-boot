module CSPM.Evaluator.Exceptions where

import Prelude

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import {-# SOURCE #-} CSPM.Evaluator.Values
import {-# SOURCE #-} CSPM.Evaluator.ValueSet
import Util.Annotated
import Util.Exception

patternMatchFailureMessage :: SrcSpan -> TCPat -> Value -> ErrorMessage
headEmptyListMessage :: SrcSpan -> Maybe InstantiatedFrame -> ErrorMessage
tailEmptyListMessage :: SrcSpan -> Maybe InstantiatedFrame -> ErrorMessage
funBindPatternMatchFailureMessage :: SrcSpan -> Name -> [[Value]] -> ErrorMessage
replicatedInternalChoiceOverEmptySetMessage :: TCExp ->
    Maybe InstantiatedFrame -> ErrorMessage
replicatedInternalChoiceOverEmptySetMessage' :: TCPat ->
    Maybe InstantiatedFrame -> ErrorMessage
typeCheckerFailureMessage :: String -> ErrorMessage
cannotConvertIntegersToListMessage :: ErrorMessage
cannotConvertProcessesToListMessage :: ErrorMessage
cannotCheckSetMembershipError :: Value -> ValueSet -> ErrorMessage
cardOfInfiniteSetMessage :: ValueSet -> ErrorMessage
cannotDifferenceSetsMessage :: ValueSet -> ValueSet -> ErrorMessage
dotIsNotValidMessage :: Value -> Int -> Value -> ValueSet -> SrcSpan ->
    Maybe InstantiatedFrame -> ErrorMessage
setNotRectangularErrorMessage :: SrcSpan -> ValueSet -> Maybe ValueSet ->
    ErrorMessage
