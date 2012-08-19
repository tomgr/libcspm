module CSPM.Evaluator.Exceptions where

import Prelude

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import {-# SOURCE #-} CSPM.Evaluator.Values
import {-# SOURCE #-} CSPM.Evaluator.ValueSet
import Util.Annotated
import Util.Exception

patternMatchFailureMessage :: SrcSpan -> TCPat -> Value -> ErrorMessage
headEmptyListMessage :: SrcSpan -> Maybe ScopeIdentifier -> ErrorMessage
tailEmptyListMessage :: SrcSpan -> Maybe ScopeIdentifier -> ErrorMessage
funBindPatternMatchFailureMessage :: SrcSpan -> Name -> [[Value]] -> ErrorMessage
replicatedInternalChoiceOverEmptySetMessage :: Exp Name -> SrcSpan -> 
    Maybe ScopeIdentifier -> ErrorMessage
replicatedInternalChoiceOverEmptySetMessage' :: Pat Name -> SrcSpan ->
    Maybe ScopeIdentifier -> ErrorMessage
typeCheckerFailureMessage :: String -> ErrorMessage
cannotConvertIntegersToListMessage :: ErrorMessage
cannotConvertProcessesToListMessage :: ErrorMessage
cannotCheckSetMembershipError :: Value -> ValueSet -> ErrorMessage
cardOfInfiniteSetMessage :: ValueSet -> ErrorMessage
cannotDifferenceSetsMessage :: ValueSet -> ValueSet -> ErrorMessage
dotIsNotValidMessage :: Value -> Int -> Value -> ValueSet -> SrcSpan ->
    Maybe ScopeIdentifier -> ErrorMessage
setNotRectangularErrorMessage :: ValueSet -> ValueSet -> ErrorMessage
