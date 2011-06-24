{-# LANGUAGE DeriveDataTypeable #-}
module CSPM.Evaluator.Exceptions (
	module Util.Exception,
	EvaluationException(..),
)
where

import Data.Typeable
import Prelude

import CSPM.DataStructures.Syntax
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Exception

data EvaluationException = 
	-- | An error that should have been caught by the type checker
	TypeCheckerException String
	| PatternMatchException TCPat Value
	| UnknownException String
	deriving (Show, Typeable)

instance Exception EvaluationException
