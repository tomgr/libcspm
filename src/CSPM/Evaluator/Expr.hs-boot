{-# LANGUAGE FlexibleInstances #-}
module CSPM.Evaluator.Expr where

import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values

eval :: TCExp -> EvaluationMonad Value
