{-# LANGUAGE FlexibleInstances #-}
module CSPM.Evaluator.Expr where

import CSPM.Syntax.AST
import CSPM.Evaluator.AnalyserMonad
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values

eval :: TCExp -> AnalyserMonad (EvaluationMonad Value)
