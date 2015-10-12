{-# LANGUAGE FlexibleInstances #-}
module CSPM.Evaluator.Expr where

import CSPM.Syntax.AST
import CSPM.Syntax.Names
import CSPM.Evaluator.AnalyserMonad
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values

eval :: TCExp -> AnalyserMonad (EvaluationMonad Value)
evalProc :: TCExp -> AnalyserMonad (EvaluationMonad Proc)
makeAnnotater :: TCExp -> AnalyserMonad ([(Name, Value)] -> Proc -> EvaluationMonad Proc)
