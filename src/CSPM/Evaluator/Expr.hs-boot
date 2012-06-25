{-# LANGUAGE FlexibleInstances #-}
module CSPM.Evaluator.Expr where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import Util.Annotated

class Evaluatable a where
    eval :: a -> EvaluationMonad Value
    
instance Evaluatable a => Evaluatable (Annotated b a)
instance Evaluatable (Exp Name)
