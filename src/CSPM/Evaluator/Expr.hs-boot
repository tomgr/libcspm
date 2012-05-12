{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, 
    UndecidableInstances #-}
module CSPM.Evaluator.Expr where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import Util.Annotated
import Util.PrettyPrint

class Evaluatable a ops where
    eval :: a -> EvaluationMonad ops (Value ops)
    
instance Evaluatable a ops => Evaluatable (Annotated b a) ops
instance (Evaluatable (p Name) ops, PrettyPrintable (UProc ops)) => 
    Evaluatable (Exp Name p) ops

completeEvent :: Value ops -> EvaluationMonad ops (ValueSet ops)
