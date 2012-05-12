{-# LANGUAGE FlexibleContexts, TypeSynonymInstances #-}
module CSPM.Evaluator.Module (
    bindModules, bindModule,
) 
where

import Control.Monad

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Expr
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import Util.Annotated
import Util.Monad
import Util.PrettyPrint

bindModules :: (Evaluatable (TCExp p) ops, PrettyPrintable (UProc ops)) => 
    [TCModule p] -> EvaluationMonad ops [(Name, EvaluationMonad ops (Value ops))]
bindModules ms = concatMapM bindModule ms

bindModule :: (Evaluatable (TCExp p) ops, PrettyPrintable (UProc ops)) => 
    TCModule p-> EvaluationMonad ops [(Name, EvaluationMonad ops (Value ops))]
bindModule an = case unAnnotate an of
    GlobalModule ds -> bindDecls ds
