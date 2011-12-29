{-# LANGUAGE TypeSynonymInstances #-}
module CSPM.Evaluator.Module (
    bindModules, bindModule,
) 
where

import Control.Monad

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import Util.Annotated

bindModules :: [TCModule] -> [(Name, EvaluationMonad Value)]
bindModules ms = concatMap bindModule ms

bindModule :: TCModule -> [(Name, EvaluationMonad Value)]
bindModule an = case unAnnotate an of
    GlobalModule ds -> bindDecls ds
