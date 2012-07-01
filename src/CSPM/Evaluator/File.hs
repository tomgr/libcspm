{-# LANGUAGE TypeSynonymInstances #-}
module CSPM.Evaluator.File (
    bindFile
) 
where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import Util.Annotated

bindFile :: TCCSPMFile -> EvaluationMonad [(Name, EvaluationMonad Value)]
bindFile an = case unAnnotate an of
    CSPMFile ds -> bindDecls ds
