{-# LANGUAGE TypeSynonymInstances #-}
module CSPM.Evaluator.File (
    bindFile
) 
where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.AnalyserMonad
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import Util.Annotated

bindFile :: TCCSPMFile -> AnalyserMonad (EvaluationMonad [(Name, EvaluationMonad Value)])
bindFile (An _ _ (CSPMFile ds)) = bindDecls ds
