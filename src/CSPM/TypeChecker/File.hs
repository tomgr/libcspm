{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module CSPM.TypeChecker.File where

import CSPM.Syntax.Names
import CSPM.Syntax.AST
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Decl
import CSPM.TypeChecker.Monad
import Util.Annotated

instance TypeCheckable (AnCSPMFile Name) () where
    errorContext _ = Nothing
    typeCheck' an =  setSrcSpan (loc an) $ typeCheck' (inner an)

instance TypeCheckable (CSPMFile Name) () where
    errorContext _ = Nothing
    typeCheck' (CSPMFile ds) = typeCheckDecls True True ds
