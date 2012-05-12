{-# LANGUAGE FlexibleContexts #-}
module CSPM.TypeChecker.Decl where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Dependencies
import CSPM.TypeChecker.Monad
import Util.PrettyPrint

typeCheckDecls :: 
    (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
        TypeCheckable (p Name) Type) => [TCDecl p] -> TypeCheckMonad ()
