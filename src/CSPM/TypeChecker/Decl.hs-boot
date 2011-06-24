module CSPM.TypeChecker.Decl where

import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.TypeChecker.Monad

typeCheckDecls :: [PDecl] -> TypeCheckMonad ()
