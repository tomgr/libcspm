module CSPM.TypeChecker.Decl where

import CSPM.DataStructures.Syntax
import CSPM.TypeChecker.Monad

typeCheckDecls :: [TCDecl] -> TypeCheckMonad ()
