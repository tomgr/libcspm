module CSPM.TypeChecker.Decl where

import CSPM.Syntax.AST
import CSPM.TypeChecker.Monad

typeCheckDecls :: Bool -> Bool -> [TCDecl] -> TypeCheckMonad ()
