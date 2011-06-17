module CSPMTypeChecker.Decl where

import CSPMDataStructures.Syntax
import CSPMDataStructures.Types
import CSPMTypeChecker.Monad

typeCheckDecls :: [PDecl] -> TypeCheckMonad ()
