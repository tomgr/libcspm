{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module CSPM.TypeChecker.InteractiveStmt where

import CSPM.Syntax.Names
import CSPM.Syntax.AST
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Decl
import CSPM.TypeChecker.Expr()
import CSPM.TypeChecker.Unification
import Util.Annotated

instance TypeCheckable TCInteractiveStmt () where
    errorContext a = Nothing
    typeCheck' = typeCheck . unAnnotate

instance TypeCheckable (InteractiveStmt Name) () where
    errorContext a = Nothing
    typeCheck' (Bind decls) = typeCheckDecls True True decls
    typeCheck' (Evaluate exp) = 
        typeCheck exp >>= evaluateDots >> return ()
    typeCheck' (RunAssertion a) = typeCheck a >> return ()
