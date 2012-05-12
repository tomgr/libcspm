{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    TypeSynonymInstances #-}
module CSPM.TypeChecker.InteractiveStmt where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Dependencies
import CSPM.TypeChecker.Decl
import CSPM.TypeChecker.Expr
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Unification
import Util.Annotated
import Util.PrettyPrint

instance (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
            TypeCheckable (p Name) Type)
        => TypeCheckable (TCInteractiveStmt p) () where
    errorContext a = Nothing
    typeCheck' = typeCheck . unAnnotate

instance (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
            TypeCheckable (p Name) Type)
        => TypeCheckable (InteractiveStmt Name p) () where
    errorContext a = Nothing
    typeCheck' (Bind decl) = typeCheckDecls [decl]
    typeCheck' (Evaluate exp) = 
        typeCheck exp >>= evaluateDots >> return ()
    typeCheck' (RunAssertion a) = typeCheck a >> return ()
