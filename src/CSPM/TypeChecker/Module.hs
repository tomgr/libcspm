{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    TypeSynonymInstances #-}
module CSPM.TypeChecker.Module where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Dependencies
import CSPM.TypeChecker.Decl
import CSPM.TypeChecker.Monad
import Util.Annotated
import Util.PrettyPrint

instance (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
            TypeCheckable (p Name) Type)
        => TypeCheckable [TCModule p] () where
    errorContext _ = Nothing
    typeCheck' ([m]) = typeCheck m

instance (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
            TypeCheckable (p Name) Type)
        => TypeCheckable (TCModule p) () where
    errorContext _ = Nothing
    typeCheck' an =  setSrcSpan (loc an) $ typeCheck' (inner an)

instance (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
            TypeCheckable (p Name) Type)
        => TypeCheckable (Module Name p) () where
    errorContext _ = Nothing
    typeCheck' (GlobalModule ds) = typeCheckDecls ds
