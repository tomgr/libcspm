{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module CSPM.TypeChecker.Module where

import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Decl
import CSPM.TypeChecker.Monad
import Util.Annotated

instance TypeCheckable [PModule] () where
	errorContext _ = Nothing
	typeCheck' ([m]) = typeCheck m

instance TypeCheckable PModule () where
	errorContext _ = Nothing
	typeCheck' an =  setSrcSpan (loc an) $ typeCheck' (inner an)

instance TypeCheckable Module () where
	errorContext _ = Nothing
	typeCheck' (GlobalModule ds) = typeCheckDecls ds
