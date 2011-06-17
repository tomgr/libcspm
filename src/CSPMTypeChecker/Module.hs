{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module CSPMTypeChecker.Module where

import CSPMDataStructures.Syntax
import CSPMDataStructures.Types
import CSPMTypeChecker.Common
import CSPMTypeChecker.Decl
import CSPMTypeChecker.Monad
import Util.Annotated

instance TypeCheckable [PModule] () where
	errorConstructor an = id
	typeCheck' ([m]) = typeCheck m

instance TypeCheckable PModule () where
	errorConstructor = ErrorWithModule
	typeCheck' (An _ _ m) = typeCheck' m

instance TypeCheckable Module () where
	errorConstructor = error "Error: Module error constructor called"
	typeCheck' (GlobalModule ds) = 
		typeCheckDecls ds


typeCheckModules :: [PModule] -> TypeCheckMonad [TCModule]
typeCheckModules  ms =
	do
		typeCheckWrapper ms
		return ms