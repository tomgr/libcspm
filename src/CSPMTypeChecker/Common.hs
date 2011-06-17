{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module CSPMTypeChecker.Common where

import CSPMDataStructures.Syntax
import CSPMDataStructures.Types
import CSPMTypeChecker.BuiltInFunctions
import CSPMTypeChecker.Monad
import CSPMTypeChecker.Unification

typeCheckWrapper :: TypeCheckable a b => a -> TypeCheckMonad b
typeCheckWrapper tc =
	do
		injectBuiltInFunctions
		-- We add an extra scope layer here to allow the built in functions
		-- to be overloaded. TODO: should we allow this?
		local [] (typeCheck tc)

-- *************************************************************************
-- Helper methods
-- *************************************************************************
ensureIsList :: Type -> TypeCheckMonad Type
ensureIsList typ =
	do
		fv <- freshTypeVar
		unify (TSeq fv) typ

ensureIsSet :: Type -> TypeCheckMonad Type
ensureIsSet typ =
	do
		fv <- freshTypeVar
		ensureHasConstraint Eq fv
		unify (TSet fv) typ

ensureIsBool :: Type -> TypeCheckMonad Type
ensureIsBool typ = unify (TBool) typ

ensureIsInt :: Type -> TypeCheckMonad Type
ensureIsInt typ = unify TInt typ

ensureIsChannel :: Type -> TypeCheckMonad Type
ensureIsChannel t = unify t TEventable

ensureIsEvent :: Type -> TypeCheckMonad Type
ensureIsEvent t = unify t TEvent

ensureHasConstraint :: Constraint -> Type -> TypeCheckMonad Type
ensureHasConstraint c t = 
	do
		fv1 <- freshTypeVarWithConstraints [c]
		unify fv1 t

ensureIsProc :: Type -> TypeCheckMonad Type
ensureIsProc t = unify TProc t

-- See http://www.haskell.org/haskellwiki/Functional_dependencies for more
-- descriptions on a -> b. Essentially it says that a determines the type
-- b (i.e. we can't have two instances that match on a but differ on b)
class TypeCheckable a b | a -> b where
	typeCheck :: a -> TypeCheckMonad b
	typeCheck a = typeCheck' a
		`catchError` (\ e -> throwError $ errorConstructor a e)
	
	errorConstructor :: a -> (TypeCheckError -> TypeCheckError)
	typeCheck' :: a -> TypeCheckMonad b

instance TypeCheckable Literal Type where
	errorConstructor = error "No error should ever occur in a literal"
	typeCheck' (Int n) = return TInt
	typeCheck' (Bool b) = return TBool
