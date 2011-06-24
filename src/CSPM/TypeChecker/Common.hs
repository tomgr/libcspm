{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module CSPM.TypeChecker.Common where

import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.TypeChecker.BuiltInFunctions
import CSPM.TypeChecker.Exceptions
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Unification
import Util.PrettyPrint

-- a -> b <=> We can't have two instances that match on a but differ on b
class TypeCheckable a b | a -> b where
	typeCheck :: a -> TypeCheckMonad b
	typeCheck a = 
		case errorContext a of
			Just c -> addErrorContext c (typeCheck' a)
			Nothing -> typeCheck' a
	typeCheck' :: a -> TypeCheckMonad b
	errorContext :: a -> Maybe ErrorContext

instance TypeCheckable Literal Type where
	errorContext a = Nothing
	typeCheck' (Int n) = return TInt
	typeCheck' (Bool b) = return TBool

-- *************************************************************************
-- Helper methods
-- *************************************************************************
ensureIsList :: Type -> TypeCheckMonad Type
ensureIsList typ = do
	fv <- freshTypeVar
	unify (TSeq fv) typ

ensureIsSet :: Type -> TypeCheckMonad Type
ensureIsSet typ = do
	fv <- freshTypeVarWithConstraints [Eq]
	unify (TSet fv) typ

ensureIsBool :: Type -> TypeCheckMonad Type
ensureIsBool typ = unify TBool typ

ensureIsInt :: Type -> TypeCheckMonad Type
ensureIsInt typ = unify TInt typ

ensureIsChannel :: Type -> TypeCheckMonad Type
ensureIsChannel t = unify TEventable t

ensureIsEvent :: Type -> TypeCheckMonad Type
ensureIsEvent t = unify TEvent t

ensureHasConstraint :: Constraint -> Type -> TypeCheckMonad Type
ensureHasConstraint c t = do
	fv1 <- freshTypeVarWithConstraints [c]
	unify fv1 t

ensureIsProc :: Type -> TypeCheckMonad Type
ensureIsProc t = unify TProc t
