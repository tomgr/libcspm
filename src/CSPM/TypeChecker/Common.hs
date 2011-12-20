{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}
module CSPM.TypeChecker.Common where

import CSPM.DataStructures.Literals
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
    
    typeCheckExpect :: a -> Type -> TypeCheckMonad b
    typeCheckExpect _ _ = panic "typeCheckExpect not supported"
    
    typeCheck' :: a -> TypeCheckMonad b
    errorContext :: a -> Maybe ErrorContext

instance TypeCheckable Literal Type where
    errorContext a = Nothing
    typeCheck' (Int n) = return TInt
    typeCheck' (Bool b) = return TBool
    
ensureAreEqual :: TypeCheckable a Type => [a] -> TypeCheckMonad Type
ensureAreEqual [] = freshTypeVar
ensureAreEqual (e:es) = do
    t <- typeCheck e
    mapM (\e -> typeCheckExpect e t) es
    return t

ensureIsList :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsList e = do
    fv <- freshTypeVar
    typeCheckExpect e (TSeq fv)

ensureIsSet :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsSet e = do
    fv <- freshTypeVarWithConstraints [Eq]
    typeCheckExpect e (TSet fv)

ensureIsBool :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsBool e = typeCheckExpect e TBool

ensureIsInt :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsInt e = typeCheckExpect e TInt

ensureIsChannel :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsChannel e = typeCheckExpect e TEventable

ensureIsEvent :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsEvent e = typeCheckExpect e TEvent

ensureIsProc :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsProc e = typeCheckExpect e TProc

ensureHasConstraint :: Constraint -> Type -> TypeCheckMonad Type
ensureHasConstraint c t = do
    fv1 <- freshTypeVarWithConstraints [c]
    unify fv1 t

