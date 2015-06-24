{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}
module CSPM.TypeChecker.Common where

import CSPM.Syntax.Literals
import CSPM.Syntax.Types
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Unification

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
    errorContext _ = Nothing
    typeCheck' (Int _) = return TInt
    typeCheck' (Bool _) = return TBool
    typeCheck' (Char _) = return TChar
    typeCheck' (String _) = return $ TSeq TChar
    
ensureAreEqual :: TypeCheckable a Type => [a] -> TypeCheckMonad Type
ensureAreEqual [] = freshRegisteredTypeVar
ensureAreEqual (e:es) = do
    t <- typeCheck e
    mapM (\e -> typeCheckExpect e t) es
    return t

ensureAreEqualTo :: TypeCheckable a Type => Type -> [a] -> TypeCheckMonad Type
ensureAreEqualTo typ es = do
    mapM (\e -> typeCheckExpect e typ) es
    return typ

ensureAreEqualAndHaveConstraint :: TypeCheckable a Type => Constraint -> [a] -> TypeCheckMonad Type
ensureAreEqualAndHaveConstraint c es = do
    fv1 <- freshRegisteredTypeVarWithConstraints [c]
    ensureAreEqualTo fv1 es

ensureIsList :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsList e = do
    fv <- freshRegisteredTypeVar
    typeCheckExpect e (TSeq fv)

ensureIsSet :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsSet e = do
    fv <- freshRegisteredTypeVarWithConstraints [CSet]
    typeCheckExpect e (TSet fv)

ensureIsBool :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsBool e = typeCheckExpect e TBool

ensureIsInt :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsInt e = typeCheckExpect e TInt

ensureIsChannel :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsChannel e = ensureIsExtendable e TEvent

ensureIsExtendable :: TypeCheckable a b => a -> Type -> TypeCheckMonad b
ensureIsExtendable e t = do
    tvref <- freshRegisteredTypeVarRef []
    typeCheckExpect e (TExtendable t tvref)

ensureIsEvent :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsEvent e = typeCheckExpect e TEvent

ensureIsProc :: TypeCheckable a b => a -> TypeCheckMonad b
ensureIsProc e = typeCheckExpect e TProc

ensureHasConstraint :: Constraint -> Type -> TypeCheckMonad Type
ensureHasConstraint c = ensureHasConstraints [c]

ensureHasConstraints :: [Constraint] -> Type -> TypeCheckMonad Type
ensureHasConstraints cs t = do
    fv1 <- freshRegisteredTypeVarWithConstraints cs
    unify fv1 t
