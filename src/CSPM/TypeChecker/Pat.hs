{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module CSPM.TypeChecker.Pat () where

import CSPM.Syntax.FreeVars
import CSPM.Syntax.Names
import CSPM.Syntax.AST hiding (getType)
import CSPM.Syntax.Types
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Unification
import Prelude hiding ((<>))
import Util.Annotated
import Util.PrettyPrint
    
instance TypeCheckable TCPat Type where
    errorContext an = Nothing
    typeCheck' an = do
        t <- setSrcSpan (loc an) $ typeCheck (inner an)
        setPType (snd (annotation an)) t
        return t
    typeCheckExpect an typ = do
        t <- setSrcSpan (loc an) $ typeCheckExpect (inner an) typ
        setPType (snd (annotation an)) t
        return t
instance TypeCheckable (Pat Name) Type where
    typeCheckExpect obj texp =
        case errorContext obj of
            Just c -> addErrorContext c m
            Nothing -> m
        where
            m = do
                tact <- typeCheck' obj
                unify texp tact

    errorContext p = Just $
        (hang (text "In the pattern" <> colon) tabWidth (prettyPrint p),
            freeVars p)

    typeCheck' (PConcat p1 p2) = do
        t <- ensureIsList p1
        typeCheckExpect p2 t
    typeCheck' (PDoublePattern p1 p2) = do
        t <- typeCheck p1
        typeCheckExpect p2 t
    typeCheck' (PDotApp p1 p2) = do
        t1 <- typeCheck p1
        t2 <- typeCheck p2
        return $ TDot t1 t2
    typeCheck' (PList ps) = do
        t <- ensureAreEqual ps
        return $ TSeq t
    typeCheck' (PLit lit) = typeCheck lit
    typeCheck' (PParen p1) = typeCheck p1
    typeCheck' (PSet ps) = do
        t <- ensureAreEqual ps
        -- We require CEq here, not CSet as otherwise this would allow processes
        -- to be compared for equality.
        ensureHasConstraint CEq t
        return $ TSet t
    typeCheck' (PTuple ps) = do
        ts <- mapM typeCheck ps
        return $ TTuple ts
    typeCheck' (PWildCard) = freshRegisteredTypeVar
    typeCheck' (PVar n) = do
        t@(ForAll _ t') <- getType n
        -- All variables are already in scope hence we can just return the
        -- type (since we always typeCheck a pattern in between a 
        -- local (freeVars pat)).
        return t'
