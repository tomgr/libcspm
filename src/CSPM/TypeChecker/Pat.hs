{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module CSPM.TypeChecker.Pat () where

import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Unification
import Util.Annotated
import Util.PrettyPrint
	
instance TypeCheckable PPat Type where
	errorContext an = Nothing
	typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)
instance TypeCheckable Pat Type where
	errorContext p = Just $
		hang (text "In the pattern" <> colon) tabWidth (prettyPrint p)
	typeCheck' (PConcat p1 p2) = do
		t1 <- typeCheck p1
		t1 <- ensureIsList t1
		t2 <- typeCheck p2
		t2 <- ensureIsList t2
		unify t1 t2
	typeCheck' (PDoublePattern p1 p2) = do
		t1 <- typeCheck p1
		t2 <- typeCheck p2
		unify t1 t2
	typeCheck' (PDotApp p1 p2) = do
		t1 <- typeCheck p1
		t2 <- typeCheck p2
		return $ TDot t1 t2
	typeCheck' (PList ps) = do
		ts <- mapM typeCheck ps
		t <- unifyAll ts
		return $ TSeq t
	typeCheck' (PLit lit) = typeCheck lit
	typeCheck' (PParen p1) = typeCheck p1
	typeCheck' (PSet ps) = do
-- TODO: add to desugarer
--		errorIfFalse (length ps <= 1) (InvalidSetPattern ps)
		ts <- mapM typeCheck ps
		t <- unifyAll ts
		ensureHasConstraint Eq t
		return $ TSet t
	typeCheck' (PTuple ps) = do
		ts <- mapM typeCheck ps
		return $ TTuple ts
	typeCheck' (PWildCard) = freshTypeVar
	typeCheck' (PVar n) = do
		t @ (ForAll _ t') <- getType n
		-- All variables are already in scope hence we can just return the
		-- type (since we always typeCheck a pattern in between a 
		-- local (freeVars pat)).
		return t'
