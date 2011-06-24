{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module CSPM.TypeChecker.Expr () where

import Control.Monad
import Data.List

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.TypeChecker.Common
import {-# SOURCE #-} CSPM.TypeChecker.Decl
import CSPM.TypeChecker.Dependencies
import CSPM.TypeChecker.Exceptions
import CSPM.TypeChecker.Pat
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Unification
import Util.Annotated
import Util.List
import Util.Monad
import Util.PrettyPrint

checkFunctionCall :: Doc -> [PExp] -> [Type] -> TypeCheckMonad ()
checkFunctionCall func args expectedTypes = do
	actTypes <- mapM typeCheck args
	let
		as = zip4 [1..] args expectedTypes actTypes
		unifyArg (count, arg, texp, tact) = 
			setSrcSpan (loc arg) $ addErrorContext (
				hang (hang (text "In the" <+> speakNth count 
							<+> text "argument of")
							tabWidth (func <> comma) )
					tabWidth (text "namely" <+> prettyPrint arg)
			) (unify texp tact)
	-- NB. the argument counts must already be correct
	unifiedTArgs <- mapM unifyArg as
	mapM unifyArg as
	return ()

instance TypeCheckable PExp Type where
	errorContext an = Nothing
	typeCheck' an = do
		t <- setSrcSpan (loc an) $ typeCheck (inner an)
		setPType (annotation an) t
		return t

instance TypeCheckable Exp Type where
	errorContext e = Just $ 
		hang (text "In the expression:") tabWidth (prettyPrint e)
	typeCheck' (App f args) = do
		tFunc <- typeCheck f
		tr <- freshTypeVar
		texp <- replicateM (length args) freshTypeVar
		unify (TFunction texp tr) tFunc
		checkFunctionCall (prettyPrint f) args texp
		return tr
	typeCheck' (BooleanBinaryOp op e1 e2) = (do
		case op of
			And	-> checkFunctionCall (text "and") [e1,e2] [TBool, TBool]
			Or	-> checkFunctionCall (text "or") [e1,e2] [TBool, TBool]
			_ 	-> do
				t1 <- typeCheck e1
				t2 <- typeCheck e2
				t <- unify t1 t2
				case op of	
					Equals			-> ensureHasConstraint Eq t
					NotEquals		-> ensureHasConstraint Eq t
					LessThan		-> ensureHasConstraint Ord t
					LessThanEq		-> ensureHasConstraint Ord t
					GreaterThan		-> ensureHasConstraint Ord t
					GreaterThanEq	-> ensureHasConstraint Ord t
				return ())
		>> return TBool
	typeCheck' (BooleanUnaryOp op e1) = do
		t1 <- typeCheck e1
		ensureIsBool t1
		return TBool
	typeCheck' (Concat e1 e2) = do
		t1 <- typeCheck e1
		t2 <- typeCheck e2
		ensureIsList t1
		ensureIsList t2
		unify t1 t2
	typeCheck' (DotApp e1 e2) = do
		t1 <- typeCheck e1
		t2 <- typeCheck e2
		return $ TDot t1 t2
	typeCheck' (If e1 e2 e3) = do
		t1 <- typeCheck e1
		ensureIsBool t1
		t2 <- typeCheck e2
		t3 <- typeCheck e3
		unify t2 t3
	typeCheck' (Lambda p exp) = do
		fvs <- freeVars p
		local fvs (do
			tr <- typeCheck exp
			targ <- typeCheck p
			return $ TFunction [targ] tr)
	typeCheck' (Let decls exp) =
		-- Add a new scope: typeCheckDecl will add vars into it	
		local [] (do
				typeCheckDecls decls
				typeCheck exp)
	typeCheck' (Lit lit) = typeCheck lit
	typeCheck' (List es) = do
		ts <- mapM typeCheck es
		t <- unifyAll ts
		return $ TSeq t
	typeCheck' (ListComp es stmts) =
		typeCheckStmts TSeq stmts (do
					ts <- mapM typeCheck es
					t <- unifyAll ts
					return $ TSeq t)
	typeCheck' (ListEnumFrom lb) = do
		t1 <- typeCheck lb
		ensureIsInt t1
		return $ TSeq TInt
	typeCheck' (ListEnumFromTo lb ub) = do
		t1 <- typeCheck lb
		ensureIsInt t1
		t2 <- typeCheck ub
		ensureIsInt t1
		return $ TSeq TInt
	typeCheck' (ListLength e) = do
		t1 <- typeCheck e
		ensureIsList t1
		return $ TInt
	typeCheck' (MathsBinaryOp op e1 e2) = do
		t1 <- typeCheck e1
		t2 <- typeCheck e2
		ensureIsInt t1
		ensureIsInt t2
		return TInt
	typeCheck' (MathsUnaryOp op e1) = do
		t1 <- typeCheck e1
		ensureIsInt t1
		return TInt
	typeCheck' (Paren e) = typeCheck e
	typeCheck' (Set es) = do
		ts <- mapM typeCheck es
		t <- unifyAll ts
		ensureHasConstraint Eq t
		return $ TSet t
	typeCheck' (SetComp es stmts) = 
		typeCheckStmts TSet stmts (do
					ts <- mapM typeCheck es
					t <- unifyAll ts
					return $ TSet t)
	typeCheck' (SetEnum es) =  do
		ts <- mapM typeCheck es
		mapM ensureIsChannel ts
		return $ TSet TEvent
	typeCheck' (SetEnumComp es stmts) = 
		typeCheckStmts TSet stmts (do
					ts <- mapM typeCheck es
					mapM ensureIsChannel ts
					return $ TSet TEvent)
	typeCheck' (SetEnumFrom lb) = do
		t1 <- typeCheck lb
		ensureIsInt t1
		-- No need to check for Eq - Ints always are
		return $ TSet TInt
	typeCheck' (SetEnumFromTo lb ub) = do
		t1 <- typeCheck lb
		ensureIsInt t1
		t2 <- typeCheck ub
		ensureIsInt t2
		return $ TSet TInt
	typeCheck' (Tuple es) = do
		ts <- mapM typeCheck es
		return $ TTuple ts
	typeCheck' (Var (UnQual n)) = do
		t <- getType n
		instantiate t

	-- Processes
	typeCheck' (AlphaParallel e1 a1 a2 e2) = do
		t1 <- typeCheck e1
		ensureIsProc t1
		t2 <- typeCheck e2
		ensureIsProc t2
		t3 <- typeCheck a1
		unify t3 (TSet TEvent)
		t4 <- typeCheck a2
		unify t4 (TSet TEvent)
		return TProc
	typeCheck' (Exception e1 a e2) = do
		t1 <- typeCheck e1
		ensureIsProc t1
		t2 <- typeCheck e2
		ensureIsProc t2
		t3 <- typeCheck a
		unify t3 (TSet TEvent)
		return TProc
	typeCheck' (ExternalChoice e1 e2) = do
		t1 <- typeCheck e1
		ensureIsProc t1
		t2 <- typeCheck e2
		ensureIsProc t2
		return TProc
	typeCheck' (Hiding e1 e2) = do
		t1 <- typeCheck e1
		ensureIsProc t1
		t2 <- typeCheck e2
		unify t2 (TSet TEvent)
		return TProc
	typeCheck' (GenParallel e1 a e2) = do
		t1 <- typeCheck e1
		ensureIsProc t1
		t2 <- typeCheck e2
		ensureIsProc t2
		t3 <- typeCheck a
		unify t3 (TSet TEvent)
		return TProc
	typeCheck' (GuardedExp e1 e2) = do
		t1 <- typeCheck e1
		ensureIsBool t1
		t2 <- typeCheck e2
		ensureIsProc t2
		return TProc
	typeCheck' (InternalChoice e1 e2) = do
		t1 <- typeCheck e1
		ensureIsProc t1
		t2 <- typeCheck e2
		ensureIsProc t2
		return TProc
	typeCheck' (Interrupt e1 e2) = do
		t1 <- typeCheck e1
		ensureIsProc t1
		t2 <- typeCheck e2
		ensureIsProc t2
		return TProc
	typeCheck' (Interleave e1 e2) = do
		t1 <- typeCheck e1
		ensureIsProc t1
		t2 <- typeCheck e2
		ensureIsProc t2
		return TProc
	typeCheck' (SequentialComp e1 e2) = do
		t1 <- typeCheck e1
		ensureIsProc t1
		t2 <- typeCheck e2
		ensureIsProc t2
		return TProc
	typeCheck' (SlidingChoice e1 e2) = do
		t1 <- typeCheck e1
		ensureIsProc t1
		t2 <- typeCheck e2
		ensureIsProc t2
		return TProc
	typeCheck' (Prefix e1 [] e2) = do
		t1 <- typeCheck e1
		ensureIsEvent t1
		t2 <- typeCheck e2
		ensureIsProc t2
		return TProc
	typeCheck' (Prefix e1 fields e2) = do
		fvs <- concatMapM freeVars fields
-- TODO
--		errorIfFalse (noDups fvs) (DuplicatedDefinitions fvs)
		t1 <- typeCheck e1
		let 
			tcfs [] tsfields = do
				unify (TDot t1 (foldr1 TDot (reverse tsfields))) TEvent
				t2 <- typeCheck e2
				ensureIsProc t2
				return TProc
			tcfs (f:fs) tsfields =
				typeCheckField f (\ t -> tcfs fs (t:tsfields))

		tcfs fields []
			
	-- Replicated Operators
	typeCheck' (ReplicatedAlphaParallel stmts alpha proc) =
		typeCheckReplicatedOp stmts (do
			t1 <- typeCheck alpha
			unify t1 (TSet TEvent)
			t2 <- typeCheck proc
			ensureIsProc t2
			return TProc)
	typeCheck' (Rename e1 exps stmts) = 
		typeCheckReplicatedOp stmts (do
			t1 <- typeCheck e1
			ensureIsProc t1
			let (as, bs) = unzip exps
			-- Unify the pairs of channels
			ast <- mapM typeCheck as
			bst <- mapM typeCheck bs
			ts <- zipWithM unify ast bst
			mapM ensureIsChannel ts
			return TProc)
	typeCheck' (ReplicatedParallel alpha stmts proc) =
		typeCheckReplicatedOp stmts (do
			talpha <- typeCheck alpha
			unify talpha (TSet TEvent)
			t1 <- typeCheck proc
			ensureIsProc t1
			return TProc)
	typeCheck' (ReplicatedInterleave stmts e1) =
		typeCheckReplicatedOp stmts (do
			t1 <- typeCheck e1
			ensureIsProc t1
			return TProc)
	typeCheck' (ReplicatedExternalChoice stmts e1) =
		typeCheckReplicatedOp stmts (do
			t1 <- typeCheck e1
			ensureIsProc t1
			return TProc)
	typeCheck' (ReplicatedInternalChoice stmts e1) =
		typeCheckReplicatedOp stmts (do
			t1 <- typeCheck e1
			ensureIsProc t1
			return TProc)
	typeCheck' x = panic ("No case for type checking a "++show x)


typeCheckField :: PField -> (Type -> TypeCheckMonad a) -> TypeCheckMonad a
typeCheckField field tc = setSrcSpan (loc field) $ addErrorContext (
		hang (text "In the field:") tabWidth (prettyPrint field)
	) $ typeCheckField' (unAnnotate field) tc
typeCheckField' (Input p (Just e)) tc = do
	t <- typeCheck e
	fvs <- freeVars p
	local fvs $ do
		tp <- typeCheck p
		unify (TSet tp) t
		tc tp
typeCheckField' (Input p Nothing) tc = do
	fvs <- freeVars p
	local fvs (typeCheck p >>= tc)
typeCheckField' (Output e) tc = typeCheck e >>= tc

-- The first argument is a type constructor, which given a type, returns
-- that type encapsulate in some other type.
typeCheckStmt :: (Type -> Type) -> PStmt -> TypeCheckMonad a -> TypeCheckMonad a
typeCheckStmt typc stmt tc = setSrcSpan (loc stmt) $ addErrorContext (
		hang (text "In the statement of a comprehension:") tabWidth
			(prettyPrint stmt)
	) $ typeCheckStmt' typc (unAnnotate stmt) tc
typeCheckStmt' typc (Qualifier e) tc = do
	t <- typeCheck e
	ensureIsBool t
	tc
typeCheckStmt' typc (Generator p exp) tc = do
	texp <- typeCheck exp
	fvs <- freeVars p
	local fvs (do
		tpat <- typeCheck p
		unify (typc tpat) texp
		tc)

-- Type check a series of statements. For each statement a new scope is added
-- to ensure that clauses only depend on variables already bound.
typeCheckStmts :: (Type -> Type) -> [AnStmt] -> TypeCheckMonad a -> TypeCheckMonad a
typeCheckStmts typc stmts tc = do
		fvs <- concatMapM freeVars stmts
		-- TODO: should this be an error?
-- TODO: do this
--		errorIfFalse (noDups fvs) (DuplicatedDefinitions fvs)
		check stmts
	where
		check [] = tc
		check (stmt:stmts) = typeCheckStmt typc stmt (check stmts)

-- Shortcut for replicated operators
typeCheckReplicatedOp :: [AnStmt] -> TypeCheckMonad a -> TypeCheckMonad a
typeCheckReplicatedOp = typeCheckStmts TSet
