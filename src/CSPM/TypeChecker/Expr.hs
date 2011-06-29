{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, DoAndIfThenElse #-}
module CSPM.TypeChecker.Expr () where

import Control.Monad
import Control.Monad.Trans
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
	mapM unifyArg as
	return ()

instance TypeCheckable PExp Type where
	errorContext an = Nothing
	typeCheck' an = do
		t <- setSrcSpan (loc an) $ typeCheck (inner an)
		setPType (annotation an) t
		return t
	typeCheckExpect an texp = do
		-- The unification is done in typeCheck (inner an)
		t <- setSrcSpan (loc an) $ typeCheckExpect (inner an) texp
		setPType (annotation an) t
		return t
instance TypeCheckable Exp Type where
	typeCheckExpect obj texp =
		case errorContext obj of
			Just c -> addErrorContext c m
			Nothing -> m
		where
			m = do
				tact <- typeCheck' obj
				unify texp tact

	errorContext e = Just $ 
		hang (text "In the expression:") tabWidth (prettyPrint e)
	
	typeCheck' (App f args) = do
		targs <- replicateM (length args) freshTypeVar
		tr <- freshTypeVar
		typeCheckExpect f (TFunction targs tr)
		checkFunctionCall (prettyPrint f) args targs
		return tr
	typeCheck' (BooleanBinaryOp op e1 e2) = (do
		case op of
			And	-> checkFunctionCall (text "and") [e1,e2] [TBool, TBool]
			Or	-> checkFunctionCall (text "or") [e1,e2] [TBool, TBool]
			_ 	-> do
				t <- typeCheck e1
				t <- typeCheckExpect e2 t
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
		ensureIsBool e1
	typeCheck' (Concat e1 e2) = do
		t1 <- ensureIsList e1
		typeCheckExpect e2 t1
	typeCheck' (DotApp e1 e2) = do
		t1 <- typeCheck e1
		t2 <- typeCheck e2
		return $ TDot t1 t2
	typeCheck' (If e1 e2 e3) = do
		ensureIsBool e1
		t2 <- typeCheck e2
		typeCheckExpect e3 t2
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
		t <- ensureAreEqual es
		return $ TSeq t
	typeCheck' (ListComp es stmts) =
		typeCheckStmts TSeq stmts (do
			t <- ensureAreEqual es
			return $ TSeq t)
	typeCheck' (ListEnumFrom lb) = do
		ensureIsInt lb
		return $ TSeq TInt
	typeCheck' (ListEnumFromTo lb ub) = do
		ensureIsInt lb
		ensureIsInt ub
		return $ TSeq TInt
	typeCheck' (ListLength e) = do
		ensureIsList e
		return $ TInt
	typeCheck' (MathsBinaryOp op e1 e2) = do
		ensureIsInt e1
		ensureIsInt e2
		return TInt
	typeCheck' (MathsUnaryOp op e1) = do
		ensureIsInt e1
		return TInt
	typeCheck' (Paren e) = typeCheck e
	typeCheck' (Set es) = do
		t <- ensureAreEqual es
		ensureHasConstraint Eq t
		return $ TSet t
	typeCheck' (SetComp es stmts) = 
		typeCheckStmts TSet stmts (do
			t <- ensureAreEqual es
			return $ TSet t)
	typeCheck' (SetEnum es) =  do
		mapM ensureIsChannel es
		return $ TSet TEvent
	typeCheck' (SetEnumComp es stmts) = 
		typeCheckStmts TSet stmts (do
			mapM ensureIsChannel es
			return $ TSet TEvent)
	typeCheck' (SetEnumFrom lb) = do
		ensureIsInt lb
		return $ TSet TInt
	typeCheck' (SetEnumFromTo lb ub) = do
		ensureIsInt lb
		ensureIsInt ub
		return $ TSet TInt
	typeCheck' (Tuple es) = do
		ts <- mapM typeCheck es
		return $ TTuple ts
	typeCheck' (Var (UnQual n)) = do
		t <- getType n
		instantiate t

	-- Processes
	typeCheck' (AlphaParallel e1 a1 a2 e2) = do
		ensureIsProc e1
		ensureIsProc e2
		typeCheckExpect a1 (TSet TEvent)
		typeCheckExpect a2 (TSet TEvent)
		return TProc
	typeCheck' (Exception e1 a e2) = do
		ensureIsProc e1
		ensureIsProc e2
		typeCheckExpect a (TSet TEvent)
		return TProc
	typeCheck' (ExternalChoice e1 e2) = do
		ensureIsProc e1
		ensureIsProc e2
		return TProc
	typeCheck' (Hiding e1 e2) = do
		ensureIsProc e1
		typeCheckExpect e2 (TSet TEvent)
		return TProc
	typeCheck' (GenParallel e1 a e2) = do
		ensureIsProc e1
		ensureIsProc e2
		typeCheckExpect a (TSet TEvent)
		return TProc
	typeCheck' (GuardedExp e1 e2) = do
		ensureIsBool e1
		ensureIsProc e2
		return TProc
	typeCheck' (InternalChoice e1 e2) = do
		ensureIsProc e1
		ensureIsProc e2
		return TProc
	typeCheck' (Interrupt e1 e2) = do
		ensureIsProc e1
		ensureIsProc e2
		return TProc
	typeCheck' (Interleave e1 e2) = do
		ensureIsProc e1
		ensureIsProc e2
		return TProc		
	typeCheck' (SequentialComp e1 e2) = do
		ensureIsProc e1
		ensureIsProc e2
		return TProc
	typeCheck' (SlidingChoice e1 e2) = do
		ensureIsProc e1
		ensureIsProc e2
		return TProc
	typeCheck' (Prefix e1 [] e2) = do
		ensureIsEvent e1
		ensureIsProc e2
		return TProc
	typeCheck' (Prefix e1 fields e2) = do
		fvsByField <- mapM (\f -> do
				fvs <- freeVars f
				return (f, fvs)) fields
		let 
			fvs = concatMap snd fvsByField
			namesToLocations = 
				[(n, loc f) | (f, fvs) <- fvsByField, n <- fvs]
		-- Throw an error if a name is defined multiple times
		manyErrorsIfFalse (noDups fvs) 
			(duplicatedDefinitionsMessage namesToLocations)
		t1 <- typeCheck e1
		let 
			tcfs [] tsfields = do
				unify (TDot t1 (foldr1 TDot (reverse tsfields))) TEvent
				ensureIsProc e2
			tcfs (f:fs) tsfields =
				typeCheckField f (\ t -> tcfs fs (t:tsfields))
		tcfs fields []

	typeCheck' (LinkParallel e1 ties stmts e2) = do
		ensureIsProc e1
		ensureIsProc e2
		typeCheckReplicatedOp stmts $ do
			let (as, bs) = unzip ties
			ast <- mapM ensureIsChannel as
			bst <- mapM ensureIsChannel bs
			ts <- zipWithM unify ast bst
			return TProc

	typeCheck' (Rename e1 exps stmts) = do
		ensureIsProc e1
		typeCheckReplicatedOp stmts (do
			let (as, bs) = unzip exps
			-- Unify the pairs of channels
			-- TODO: error contexts
			ast <- mapM ensureIsChannel as
			bst <- mapM ensureIsChannel bs
			ts <- zipWithM unify ast bst
			return TProc)
			
	-- Replicated Operators
	typeCheck' (ReplicatedAlphaParallel stmts alpha proc) =
		typeCheckReplicatedOp stmts (do
			t1 <- typeCheck alpha
			unify t1 (TSet TEvent)
			ensureIsProc proc)
	typeCheck' (ReplicatedParallel alpha stmts proc) =
		typeCheckReplicatedOp stmts (do
			typeCheckExpect alpha (TSet TEvent)
			ensureIsProc proc)
	typeCheck' (ReplicatedLinkParallel ties stmts proc) = 
		typeCheckStmts TSeq stmts $ do
			let (as, bs) = unzip ties
			ast <- mapM ensureIsChannel as
			bst <- mapM ensureIsChannel bs
			ts <- zipWithM unify ast bst
			ensureIsProc proc
	typeCheck' (ReplicatedInterleave stmts e1) =
		typeCheckReplicatedOp stmts (ensureIsProc e1)
	typeCheck' (ReplicatedExternalChoice stmts e1) =
		typeCheckReplicatedOp stmts (ensureIsProc e1)
	typeCheck' (ReplicatedInternalChoice stmts e1) =
		typeCheckReplicatedOp stmts (ensureIsProc e1)
	typeCheck' x = panic ("No case for type checking a "++show x)


typeCheckField :: PField -> (Type -> TypeCheckMonad a) -> TypeCheckMonad a
typeCheckField field tc = 
	let
		errCtxt = hang (text "In the field:") tabWidth (prettyPrint field)
		check (Input p (Just e)) = do
			t <- typeCheck e
			fvs <- freeVars p
			local fvs $ do
				tp <- addErrorContext errCtxt (do
						tp <- typeCheck p
						unify (TSet tp) t
						return tp
					)
				tc tp
		check (Input p Nothing) = do
			fvs <- freeVars p
			local fvs (addErrorContext errCtxt (typeCheck p) >>= tc)
		check (Output e) = do
			addErrorContext errCtxt (typeCheck e) >>= tc
	in setSrcSpan (loc field) (check (unAnnotate field))

-- | The first argument is a type constructor, which given a type, returns
-- that type encapsulate in some other type.
typeCheckStmt :: (Type -> Type) -> PStmt -> TypeCheckMonad a -> TypeCheckMonad a
typeCheckStmt typc stmt tc = 
	let
		errCtxt = hang (text "In the statement of a comprehension:") tabWidth
						(prettyPrint stmt)
		
		check (Qualifier e) = do
			addErrorContext errCtxt (ensureIsBool e)
			tc
		check (Generator p exp) = do
			fvs <- freeVars p
			texp <- addErrorContext errCtxt (typeCheck exp)
			local fvs $ do
				addErrorContext errCtxt (do
					tpat <- typeCheck p
					unify (typc tpat) texp)
				tc
	in setSrcSpan (loc stmt) (check (unAnnotate stmt))

-- | Type check a series of statements. For each statement a new scope is added
-- to ensure that clauses only depend on variables already bound.
typeCheckStmts :: (Type -> Type) -> [AnStmt] -> TypeCheckMonad a -> TypeCheckMonad a
typeCheckStmts typc stmts tc = do
		fvsByStmt <- mapM (\stmt -> do
				fvs <- freeVars stmt
				return (stmt, fvs)) stmts
		let 
			fvs = concatMap snd fvsByStmt
			namesToLocations = 
				[(n, loc f) | (f, fvs) <- fvsByStmt, n <- fvs]
		-- Throw an error if a name is defined multiple times
		manyErrorsIfFalse (noDups fvs) 
			(duplicatedDefinitionsMessage namesToLocations)
		check stmts
	where
		check [] = tc
		check (stmt:stmts) = typeCheckStmt typc stmt (check stmts)

-- | Shortcut for replicated operators
typeCheckReplicatedOp :: [AnStmt] -> TypeCheckMonad a -> TypeCheckMonad a
typeCheckReplicatedOp = typeCheckStmts TSet
