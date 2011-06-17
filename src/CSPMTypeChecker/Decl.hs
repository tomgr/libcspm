{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module CSPMTypeChecker.Decl (typeCheckDecls) where

import Data.Graph
import List (nub, intersect)

import CSPMDataStructures.Syntax
import CSPMDataStructures.Types
import CSPMTypeChecker.BuiltInFunctions
import CSPMTypeChecker.Common
import CSPMTypeChecker.Dependencies
import CSPMTypeChecker.Expr
import CSPMTypeChecker.Monad
import CSPMTypeChecker.Pat
import CSPMTypeChecker.Unification
import Util.Annotated
import Util.List
import Util.Monad
import Util.PartialFunctions

-- Type check a list of possibly mutually recursive functions
typeCheckDecls :: [PDecl] -> TypeCheckMonad ()
typeCheckDecls decls =
	do
		let declMap = zip decls [0..]
		boundVarToDeclIdMap <- 
			concatMapM (\ decl -> 
				do
					namesBound <- namesBoundByDecl decl
					return [(n, apply declMap decl) | n <- namesBound]) decls
		let fvs = map fst boundVarToDeclIdMap
		
		errorIfFalse (noDups fvs) (DuplicatedDefinitions fvs)

		-- Pre-analysis phase:
		-- We prebind the datatypes and channels so that we can detect when something is
		-- a free variable in a pattern and when it is bound
		mapM_ prebindDecl (map unAnnotate decls)
-- TODO: prebind channels since people can pattern match on them, evily
		-- Map from decl id -> [decl id] meaning decl id depends on the list of
		-- ids
		declDeps <- mapM (\ decl -> 
				do
					fvsd <- dependencies decl
					let fvs' = intersect fvsd fvs
					return (apply declMap decl, mapPF boundVarToDeclIdMap fvs')
					) decls

		-- Edge from n -> n' iff n uses n'
		let sccs = map flattenSCC 
						(stronglyConnComp [(id, id, deps) | (id, deps) <- declDeps])
		let	typeInferenceGroups = map (mapPF (invert declMap)) sccs

		debugOutput ("Type check order: "
				++show (map (safeMapPF (invert boundVarToDeclIdMap)) sccs))
		
		mapM_ typeCheckMutualyRecursiveGroup typeInferenceGroups
	
typeCheckMutualyRecursiveGroup :: [PDecl] -> TypeCheckMonad ()
typeCheckMutualyRecursiveGroup ds =
	do
		-- We only create variables for non datatype declarations as the others
		-- were prebound in prebindDataType
		fvs <- liftM nub (concatMapM namesBoundByDecl 
					(filter (not . isDataTypeDecl) ds))
		ftvs <- replicateM (length fvs) freshTypeVar
		zipWithM setType fvs (map (ForAll []) ftvs)
		
		-- The list of all variables bound by these declaration
		fvs <- liftM nub (concatMapM namesBoundByDecl ds)

		-- Type check each declaration then generalise the types
		nts <- generaliseGroup fvs (map typeCheck ds)
		-- Add the type of each declaration (if one exists to each declaration)
--		zipWithM annotate nts ds
		
		-- Compress all the types we have inferred here
		-- (They should never be touched again)
		mapM_ (\ n -> 
			do
				t <- getType n
				t' <- compressTypeScheme t
				let Name n' = n
				liftIO $ putStrLn (n'++" :: "++show (prettyPrintTypeScheme t'))
				setType n t') fvs
	where
		isDataTypeDecl (An _ _ inner) = isDataTypeDecl' inner
		isDataTypeDecl' (DataType _ _ ) = True
		isDataTypeDecl' _ = False
		
		annotate nts (An _ psymbtable _) =
			setPSymbolTable psymbtable nts

evaluateTypeExpression :: Type -> TypeCheckMonad Type
evaluateTypeExpression (TTuple ts) =
	-- Could be a tuple of sets (TTuple [TSet t1,...] = TSet (TTuple [t1,...]))
	-- according to Bill's book P529
	do
		ts' <- mapM evaluateTypeExpression ts
		return $ TTuple ts'
evaluateTypeExpression (TSeq t) =
	do
		t' <- evaluateTypeExpression t
		return $ TSeq t'
evaluateTypeExpression t =		
	do
		fv <- freshTypeVar
		unify t (TSet fv)
		return fv

instance TypeCheckable PDecl [(Name, Type)] where
	errorConstructor = ErrorWithDecl
	-- We perform type annotation in  typeCheckMutualyRecursiveGroup since
	-- this is the first time we know the TypeScheme.
	typeCheck' = typeCheck' . unAnnotate

instance TypeCheckable Decl [(Name, Type)] where
	errorConstructor = error "Decl () error constructor called"
	typeCheck' (FunBind n ms) = 
		do
			ts <- mapM typeCheck ms
			ForAll [] t <- getType n
			(t' @ (TFunction tsargs _)) <- unifyAll (t:ts)
			return [(n, t')]
	typeCheck' (PatBind pat exp) =
		do
			tpat <- typeCheck pat
			tpat <- evaluateDots tpat
			fvs <- freeVars pat
			-- Allow the pattern to be recursive
			texp <- local fvs (typeCheck exp)
			texp <- evaluateDots texp
			unify texp tpat
			ns <- namesBoundByDecl' (PatBind pat exp)
			ts <- mapM getType ns
			return $ zip ns [t | ForAll _ t <- ts]
	typeCheck' (Channel ns es) =
		do
			ts <- mapM typeCheck es
--			fvs <- replicateM (length ts) freshTypeVar
			-- Make sure everything is a set
			ts' <- mapM evaluateTypeExpression ts
			-- Channels require that each component is comparable
			mapM (ensureHasConstraint Eq) ts'
			let t = foldr TDotable TEvent ts'
			mapM_ (\ n -> setType n (ForAll [] t)) ns
			return [(n,t) | n <- ns]
	-- This clause is relies on the fact that prebindDataType has been called first
	-- any changes to that will require a change here
	typeCheck' (DataType n clauses) =
		do
			nts <- mapM (\ clause -> 
				do
					(n', ts) <- typeCheck clause
					-- We already have a variable for n' (introduced in prebindDataType)
					ForAll [] t <- getType n'
					unify t (foldr (TDotable) (TDatatype n) ts)
					return (n', t)
					) clauses
			-- We have already set the type of n in prebindDataType
			ForAll [] t <- getType n
			return $ (n,t):nts
	typeCheck' (Transparent ns) =
		do
			let ts = map (\ (Name n) -> 
							(Name n, ForAll [] (apply transparentFunctions n))) ns
			mapM_ (\ (n, t) -> setType n t) ts
			return []
	typeCheck' (External ns) =
		do
			let ts = map (\ (Name n) -> 
							(Name n, ForAll [] (apply externalFunctions n))) ns
			mapM_ (\ (n, t) -> setType n t) ts
			return []

 	typeCheck' (Assert a) = typeCheck a >> return []

instance TypeCheckable Assertion () where
	errorConstructor = error "really?"
	typeCheck' (PropertyCheck e1 p m) =
		do
			t1 <- typeCheck e1
			ensureIsProc t1
			return ()
 	typeCheck' (Refinement e1 m e2 p) =
		do
			t1 <- typeCheck e1 	
			t2 <- typeCheck e2
			ensureIsProc t1
			ensureIsProc t2
			case p of
 				Just (TauPriority e3) -> do
										t1 <- typeCheck e3
										unify t1 (TSet TEvent)
										return ()
				Nothing				-> return ()
 	
instance TypeCheckable PDataTypeClause (Name, [Type]) where
	errorConstructor = ErrorWithDataTypeClause
	typeCheck' (An srcloc typ inner) = 
		do
			t' <- typeCheck' inner
			-- TODO: type annotation
--			setPType typ t'
			return t'
-- TODO: allow Proc in a datatype declaration
-- TODO: check no type variables are left in a declaration of a datatype or
-- channel
instance TypeCheckable DataTypeClause (Name, [Type]) where
	errorConstructor = error "Error: DataTypeClause error constructor called"
	typeCheck' (DataTypeClause n' es) =
		do
			ts <- mapM typeCheck es
			--fvs <- replicateM (length ts) freshTypeVar 
			-- Make sure everything is a set
			ts' <- mapM evaluateTypeExpression ts
			return (n', ts')

instance TypeCheckable PMatch Type where
	errorConstructor = ErrorWithMatch
	typeCheck' (An srcloc typ inner) = 
		do
			t' <- typeCheck' inner
			return t'
instance TypeCheckable Match Type where
	errorConstructor = error "Error: match error constructor called"
	typeCheck' (Match groups exp) = 
		do
			fvs <- liftM concat (mapM freeVars groups)
			local fvs (
				do
					tgroups <- mapM (\ pats -> mapM (\ pat ->
						do 
							t <- typeCheck pat
--							liftIO $ putStrLn "Evaling"
--							liftIO $ putStrLn (show t)
							t <- evaluateDots t
--							liftIO $ putStrLn "Evaled"
							return t
						) pats) groups

					tr <- typeCheck exp
					-- TODO: surely this can't be the only place to do it
					tr <- evaluateDots tr
					-- TODO: we need to evaluate the dots in the patterns
					-- twice just in case the type inferences on the RHS
					-- have resulted in extra dots on the left being able to
					-- be removed
					tgroups <- mapM (\ pats -> mapM (\ pat ->
						do 
							t <- typeCheck pat
--							liftIO $ putStrLn "Evaling"
--							liftIO $ putStrLn (show t)
							t <- evaluateDots t
--							liftIO $ putStrLn "Evaled"
							return t
						) pats) groups

					return $ foldr (\ targs tr -> TFunction targs tr) tr tgroups)
