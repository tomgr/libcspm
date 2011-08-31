{-# LANGUAGE TypeSynonymInstances #-}
module CSPM.Evaluator.DeclBind (
	bindDecls, valuesForChannel, valuesForDataTypeClause,
) where

import Control.Monad
import Data.Maybe

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Exceptions
import {-# SOURCE #-} CSPM.Evaluator.Expr
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import Util.Annotated

bindDecls :: [TCDecl] -> EvaluationMonad [(Name, Value)]
bindDecls ds = do
	bss <- mapM bindDecl ds
	return (concat bss)

bindDecl :: AnDecl -> EvaluationMonad [(Name, Value)]
bindDecl (an@(An _ _ (FunBind n ms))) = do
		func <- collectArgs argGroupCount []
		return [(n, func)]
	where
		mss = map unAnnotate ms
		argGroupCount = head (map (\ (Match pss e) -> length pss) mss)
		collectArgs :: Int -> [[Value]] -> EvaluationMonad Value
		collectArgs 0 ass_ = do
			bss <- mapM (\ (Match pss e) -> do
					r <- zipWithM bindAll pss ass
					let b = and (map fst r)
					let binds = concatMap snd r
					return ((b, binds), e)
				) mss
			let
				rs :: [([(Name, Value)], TCExp)]
				rs = [(bs, e) | ((True, bs), e) <- bss]
			case rs of
				((binds, exp):_) ->
					addScopeAndBind binds (do
						v <- eval exp
						case v of
							VProc p -> 
								return $ VProc $ PProcCall (procId n ass) (Just p)
							_ -> return v)
				_		-> throwError $ 
					funBindPatternMatchFailureMessage (loc an) n ass
			where
				ass = reverse ass_
		collectArgs n ass =
			return $ VFunction $ \ vs -> collectArgs (n-1) (vs:ass)
bindDecl (an@(An _ _ (PatBind p e))) = do
	v <- eval e
	r <- bind p v
	case r of 
		(True, bs) -> return bs
		(False, _) -> throwError $ patternMatchFailureMessage (loc an) p v
bindDecl (an@(An _ _ (Channel ns me))) = do
	-- TODO: check channel values are in es
	vs <- case me of 
		Nothing -> return []
		Just e -> do
			v <- eval e
			return $ evalTypeExprToList v
	return $ [(n, VEvent n []) | n <- ns]++
			[(internalNameForChannel n, VTuple (map VSet vs)) | n <- ns]
bindDecl (an@(An _ _ (DataType n cs))) =
	-- TODO: check data values are in e
	let
		bindClause (DataTypeClause nc Nothing) = do
			return (emptySet, [(nc, VDataType nc []), 
					(internalNameForDataTypeClause nc, VTuple [])])
		bindClause (DataTypeClause nc (Just e)) = do
			v <- eval e
			let sets = evalTypeExprToList v
			let setOfValues = cartesianProduct (VDataType nc) sets
			let binds = [(nc, VDataType nc []),
				(internalNameForDataTypeClause nc, VTuple (map VSet sets))]
			return (setOfValues, binds)
	in do
		(sets, binds) <- mapAndUnzipM (bindClause . unAnnotate) cs
		let dt = (n, VSet (unions sets))
		return $ dt:concat binds
bindDecl (an@(An _ _ (NameType n e))) = do
	v <- eval e
	return [(n, VSet $ evalTypeExpr v)]

bindDecl (an@(An _ _ (Assert _))) = return []
bindDecl (an@(An _ _ (External ns))) = return []
bindDecl (an@(An _ _ (Transparent ns))) = return []

internalNameForChannel, internalNameForDataTypeClause :: Name -> Name
internalNameForChannel (Name n) = 
	mkInternalName ("VALUE_TUPLE_CHANNEL_"++n)
internalNameForDataTypeClause (Name n) = 
	mkInternalName ("VALUE_TUPLE_DT_CLAUSE_"++n)

valuesForChannel :: Name -> EvaluationMonad [ValueSet]
valuesForChannel n = do
	VTuple vs <- lookupVar (internalNameForChannel n)
	return $ map (\(VSet s) -> s) vs

valuesForDataTypeClause :: Name -> EvaluationMonad [ValueSet]
valuesForDataTypeClause n = do
	VTuple vs <- lookupVar (internalNameForDataTypeClause n)
	return $ map (\(VSet s) -> s) vs

evalTypeExpr :: Value -> ValueSet
evalTypeExpr (VSet s) = s
evalTypeExpr (VDot vs) = cartesianProduct VDot (map evalTypeExpr vs)
evalTypeExpr (VTuple vs) = cartesianProduct VTuple (map evalTypeExpr vs)

evalTypeExprToList :: Value -> [ValueSet]
evalTypeExprToList (VDot vs) = map evalTypeExpr vs
evalTypeExprToList v = [evalTypeExpr v]
