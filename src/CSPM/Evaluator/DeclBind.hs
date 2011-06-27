{-# LANGUAGE TypeSynonymInstances #-}
module CSPM.Evaluator.DeclBind (bindDecls) where

import Control.Monad

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import {-# SOURCE #-} CSPM.Evaluator.Expr
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import Util.Annotated

bindDecls :: [TCDecl] -> EvaluationMonad [(Name, Value)]
bindDecls ds = do
	bss <- mapM (bindDecl . unAnnotate) ds
	return (concat bss)

bindDecl :: Decl -> EvaluationMonad [(Name, Value)]
bindDecl (FunBind n ms) = do
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
				((binds, exp):rs) ->
					addScopeAndBind binds (eval exp)
				_		-> error ("Non exaustive patterns "++show n)
					--throwException $ NonExaustivePatterns n
			where
				ass = reverse ass_
		collectArgs n ass =
			return $ VFunction $ \ vs -> collectArgs (n-1) (vs:ass)
bindDecl (PatBind p e) = do
	v <- eval e
	r <- bind p v
	case r of 
		(True, bs) -> return bs
		(False, _) -> error "pattern match failure"
bindDecl (Channel ns e) = 
	-- TODO: check channel values are in es
	return [(n, VEvent n []) | n <- ns]
bindDecl (DataType n cs) =
	-- TODO: check data values are in e
	let
		bindClause (DataTypeClause nc Nothing) = do
			return (emptySet, [(nc, VDataType nc []), 
					(internalNameForDataTypeClause nc, VTuple [])])
		bindClause (DataTypeClause nc (Just e)) = do
			v <- eval e
			let sets = conv v
			let setOfValues = cartesianProduct (VDataType nc) sets
			let binds = [(nc, VDataType nc []),
				(internalNameForDataTypeClause nc, VTuple (map VSet sets))]
			return (setOfValues, binds)
			where
				conv (VDot vs) = map evalTypeExpr vs
				conv v = [evalTypeExpr v]
	in do
		(sets, binds) <- mapAndUnzipM (bindClause . unAnnotate) cs
		let dt = (n, VSet (unions sets))
		return $ dt:concat binds
--bindDecl (NameType n e) = do
--	v <- eval e
--	evalTypeExpr [v]
--	return [(n, 

bindDecl (Assert _) = return []
bindDecl (External ns) = return []
bindDecl (Transparent ns) = return []

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
evalTypeExpr (VList [VSet s]) = allSequences s
