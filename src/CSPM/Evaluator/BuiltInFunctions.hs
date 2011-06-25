module CSPM.Evaluator.BuiltInFunctions where

import Control.Monad

import CSPM.DataStructures.Names
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet as S
import Util.Exception

builtInFunctions :: [(Name, Value)]
builtInFunctions = 
	let
		cspm_union [VSet s1, VSet s2] = S.union s1 s2
		cspm_inter [VSet s1, VSet s2] = S.intersection s1 s2
		cspm_diff [VSet s1, VSet s2] = S.difference s1 s2
		cspm_Union ss = S.unions (map (\ (VSet s) -> s) ss)
		cspm_Inter ss = S.intersections (map (\ (VSet s) -> s) ss)
		cspm_member [v, VSet s] = VBool $ S.member v s
		cspm_card [VSet s] = VInt $ S.card s
		cspm_empty [VSet s] = VBool $ S.empty s
		cspm_set [VList xs] = S.fromList xs
		-- | Powerset
		cspm_Set [VSet s] = 
			(S.fromList . map (VSet . S.fromList)
				. filterM (\x -> [True, False]) . S.toList) s
		--return $ VSet (powerset (card s))
		{-	where
				-- sets up to size n
				powerset 0 = S.fromList [S.empty]
				powerset n =
					S.map (\ e -> ) s
					where
						subsets = powerset (n-1)
		-}
		-- | Set of all sequences over s
		cspm_Seq [VSet s] = panic "Not implemented"
		{-
			itemsAsList <- S.toList s
			let 
				list n = seqsOfLength n ++ list (n+1)
				seqsOfLength 0 = []
				seqsOfLength n =
					concatMap (\ h -> map (h:) ends) itemsAsList
					where
						ends = seqsOfLength n-1
			S.lazySet (list 0)		
		-}
		cspm_seq [VSet s] = panic "Not implemented" --S.toList s
		
		cspm_length [VList xs] = VInt $ (toInteger (length xs))
		cspm_null [VList xs] = VBool $ null xs
		cspm_head [VList xs] = head xs
		cspm_tail [VList xs] = tail xs
		cspm_concat [VList xs] = concat (map (\(VList ys) -> ys) xs)
		cspm_elem [v, VList vs] = VBool $ v `elem` vs
		
		set_funcs = [
			("union", cspm_union), ("inter", cspm_inter), 
			("diff", cspm_diff), ("Union", cspm_Union), 
			("Inter", cspm_Inter), ("set", cspm_set), 
			("Set", cspm_Set), ("Seq", cspm_Seq)
			]
		
		seq_funcs = [
			("seq", cspm_seq), ("tail", cspm_tail), ("concat", cspm_concat)
			]
		
		other_funcs = [
			("length", cspm_length), ("null", cspm_null), 
			("head", cspm_head), ("elem", cspm_elem),
			("member", cspm_member), ("card", cspm_card),
			("empty", cspm_empty)
			]
		
		mkFunc (s, f) = (Name s, VFunction (\ vs -> return (f vs)))
	in
		map mkFunc (
			map (\ (n, f) -> (n, VSet . f)) set_funcs
			++ map (\ (n, f) -> (n, VList . f)) seq_funcs
			++ other_funcs)

injectBuiltInFunctions :: EvaluationMonad a -> EvaluationMonad a
injectBuiltInFunctions = addScopeAndBind builtInFunctions
