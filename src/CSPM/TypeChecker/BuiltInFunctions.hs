{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module CSPM.TypeChecker.BuiltInFunctions(
	injectBuiltInFunctions, externalFunctions, transparentFunctions,
	builtInNames
) where

import CSPM.DataStructures.Types
import CSPM.TypeChecker.Monad

import CSPM.DataStructures.Names
import Util.Exception
import Util.PartialFunctions

-- ************************************************************************
-- Built in function types
-- *************************************************************************
sets :: [Type -> (String, [Type], Type)]
sets = 
	let
		cspm_union fv = ("union", [TSet fv, TSet fv], TSet fv)
		cspm_inter fv = ("inter", [TSet fv, TSet fv], TSet fv)
		cspm_diff fv = ("diff", [TSet fv, TSet fv], TSet fv)
		cspm_Union fv = ("Union", [TSet (TSet fv)], TSet fv)
		cspm_Inter fv = ("Inter", [TSet (TSet fv)], TSet fv)
		cspm_member fv = ("member", [fv, TSet fv], TBool)
		cspm_card fv = ("card", [TSet fv], TInt)
		cspm_empty fv = ("empty", [TSet fv], TBool)
		cspm_set fv = ("set", [TSeq fv], TSet fv)
		cspm_Set fv = ("Set", [TSet fv], TSet (TSet fv))
		cspm_Seq fv = ("Seq", [TSet fv], TSet (TSeq fv))
		cspm_seq fv = ("seq", [TSet fv], TSeq fv)	
	in
		[cspm_union, cspm_inter, cspm_diff, cspm_Union, cspm_Inter,
				cspm_member, cspm_card, cspm_empty, cspm_set, cspm_Set,
				cspm_Seq, cspm_seq]
seqs :: [Type -> (String, [Type], Type)]
seqs = 
	let
		cspm_length fv = ("length", [TSeq fv], TInt)
		cspm_null fv = ("null", [TSeq fv], TBool)
		cspm_head fv = ("head", [TSeq fv], fv)
		cspm_tail fv = ("tail", [TSeq fv], TSeq fv)
		cspm_concat fv = ("concat", [TSeq (TSeq fv)], TSeq fv)
		cspm_elem fv = ("elem", [fv, TSeq fv], TBool)		
	in
		[cspm_length, cspm_null, cspm_head, cspm_tail, cspm_concat,
				cspm_elem]
				
builtInProcs :: [(String, Type)]
builtInProcs =
	let
		cspm_STOP = ("STOP", TProc)
		cspm_SKIP = ("SKIP", TProc)
		cspm_CHAOS = ("CHAOS", TFunction [TSet TEvent] TProc)
	in
		[cspm_STOP, cspm_SKIP, cspm_CHAOS]

typeConstructors :: [(String, Type)]
typeConstructors =
	let
		cspm_Int = ("Int", TSet TInt)
		cspm_Bool = ("Bool", TSet TBool)
		cspm_Proc = ("Proc", TSet TProc)
		cspm_Events = ("Events", TSet TEvent)
	in	
		[cspm_Int, cspm_Bool, cspm_Proc, cspm_Events]

injectBuiltInFunctions :: TypeCheckMonad ()
injectBuiltInFunctions =
	let
		mkFuncType cs func = 
			do
				fv @ (TVar (TypeVarRef tv _ _)) <- freshTypeVarWithConstraints cs
				let (n, args, ret) = func fv
				let t = ForAll [(tv, cs)] (TFunction args ret)
				setType (Name n) t
		mkPatternType func =
			do 
				let (n, t) = func
				setType (Name n) (ForAll [] t)
	in do
		mapM_ (mkFuncType []) seqs
		mapM_ (mkFuncType [Eq]) sets
		mapM_ mkPatternType typeConstructors
		mapM_ mkPatternType builtInProcs

externalFunctions :: PartialFunction String Type
externalFunctions = []

transparentFunctions :: PartialFunction String Type
transparentFunctions = [
	("diamond", TFunction [TProc] TProc),
	("normal", TFunction [TProc] TProc),
	("sbisim", TFunction [TProc] TProc),
	("tau_loop_factor", TFunction [TProc] TProc),
	("model_compress", TFunction [TProc] TProc),
	("explicate", TFunction [TProc] TProc),
	("chase", TFunction [TProc] TProc)
	]

builtInNames :: [String]
builtInNames = 
		map fst externalFunctions
		++ map fst transparentFunctions
		++ map fst typeConstructors
		++ map extract seqs
		++ map extract sets
	where
		extract f = let (a,_,_) = f (panic "Dummy type var evaluated") in a
