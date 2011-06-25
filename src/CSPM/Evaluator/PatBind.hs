{-# LANGUAGE TypeSynonymInstances #-}
module CSPM.Evaluator.PatBind where

import Control.Monad

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import Util.Annotated
import Util.Exception

-- Bind :: Pattern, value -> (Matches Pattern, Values to Bind
class Bindable a where
	bind :: a -> Value -> EvaluationMonad (Bool, [(Name, Value)])

instance Bindable a => Bindable (Annotated b a) where
	bind (An _ _ a) v = bind a v

instance Bindable Pat where
	-- We can decompose any PConcat pattern into three patterns representing:
	-- Begining (of the form PList), middle (either PWildcard or PVar)
	-- and end (of the form PList), With both begining and end possible empty
	bind (PConcat p1 p2) (VList xs) = panic "PConcat not implemented"
	bind (PDotApp p1 p2) v = panic "PDotApp not implemented"
		-- above v should only be: VEvent, VDot VDataType
	bind (PDoublePattern p1 p2) v = do
		(m1, b1) <- bind p1 v
		(m2, b2) <- bind p2 v
		return $ (m1 && m2, b1++b2)
	bind (PList ps) (VList xs) | length ps == length xs = bindAll ps xs
	bind (PLit (Int i1)) (VInt i2) | i1 == i2 = return (True, [])
	bind (PLit (Bool b1)) (VBool b2) | b1 == b2 = return (True, [])
	bind (PParen p) v = bind p v
	bind (PSet [p]) (VSet s) = panic "PSet not implemented"
	bind (PTuple ps) (VTuple vs) = do
		-- NB: len ps == len vs by typechecker
		bindAll ps vs
	-- TODO: remember n could refer to a datatype or channel
	-- IDEA: have a desugar phase that sorts out PVar and PConcat patterns,
	-- and possibly some other things
	bind (PVar n) v = return (True, [(n, v)])
	bind PWildCard v = return (True, [])
	bind _ _ = return (False, [])

bindAll :: Bindable a => [a] -> [Value]
			-> EvaluationMonad (Bool, [(Name, Value)])
bindAll ps xs = do
	rs <- zipWithM bind ps xs
	return (and (map fst rs), concat (map snd rs))
