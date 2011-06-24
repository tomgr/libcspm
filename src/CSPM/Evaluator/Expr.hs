{-# LANGUAGE TypeSynonymInstances #-}
module CSPM.Evaluator.Expr (
	Evaluatable, eval,
) where

import Control.Monad.Trans

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Environment
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import Util.Annotated

-- In order to keep lazy evaluation working properly only use pattern
-- matching when you HAVE to know the value. (Hence why we delay pattern
-- matching in BooleanBinaryOp And in case the first value is false.)

class Evaluatable a where
	eval :: a -> EvaluationMonad Value

instance Evaluatable a => Evaluatable (Annotated b a) where
	eval (An _ _ a) = eval a

instance Evaluatable Exp where
	eval (App func args) = do
		vs <- mapM eval args
		VFunction f <- eval func
		f vs
	eval (BooleanBinaryOp op e1 e2) = do
		v1 <- eval e1
		v2 <- eval e2
		case op of
			And -> 
				let 
					VBool b1 = v1
					VBool b2 = v2
				in return $ VBool (b1 && b2)
			Or -> 
				let 
					VBool b1 = v1
					VBool b2 = v2
				in return $ VBool (b1 || b2)
			Equals -> return $ VBool (v1 == v2)
			NotEquals -> return $ VBool (v1 /= v2)
			LessThan -> return $ VBool (v1 < v2)
			GreaterThan -> return $ VBool (v1 > v2)
			LessThanEq -> return $ VBool (v1 <= v2)
			GreaterThanEq -> return $ VBool (v1 >= v2)
	eval (BooleanUnaryOp op e) = do
		VBool b <- eval e
		case op of
			Not -> return $ VBool (not b)
	eval (Concat e1 e2) = do
		VList vs1 <- eval e1
		v2 <- eval e2
		-- If we instead wrote VList v2 <- eval e2
		-- then this would force evaluation of e2 to a list immediately.
		-- However, if we do the following instead this means that
		-- the second argument is only evaluated if it absolutely has to 
		-- be (what if e2 was bottom and we were taking head(e1^e2)).
		-- (To see why haskell does this consider the desugared form with
		-- the do's removed. It would be:
		-- ... eval e1) >>= (\ (VList vs2) -> ...)
		-- and therefore the pattern match would force evaluation.)
		let VList vs2 = v2
		return $ VList (vs1++vs2)
	eval (DotApp e1 e2) = do
			v1 <- eval e1
			v2 <- eval e2
			return $ combineDots v1 v2
		where
			combineDots (VDot vs1) (VDot vs2) = VDot (vs1++vs2)
			combineDots (VDot vs) y = VDot (vs++[y])
			combineDots (VEvent n vs1) (VDot vs2) = VEvent n (vs1++vs2)
			combineDots (VEvent n vs1) x = VEvent n (vs1++[x])
			combineDots (VDataType n vs1) (VDot vs2) = VDataType n (vs1++vs2)
			combineDots (VDataType n vs1) x = VDataType n (vs1++[x])
			combineDots v1 v2 = VDot [v1, v2]
	eval (If e1 e2 e3) = do
		VBool b <- eval e1
		if b then eval e2 else eval e3
	eval (Lambda p e) =
		return $ VFunction $ \ [v] -> do
			(matches, binds) <- bind p v
			if matches then
				addScopeAndBind binds (eval e)
			else
				throwException $ PatternMatchException p v
	eval (Let decls e) = do
		bs <- bindDecls decls
		addScopeAndBind bs (eval e)
	eval (Lit lit) = return $
		case lit of
			Int i -> VInt i
			Bool b -> VBool b	
	eval (List es) = mapM eval es >>= return . VList
	eval (ListComp es stmts) = do
			xs <- evStmts (map unAnnotate stmts)
			return $ VList xs
		where
			-- TODO: we need to de-sugar this to move the generators
			-- before the qualifiers. Then, we can produce a list of values
			-- that can be checked by the qualifiers. Should be easy to
			-- make this lazy.
			-- We need to make sure that the qualifiers are kept in the correct
			-- order otherwise we could get errors.
			-- Note that the above relies on the fact that evaluation of
			-- the components is lazy. Consider the following:
			-- let f(0) = 1 within head(<x | x <- <0..>, y <- <0..>, x+y == 0>)
			-- let gen(x) = <x>^gen(x) within head(gen(0))
			-- | Progressively generates new values lazily
			evStmts :: [Stmt] -> EvaluationMonad [Value]
			evStmts [] = mapM eval es
			evStmts (Qualifier e:stmts) = do
				VBool b <- eval e
				if b then evStmts stmts else return []
			evStmts (Generator p e:stmts) = do
				VList vs <- eval e
				vss <- mapM (\v -> do
					(matches, binds) <- bind p v
					if matches then 
						addScopeAndBind binds (evStmts stmts)
					else return []) vs
				return $ concat vss
	eval (ListEnumFrom e) = do
		VInt lb <- eval e
		return $ VList (map VInt [lb..])
	eval (ListEnumFromTo e1 e2) = do
		VInt lb <- eval e1
		VInt ub <- eval e2
		return $ VList (map VInt [lb..ub])
	eval (ListLength e) = do
		VList xs <- eval e 
		return $ VInt (toInteger (length xs))
	eval (MathsBinaryOp op e1 e2) = do
		VInt i1 <- eval e1
		VInt i2 <- eval e2
		case op of
			Divide -> return $ VInt (i1 `div` i2)
			Minus -> return $ VInt (i1 - i2)
			Mod -> return $ VInt (i1 `mod` i2)
			Plus -> return $ VInt (i1 + i2)
			Times -> return $ VInt (i1 * i2)
	eval (MathsUnaryOp op e) = do
		VInt i <- eval e
		case op of 
			Negate -> return $ VInt (-i)
	eval (Paren e) = eval e
	eval (Set es) = mapM eval es >>= return . VSet . S.fromList
--	eval (SetComp es stmts) = error "TODO: setComp"
--	eval (SetEnum es) = error "TODO: SetEnum"
--	eval (SetEnumComp es stmts) = error "TODO: SetEnumComp"
	eval (SetEnumFrom e) = do
		VInt lb <- eval e
		return $ VSet (S.IntSetFrom lb)
	eval (SetEnumFromTo e1 e2) = do
		VInt lb <- eval e1
		VInt ub <- eval e2
		return $ VSet (S.RangedSet lb ub)
	eval (Tuple es) = mapM eval es >>= return . VTuple
	eval (Var (UnQual n)) = lookupVar n

	eval e = panic ("No clause to eval "++show e)
