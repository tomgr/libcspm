{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, UndecidableInstances #-}
module CSPM.Evaluator.Expr (
    Evaluatable, eval, evalStmts, evalStmts', extendEvent, completeEvent,
) where

import Control.Monad.Trans
import qualified Data.Foldable as F
import Data.Maybe
import Data.Sequence ((<|), (|>))
import qualified Data.Sequence as Sq

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Environment
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import Util.Annotated
import Util.Exception
import Util.Monad
import Util.Prelude
import Util.PrettyPrint

-- In order to keep lazy evaluation working properly only use pattern
-- matching when you HAVE to know the value. (Hence why we delay pattern
-- matching in BooleanBinaryOp And in case the first value is false.)

class Evaluatable a ops where
    eval :: a -> EvaluationMonad ops (Value ops)

instance Evaluatable a ops => Evaluatable (Annotated b a) ops where
    eval (An _ _ a) = eval a

instance (Evaluatable (p Name) ops, PrettyPrintable (UProc ops)) => 
        Evaluatable (Exp Name p) ops where
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
                    -- This is lazy, only pattern matches if b2 is required.
                    VBool b2 = v2
                in return $ VBool (b1 && b2)
            Or -> 
                let 
                    VBool b1 = v1
                    -- This is lazy, only pattern matches if b2 is required.
                    VBool b2 = v2
                in return $ VBool (b1 || b2)
            Equals -> return $ VBool (compareValues v1 v2 == Just EQ)
            NotEquals -> return $ VBool (compareValues v1 v2 /= Just EQ)
            LessThan -> return $ VBool (compareValues v1 v2 == Just LT)
            GreaterThan -> return $ VBool (compareValues v1 v2 == Just GT)
            LessThanEq -> return $ VBool (compareValues v1 v2 `elem` [Just LT, Just EQ])
            GreaterThanEq -> return $ VBool (compareValues v1 v2 `elem` [Just GT, Just EQ])
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
            combineDots v1 v2
    eval (If e1 e2 e3) = do
        VBool b <- eval e1
        if b then eval e2 else eval e3
    eval (Lambda p e) = do
        st <- getState
        return $ VFunction $ \ [v] -> return $ runEvaluator st $ do
            let (matches, binds) = bind p v
            if matches then do
                p <- getParentProcName
                updateParentProcName (annonymousProcId [[v]] p) $ do
                    addScopeAndBind binds (eval e)
            else
                throwError $ patternMatchFailureMessage (loc p) p v
    eval (Let decls e) = do
        nvs <- bindDecls decls
        addScopeAndBindM nvs (eval e)
    eval (Lit lit) = return $
        case lit of
            Int i -> VInt i
            Bool b -> VBool b   
    eval (List es) = mapM eval es >>= return . VList
    eval (ListComp es stmts) = do
            xs <- evalStmts (\(VList xs) -> xs) stmts (mapM eval es)
            return $ VList xs
    eval (ListEnumFrom e) = do
        VInt lb <- eval e
        return $ VList (map VInt [lb..])
    eval (ListEnumFromTo e1 e2) = do
        VInt lb <- eval e1
        VInt ub <- eval e2
        return $ VList (map VInt [lb..ub])
    eval (ListLength e) = do
        VList xs <- eval e 
        return $ VInt (length xs)
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
    eval (Process p) = eval p
    eval (Set es) = mapM eval es >>= return . VSet . S.fromList
    eval (SetComp es stmts) = do
        xs <- evalStmts (\(VSet s) -> S.toList s) stmts (mapM eval es)
        return $ VSet (S.fromList xs)
    eval (SetEnum es) = do
        evs <- mapM eval es
        ss <- mapM completeEvent evs
        return $ VSet (S.unions ss)
    eval (SetEnumComp es stmts) = do
        ss <- evalStmts (\(VSet s) -> S.toList s) stmts 
                        (mapM (\e -> eval e >>= completeEvent) es)
        return $ VSet (S.unions ss)
    eval (SetEnumFrom e) = do
        VInt lb <- eval e
        return $ VSet (S.IntSetFrom lb)
    eval (SetEnumFromTo e1 e2) = do
        VInt lb <- eval e1
        VInt ub <- eval e2
        return $ VSet (S.fromList (map VInt [lb..ub]))
    eval (Tuple es) = mapM eval es >>= return . VTuple
    eval (Var n) | isNameDataConstructor n = do
        VTuple [dc, _, _] <- lookupVar n
        return dc
    eval (Var n) = lookupVar n

-- | Evaluates the statements, evaluating `prog` for each possible 
-- assingment to the generators that satisfies the qualifiers.
evalStmts :: (Evaluatable (p Name) ops, PrettyPrintable (UProc ops)) => 
    (Value ops -> [Value ops]) -> [TCStmt p] -> EvaluationMonad ops [a] -> 
    EvaluationMonad ops [a]
evalStmts extract anStmts prog =
    let
        -- | Progressively generates new values lazily
        evStmts [] = prog
        evStmts (Qualifier e:stmts) = do
            VBool b <- eval e
            if b then evStmts stmts else return []
        evStmts (Generator p e:stmts) = do
            v <- eval e
            vss <- mapM (\v -> do
                let (matches, binds) = bind p v
                if matches then 
                    addScopeAndBind binds (evStmts stmts)
                else return []) (extract v)
            return $ concat vss
    in
        evStmts (map unAnnotate anStmts)

-- | Evaluates the statements, evaluating `prog` for each possible 
-- assingment to the generators that satisfies the qualifiers.
evalStmts' :: (Evaluatable (p Name) ops, PrettyPrintable (UProc ops)) => 
    (Value ops -> Sq.Seq (Value ops)) -> [TCStmt p] -> EvaluationMonad ops a -> 
    EvaluationMonad ops (Sq.Seq a)
evalStmts' extract anStmts prog =
    let
        evStmts [] = prog >>= return . Sq.singleton
        evStmts (Qualifier e:stmts) = do
            VBool b <- eval e
            if b then evStmts stmts else return Sq.empty
        evStmts (Generator p e:stmts) = do
            v <- eval e
            F.foldlM (\ s v -> do
                let (matches, binds) = bind p v
                if matches then do
                    s' <- addScopeAndBind binds (evStmts stmts)
                    return $ s Sq.>< s'
                else return s) Sq.empty (extract v)
    in evStmts (map unAnnotate anStmts)

-- | Takes a VEvent and then computes all events that this is a prefix of.
completeEvent :: Value ops -> EvaluationMonad ops (S.ValueSet ops)
completeEvent ev = do
    exs <- extensions ev
    l <- mapM (extendEvent ev) exs
    return $ S.fromList l

extendEvent :: Value ops -> Value ops -> EvaluationMonad ops (Value ops)
extendEvent ev exs = combineDots ev exs
