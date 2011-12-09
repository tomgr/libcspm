{-# LANGUAGE TypeSynonymInstances #-}
module CSPM.Evaluator.Expr (
    Evaluatable, eval,
) where

import Control.Monad.Trans
import Data.Maybe

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
import Util.Exception
import Util.PrettyPrint

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
                throwError $ patternMatchFailureMessage (loc p) p v
    eval (Let decls e) = do
        bs <- bindDecls decls
        addScopeAndBind bs (eval e)
    eval (Lit lit) = return $
        case lit of
            Int i -> VInt i
            Bool b -> VBool b   
    eval (List es) = mapM eval es >>= return . VList
    eval (ListComp es stmts) = do
            xs <- evalStmts (\(VList xs) -> xs) stmts (mapM eval es)
            return $ VList xs
        where
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
    eval (Var (UnQual n)) = do
        v <- lookupVar n
        case v of
            VProc p -> return $ VProc $ PProcCall (procId n []) (Just p)
            _       -> return v

    -- This is the most complicated process because it is actually a shorthand
    -- for external choice and internal choice.
    eval (Prefix e1 fs e2) = do
        VEvent n vs1 <- eval e1
        let
            normalizeEvent [] = []
            normalizeEvent ((VDot vs1):vs2) = normalizeEvent (vs1++vs2)
            normalizeEvent (v:vs) = v:normalizeEvent vs
            
            evalInputField :: [Value] -> [Field] -> AnPat -> S.ValueSet -> 
                                EvaluationMonad [Proc]
            evalInputField vs fs p s = do
                mps <- mapM (\v -> do
                    (matches, binds) <- bind p v
                    if matches then do
                        p <- addScopeAndBind binds 
                            (evalFields (vs++normalizeEvent [v]) fs)
                        return $ Just p
                    else return Nothing) (S.toList s)
                return $ catMaybes mps
            
            evalFields :: [Value] -> [Field] -> EvaluationMonad Proc
            evalFields vs [] = do
                p <- evalProc e2
                return $ PPrefix (valueEventToEvent (VEvent n vs)) p
            evalFields vs (Output e:fs) = do
                v <- eval e
                evalFields (vs++normalizeEvent [v]) fs
            
            evalFields vs (Input p (Just e):fs) = do
                VSet s <- eval e
                ps <- evalInputField vs fs p s
                return $ PExternalChoice ps
            evalFields vs (Input p Nothing:fs) = do
                -- Calculate which component this is
                let component = length vs
                chanVs <- valuesForChannel n
                let s = chanVs!!component
                ps <- evalInputField vs fs p s
                return $ PExternalChoice ps
            
            evalFields vs (NonDetInput p (Just e):fs) = do
                VSet s <- eval e
                ps <- evalInputField vs fs p s
                return $ PInternalChoice ps
            evalFields vs (NonDetInput p Nothing:fs) = do
                -- Calculate which component this is
                let component = length vs
                chanVs <- valuesForChannel n
                let s = chanVs!!component
                ps <- evalInputField vs fs p s
                return $ PInternalChoice ps
                
            -- Takes a proc and combines nested [] and |~|
            simplify :: Proc -> Proc
-- TODO: error over PInternalChoice []
            simplify (PExternalChoice [p]) = simplify p
            simplify (PInternalChoice [p]) = simplify p
            
            simplify (PExternalChoice ((PExternalChoice ps1):ps2)) =
                simplify (PExternalChoice (ps1++ps2))
            simplify (PExternalChoice ps) =
                PExternalChoice (map simplify ps)

            simplify (PInternalChoice ((PInternalChoice ps1):ps2)) =
                simplify (PInternalChoice (ps1++ps2))
            simplify (PInternalChoice ps) =
                PInternalChoice (map simplify ps)

            simplify p = p
            
        p <- evalFields vs1 (map unAnnotate fs)
        return $ VProc $ simplify p

    eval (AlphaParallel e1 e2 e3 e4) = do
        p1 <- evalProc e1
        p2 <- evalProc e4
        VSet a1 <- eval e2
        VSet a2 <- eval e3
        return $ VProc $ PAlphaParallel [(S.valueSetToEventSet a1, p1),
                                        (S.valueSetToEventSet a2, p2)]
    eval (Exception e1 e2 e3) = do
        p1 <- evalProc e1
        VSet a <- eval e2
        p2 <- evalProc e3
        return $ VProc $ PException p1 (S.valueSetToEventSet a) p2
    eval (ExternalChoice e1 e2) = do
        p1 <- evalProc e1
        p2 <- evalProc e2
        return $ VProc $ PExternalChoice [p1, p2]
    eval (GenParallel e1 e2 e3) = do
        ps <- evalProcs [e1, e3]
        VSet a <- eval e2
        return $ VProc $ PGenParallel (S.valueSetToEventSet a) ps
    eval (GuardedExp guard proc) = do
        VBool b <- eval guard
        if b then eval proc else lookupVar (Name "STOP")
    eval (Hiding e1 e2) = do
        p <- evalProc e1
        VSet s <- eval e2
        return $ VProc $ PHide p (S.valueSetToEventSet s)
    eval (InternalChoice e1 e2) = do
        ps <- evalProcs [e1, e2]
        return $ VProc $ PInternalChoice ps
    eval (Interrupt e1 e2) = do
        p1 <- evalProc e1
        p2 <- evalProc e2
        return $ VProc $ PInterrupt p1 p2
    eval (Interleave e1 e2) = do
        ps <- evalProcs [e1, e2]
        return $ VProc $ PInterleave ps
-- TODO
--  eval (LinkParallel e1 ties stmts e2)
--  eval (Rename e1 ties stmts) = 
    eval (SequentialComp e1 e2) = do
        p1 <- evalProc e1
        p2 <- evalProc e2
        return $ VProc $ PSequentialComp p1 p2
    eval (SlidingChoice e1 e2) = do
        p1 <- evalProc e1
        p2 <- evalProc e2
        return $ VProc $ PSlidingChoice p1 p2
    
    eval (ReplicatedAlphaParallel stmts e1 e2) = do
        aps <- evalStmts (\(VSet s) -> S.toList s) stmts (do
            VSet s <- eval e1
            p <- evalProc e2
            return [(S.valueSetToEventSet s, p)])
        return $ VProc $ PAlphaParallel aps
    eval (ReplicatedExternalChoice stmts e) = do
        ps <- evalStmts (\(VSet s) -> S.toList s) stmts (evalProcs [e])
        return $ VProc $ PExternalChoice ps
    eval (ReplicatedInterleave stmts e) = do
        ps <- evalStmts (\(VSet s) -> S.toList s) stmts (evalProcs [e])
        return $ VProc $ PInterleave ps
    eval (ReplicatedInternalChoice stmts e) = do
        ps <- evalStmts (\(VSet s) -> S.toList s) stmts (evalProcs [e])
        return $ VProc $ PInternalChoice ps
-- TODO
--  eval (ReplicatedLinkParallel e1 ties stmts e2) =
    eval (ReplicatedParallel e1 stmts e2) = do
        VSet s <- eval e1
        ps <- evalStmts (\(VSet s) -> S.toList s) stmts (evalProcs [e2])
        return $ VProc $ PGenParallel (S.valueSetToEventSet s) ps
    
    eval e = panic ("No clause to eval "++show e)

evalProcs = mapM evalProc

evalProc :: Evaluatable a => a -> EvaluationMonad Proc
evalProc a = eval a >>= \v -> case v of
    VProc x -> return x
    _       -> panic "Type checker error"

-- | Evaluates the statements, evaluating `prog` for each possible 
-- assingment to the generators that satisfies the qualifiers.
evalStmts :: (Value -> [Value]) -> [AnStmt] -> EvaluationMonad [a] -> 
            EvaluationMonad [a]
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
                (matches, binds) <- bind p v
                if matches then 
                    addScopeAndBind binds (evStmts stmts)
                else return []) (extract v)
            return $ concat vss
    in
        evStmts (map unAnnotate anStmts)

-- | Takes a VEvent and then computes all events that this is a prefix of.
completeEvent :: Value -> EvaluationMonad S.ValueSet
completeEvent (VEvent n vs) = do
    chanVs <- valuesForChannel n
    let remainingComponents = drop (length vs) chanVs
    if length remainingComponents == 0 then return $ S.fromList [VEvent n vs]
    else return $ S.cartesianProduct (\vs' -> VEvent n (vs++vs')) remainingComponents
