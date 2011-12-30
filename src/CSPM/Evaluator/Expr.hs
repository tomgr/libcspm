{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CSPM.Evaluator.Expr (
    Evaluatable, eval,
) where

import Control.Monad.Trans
import Data.Maybe

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

class Evaluatable a where
    eval :: a -> EvaluationMonad Value

instance Evaluatable a => Evaluatable (Annotated b a) where
    eval (An _ _ a) = eval a

instance Evaluatable (Exp Name) where
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
    eval (Lambda p e) =
        return $ VFunction $ \ [v] -> do
            let (matches, binds) = bind p v
            if matches then
                addScopeAndBind binds (eval e)
            else
                throwError $ patternMatchFailureMessage (loc p) p v
    eval (Let decls e) = do
        addScopeAndBindM (bindDecls decls) (eval e)
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
    eval (Var n) | isNameDataConstructor n = do
        VTuple [dc, _, _] <- lookupVar n
        return dc
    eval (Var n) = do
        v <- lookupVar n
        case v of
            VProc p -> return $ VProc $ PProcCall (procId n []) (Just p)
            _       -> return v

    -- This is the most complicated process because it is actually a shorthand
    -- for external choice and internal choice.
    eval (Prefix e1 fs e2) = do
        ev <- eval e1
        let
            normalizeEvent [] = []
            normalizeEvent ((VDot vs1):vs2) = normalizeEvent (vs1++vs2)
            normalizeEvent (v:vs) = v:normalizeEvent vs
            
            evalInputField :: [Value] -> [Field Name] -> TCPat -> S.ValueSet -> 
                                EvaluationMonad [Proc]
            evalInputField vs fs p s = do
                mps <- mapM (\v -> do
                    let (matches, binds) = bind p v
                    if matches then do
                        p <- addScopeAndBind binds 
                            (evalFields (vs++normalizeEvent [v]) fs)
                        return $ Just p
                    else return Nothing) (S.toList s)
                return $ catMaybes mps
            
            evalFields :: [Value] -> [Field Name] -> EvaluationMonad Proc
            evalFields vs [] = do
                p <- evalProc e2
                return $ PPrefix (valueEventToEvent ev) p
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
                panic "unsupported case"
                --chanVs <- valuesForChannel n
                --let s = chanVs!!component
                --ps <- evalInputField vs fs p s
                --return $ PExternalChoice ps
            
            evalFields vs (NonDetInput p (Just e):fs) = do
                VSet s <- eval e
                ps <- evalInputField vs fs p s
                return $ PInternalChoice ps
            evalFields vs (NonDetInput p Nothing:fs) = do
                -- Calculate which component this is
                let component = length vs
                panic "unsupported case"
                ----chanVs <- valuesForChannel n
                ----let s = chanVs!!component
                ----ps <- evalInputField vs fs p s
                --return $ PInternalChoice ps
-- TODO: the semantics of $ are not currently correct                
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
            
    -- TODO: is this right?
            vs1 = case ev of
                    VDot vs -> vs
                    _ -> [ev]
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
        if b then eval proc else lookupVar (builtInName "STOP")
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
    eval (LinkParallel e1 ties stmts e2) = do
        p1 <- evalProc e1
        p2 <- evalProc e2
        ts <- evalTies stmts ties
        return $ VProc $ PLinkParallel ts [p1, p2]
    eval (Rename e1 ties stmts) = do
        p1 <- evalProc e1
        ts <- evalTies stmts ties
        return $ VProc $ PRename ts p1
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
        if length ps == 0 then -- TODO: speed up
            let e' = ReplicatedInternalChoice stmts e in
            throwError $ replicatedInternalChoiceOverEmptySetMessage (loc e) e'
        else return $ VProc $ PInternalChoice ps
    eval (ReplicatedLinkParallel ties tiesStmts stmts e) = do
        ts <- evalTies tiesStmts ties
        ps <- evalStmts (\(VList vs) -> vs) stmts (evalProcs [e])
        return $ VProc $ PLinkParallel ts ps
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

evalTies :: [TCStmt] -> [(TCExp, TCExp)] -> EvaluationMonad [(Event, Event)]
evalTies stmts ties = do
    tss <- evalStmts (\(VSet s) -> S.toList s) stmts (mapM evalTie ties)
    return $ concat tss
    where
        extendTie :: (Value, Value) -> Value -> EvaluationMonad (Event, Event)
        extendTie (evOld, evNew) ex = do
            ev1 <- extendEvent evOld ex
            ev2 <- extendEvent evNew ex
            return (valueEventToEvent ev1, valueEventToEvent ev2)
        evalTie (eOld, eNew) = do
            evOld <- eval eOld
            evNew <- eval eNew
            -- Obviously evOld and evNew could be channels, or prefixes
            -- of events so we compute the extensions.
            -- TODO: this assumes extensions evOld <= extensions evNew
            exsOld <- extensions evOld
            mapM (\ex -> extendTie (evOld, evNew) ex) exsOld

-- | Evaluates the statements, evaluating `prog` for each possible 
-- assingment to the generators that satisfies the qualifiers.
evalStmts :: (Value -> [Value]) -> [TCStmt] -> EvaluationMonad [a] -> 
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
                let (matches, binds) = bind p v
                if matches then 
                    addScopeAndBind binds (evStmts stmts)
                else return []) (extract v)
            return $ concat vss
    in
        evStmts (map unAnnotate anStmts)

-- | Takes a VEvent and then computes all events that this is a prefix of.
completeEvent :: Value -> EvaluationMonad S.ValueSet
completeEvent ev = do
    exs <- extensions ev
    l <- mapM (extendEvent ev) exs
    return $ S.fromList l

extendEvent :: Value -> Value -> EvaluationMonad Value
extendEvent ev exs = combineDots ev exs
