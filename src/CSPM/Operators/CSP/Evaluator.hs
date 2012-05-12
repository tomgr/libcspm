{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module CSPM.Operators.CSP.Evaluator () where

import Control.Monad.Trans
import qualified Data.Foldable as F
import Data.Maybe
import Data.Sequence ((<|), (|>))
import qualified Data.Sequence as Sq

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Expr
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import CSPM.Operators.CSP.PrettyPrinter
import CSPM.Operators.CSP.Processes
import CSPM.Operators.CSP.Syntax
import Util.Annotated
import Util.Exception
import Util.Monad
import Util.Prelude
import Util.PrettyPrint

instance BuiltInFunctions UnCompiledCSPOp where
    compressionOperator = UCSPOp . POperator
    extraBuiltInsDefinitions = 
        let   
            csp_chaos [VSet a] = VProc chaosCall
                where
                    chaosCall = PProcCall n p
                    n = procId (builtInName "CHAOS") [[VSet a]] Nothing
                    evSet = S.valueSetToEventSet a
                    branches :: Sq.Seq UnCompiledCSPProc
                    branches = fmap (\ ev -> PUnaryOp (UCSPOp $ PPrefix ev) chaosCall) evSet
                    stopProc = PProcCall (procId (builtInName "STOP") [] Nothing) csp_stop
                    p = POp (UCSPOp PInternalChoice) (stopProc Sq.<| branches)

            procs = [("STOP", csp_stop), ("SKIP", csp_skip)]
            csp_skip_id = procId (builtInName "SKIP") [] Nothing
            csp_stop_id = procId (builtInName "STOP") [] Nothing
            -- We actually inline stop, for efficiency
            csp_stop = POp (UCSPOp PExternalChoice) Sq.empty
            csp_skip = PUnaryOp (UCSPOp $ PPrefix Tick) csp_stop
            
            mkProc (s, p) = (builtInName s, VProc p)  
        in makeFunction ("CHAOS", csp_chaos) : map mkProc procs

type CSPValue = Value UnCompiledCSPOp
type CSPValueSet = S.ValueSet UnCompiledCSPOp
type CSPMonad a = EvaluationMonad UnCompiledCSPOp a

instance Evaluatable (CSPProcess Name) UnCompiledCSPOp where
    -- This is the most complicated process because it is actually a shorthand
    -- for external choice and internal choice.
    eval (Prefix e1 fs e2) =
        let
            evalInputField :: CSPValue -> [CSPField Name] -> 
                TCPat -> CSPValueSet -> 
                (CSPValue -> [CSPField Name] -> CSPMonad UnCompiledCSPProc) ->
                CSPMonad (Sq.Seq UnCompiledCSPProc)
            evalInputField evBase fs p s evalRest = do
                mps <- mapM (\v -> do
                    let (matches, binds) = bind p v
                    if matches then do
                        ev' <- combineDots evBase v
                        p <- addScopeAndBind binds (evalRest ev' fs)
                        return $ Just p
                    else return Nothing) (S.toList s)
                return $ Sq.fromList $ catMaybes mps
            
            -- | Evalutates an input field, deducing the correct set of values
            -- to input over.
            evalInputField2 :: 
                CSPValue -> 
                [CSPField Name] -> 
                Pat Name -> 
                (CSPValue -> [CSPField Name] -> CSPMonad UnCompiledCSPProc) ->
                (Sq.Seq UnCompiledCSPProc -> UnCompiledCSPProc) -> 
                CSPMonad UnCompiledCSPProc
            evalInputField2 evBase fs p evalRest procConstructor = 
                let
                    -- | The function to use to generate the options. If this
                    -- is the last field AND the last pattern in the current
                    -- field it uses 'extensions' to extend to a fully formed 
                    -- event, otherwise we use 'oneFieldExtensions' to extend 
                    -- by precisely one field.
                    extensionsOperator :: 
                        [Pat Name] -> CSPValue -> CSPMonad [CSPValue]
                    extensionsOperator ps | fs /= [] = oneFieldExtensions
                    extensionsOperator [p] = extensions 
                    extensionsOperator (p1:p2:ps) = oneFieldExtensions
                    
                    -- | Converts a pattern to its constituent fields.
                    patToFields :: Pat Name -> [Pat Name]
                    patToFields (PCompDot ps _) = map unAnnotate ps
                    patToFields (PDoublePattern p1 p2) = patToFields (unAnnotate p1)
                    patToFields p = [p]

                    -- | Given a value and a list of patterns (from 
                    -- 'patToFields') computes the appropriate set of events and
                    -- then evaluates it.
                    evExtensions :: CSPValue -> [Pat Name] -> [(Name, CSPValue)] -> 
                        CSPMonad (Sq.Seq UnCompiledCSPProc)
                    evExtensions evBase [] bs = do
                        p <- addScopeAndBind bs $ evalRest evBase fs
                        return $ Sq.singleton p
                    evExtensions evBase (PVar n:ps) bs | isNameDataConstructor n = do
                        VTuple [dc, _, _] <- lookupVar n
                        evBase' <- combineDots evBase dc
                        evExtensions evBase' ps bs
                    evExtensions evBase (p:ps) bs = do
                        vs <- extensionsOperator (p:ps) evBase
                        mps <- mapM (\v -> do
                                let 
                                    -- The extensions operator always returns a
                                    -- dot list. However, here, in the case the
                                    -- dotlist contains only one item, we do not
                                    -- want to bind to the dot list, rather to
                                    -- the value inside.
                                    extract (VDot [v]) = v
                                    extract v = v
                                    (matches, bs') = bind p (extract v)
                                if matches then do
                                    evBase' <- combineDots evBase v
                                    proc <- evExtensions evBase' ps (bs++bs')
                                    return $ Just proc
                                else return Nothing) vs
                        return $ F.msum $ catMaybes mps
                in do
                    ps <- evExtensions evBase (patToFields p) []
                    return $ procConstructor ps

            evalNonDetFields :: CSPValue -> [CSPField Name] -> 
                CSPMonad UnCompiledCSPProc
            evalNonDetFields evBase (NonDetInput p (Just e):fs) = do
                VSet s <- eval e
                ps <- evalInputField evBase fs p s evalNonDetFields
                return $ POp (UCSPOp PInternalChoice) ps
            evalNonDetFields evBase (NonDetInput p Nothing:fs) =
                evalInputField2 evBase fs (unAnnotate p) evalNonDetFields 
                    (POp (UCSPOp PInternalChoice))
            evalNonDetFields evBase fs = evalFields evBase fs

            evalFields :: CSPValue -> [CSPField Name] -> CSPMonad UnCompiledCSPProc
            evalFields ev [] = do
                -- TODO: check valid event
                p <- getParentProcName
                updateParentProcName (annonymousProcId [[ev]] p) $ do
                    p <- evalProc e2
                    return $ PUnaryOp (UCSPOp $ PPrefix (valueEventToEvent ev)) p
            evalFields evBase (Output e:fs) = do
                v <- eval e
                ev' <- combineDots evBase v
                evalFields ev' fs
            evalFields evBase (Input p (Just e):fs) = do
                VSet s <- eval e
                ps <- evalInputField evBase fs p s evalFields
                return $ POp (UCSPOp PExternalChoice) ps
            evalFields evBase (Input p Nothing:fs) =
                evalInputField2 evBase fs (unAnnotate p) evalFields 
                    (POp (UCSPOp PExternalChoice))
            evalFields evBase (NonDetInput _ _:fs) = 
                panic "Evaluation of $ after ! or ? is not supported."

            -- Takes a proc and combines nested [] and |~|
            --simplify :: UProc -> UProc
            --simplify (POp (PExternalChoice [p])) = simplify p
            --simplify (POp (PInternalChoice [p])) = simplify p
            --simplify (POp (PExternalChoice (ps@((PExternalChoice _):_))) =
            --    let extract (PExternalChoice ps) = ps in
            --    simplify (PExternalChoice (concatMap extract ps))
            --simplify (PExternalChoice ps) = PExternalChoice (map simplify ps)
            --simplify (PInternalChoice (ps@((PInternalChoice _):_))) =
            --    let extract (PInternalChoice ps) = ps in
            --    simplify (PInternalChoice (concatMap extract ps))
            --simplify (PInternalChoice ps) = PInternalChoice (map simplify ps)
            --simplify p = p
        in do
            ev@(VDot (VChannel n:vfs)) <- eval e1
            VTuple [_, VInt arity, VList fieldSets] <- lookupVar n
            p <- evalNonDetFields ev (map unAnnotate fs)
            return $ VProc p --(simplify p)

    eval (AlphaParallel e1 e2 e3 e4) = do
        p1 <- evalProc e1
        p2 <- evalProc e4
        VSet a1 <- eval e2
        VSet a2 <- eval e3
        return $ VProc $ POp (UCSPOp $ PAlphaParallel 
                (S.valueSetToEventSet a1 <| S.valueSetToEventSet a2 <| Sq.empty))
                (p1 <| p2 <| Sq.empty)
    eval (Exception e1 e2 e3) = do
        p1 <- evalProc e1
        VSet a <- eval e2
        p2 <- evalProc e3
        return $ VProc $ PBinaryOp (UCSPOp $ PException (S.valueSetToEventSet a)) p1 p2
    eval (ExternalChoice e1 e2) = do
        p1 <- evalProc e1
        p2 <- evalProc e2
        return $ VProc $ POp (UCSPOp PExternalChoice) (p1 <| p2 <| Sq.empty)
    eval (GenParallel e1 e2 e3) = do
        ps <- evalProcs [e1, e3]
        VSet a <- eval e2
        return $ VProc $ POp (UCSPOp $ PGenParallel (S.valueSetToEventSet a)) ps
    eval (GuardedExp guard proc) = do
        VBool b <- eval guard
        if b then eval proc else lookupVar (builtInName "STOP")
    eval (Hiding e1 e2) = do
        p <- evalProc e1
        VSet s <- eval e2
        return $ VProc $ PUnaryOp (UCSPOp $ PHide (S.valueSetToEventSet s)) p
    eval (InternalChoice e1 e2) = do
        ps <- evalProcs [e1, e2]
        return $ VProc $ POp (UCSPOp PInternalChoice) ps
    eval (Interrupt e1 e2) = do
        p1 <- evalProc e1
        p2 <- evalProc e2
        return $ VProc $ PBinaryOp (UCSPOp PInterrupt) p1 p2
    eval (Interleave e1 e2) = do
        ps <- evalProcs [e1, e2]
        return $ VProc $ POp (UCSPOp PInterleave) ps
    eval (LinkParallel e1 ties stmts e2) = do
        p1 <- evalProc e1
        p2 <- evalProc e2
        ts <- evalTies stmts ties
        return $ VProc $ PBinaryOp (UCSPOp $ PLinkParallel ts) p1 p2
    eval (Rename e1 ties stmts) = do
        p1 <- evalProc e1
        ts <- evalTies stmts ties
        return $ VProc $ PUnaryOp (UCSPOp $ PRename ts) p1
    eval (SequentialComp e1 e2) = do
        p1 <- evalProc e1
        p2 <- evalProc e2
        return $ VProc $ PBinaryOp (UCSPOp PSequentialComp) p1 p2
    eval (SlidingChoice e1 e2) = do
        p1 <- evalProc e1
        p2 <- evalProc e2
        return $ VProc $ PBinaryOp (UCSPOp PSlidingChoice) p1 p2
    
    eval (ReplicatedAlphaParallel stmts e1 e2) = do
        aps <- evalStmts' (\(VSet s) -> S.toSeq s) stmts $ do
            VSet s <- eval e1
            p <- evalProc e2
            return (S.valueSetToEventSet s, p)
        let (as, ps) = unzipSq aps
        return $ VProc $ POp (UCSPOp $ PAlphaParallel as) ps
    eval (ReplicatedExternalChoice stmts e) = do
        ps <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e)
        return $ VProc $ POp (UCSPOp PExternalChoice) ps
    eval (ReplicatedInterleave stmts e) = do
        ps <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e)
        return $ VProc $ POp (UCSPOp PInterleave) ps
    eval (ReplicatedInternalChoice stmts e) = do
        ps <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e)
        let e' = ReplicatedInternalChoice stmts e
        if Sq.null ps then
            throwError $ replicatedInternalChoiceOverEmptySetMessage (loc e) (unAnnotate e)
        else return $ VProc $ POp (UCSPOp PInternalChoice) ps
    eval (ReplicatedLinkParallel ties tiesStmts stmts e) = do
        tsps <- evalStmts' (\(VList vs) -> Sq.fromList vs) stmts $ do
            ts <- evalTies tiesStmts ties
            p <- evalProc e
            return (ts, p)
        let 
            (tsps' Sq.:> (_, lastProc)) = Sq.viewr tsps
            mkLinkPar (ts, p1) p2 = PBinaryOp (UCSPOp $ PLinkParallel ts) p1 p2
        return $ VProc $ F.foldr mkLinkPar lastProc tsps'
    eval (ReplicatedParallel e1 stmts e2) = do
        VSet s <- eval e1
        ps <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e2)
        return $ VProc $ POp (UCSPOp $ PGenParallel (S.valueSetToEventSet s)) ps
    
evalProcs :: (Evaluatable a ops, PrettyPrintable (UProc ops)) =>
    [a] -> EvaluationMonad ops (Sq.Seq (UProc ops))
evalProcs as = mapM evalProc as >>= return . Sq.fromList

evalProc :: (Evaluatable a ops, PrettyPrintable (UProc ops)) =>
    a -> EvaluationMonad ops (UProc ops)
evalProc a = eval a >>= \v -> case v of
    VProc x -> return x
    _       -> panic "Type checker error"

evalTies :: (Evaluatable (p Name) ops, PrettyPrintable (UProc ops)) =>
    [TCStmt p] -> [(TCExp p, TCExp p)] -> EvaluationMonad ops (Sq.Seq (Event ops, Event ops))
evalTies stmts ties = do
    tss <- evalStmts (\(VSet s) -> S.toList s) stmts (mapM evalTie ties)
    return $ Sq.fromList (concat tss)
    where
        extendTie :: (Value ops, Value ops) -> Value ops -> EvaluationMonad ops (Event ops, Event ops)
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

unzipSq :: Sq.Seq (a,b) -> (Sq.Seq a, Sq.Seq b)
unzipSq sqs = F.foldr 
    (\ (a,b) (as, bs) -> (a <| as, b <| bs) ) (Sq.empty, Sq.empty) sqs
