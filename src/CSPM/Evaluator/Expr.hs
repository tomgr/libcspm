{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CSPM.Evaluator.Expr (
    eval,
) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Sequence ((<|))
import qualified Data.Sequence as Sq
import qualified Data.Traversable as T

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.AnalyserMonad
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Dot
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.PrefixExpr
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import Util.Annotated
import Util.Exception
import Util.List

-- In order to keep lazy evaluation working properly only use pattern
-- matching when you HAVE to know the value. (Hence why we delay pattern
-- matching in BooleanBinaryOp And in case the first value is false.)

eval :: TCExp -> AnalyserMonad (EvaluationMonad Value)
eval (An _ _ (App func args)) = do
    func <- eval func
    args <- mapM eval args
    return $! do
        vs <- sequence args
        VFunction _ f <- func
        f vs
eval (An _ _ (BooleanBinaryOp op e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    let fn = case op of
            And -> \ v1 v2 ->
                let 
                    VBool b1 = v1
                    -- This is lazy, only pattern matches if b2 is required.
                    VBool b2 = v2
                in b1 && b2
            Or -> \ v1 v2 ->
                let 
                    VBool b1 = v1
                    -- This is lazy, only pattern matches if b2 is required.
                    VBool b2 = v2
                in b1 || b2
-- TODO: optimise these calls so that compareValues takes a target comparator
            Equals -> \ v1 v2 -> compareValues v1 v2 == Just EQ
            NotEquals -> \ v1 v2 -> compareValues v1 v2 /= Just EQ
            LessThan -> \ v1 v2 -> compareValues v1 v2 == Just LT
            GreaterThan -> \ v1 v2 -> compareValues v1 v2 == Just GT
            LessThanEq -> \ v1 v2 -> compareValues v1 v2 `elem` [Just LT, Just EQ]
            GreaterThanEq -> \ v1 v2 -> compareValues v1 v2 `elem` [Just GT, Just EQ]
    return $! do
        v1 <- e1
        v2 <- e2
        return $! VBool $ fn v1 v2
eval (An _ _ (BooleanUnaryOp Not e)) = do
    e <- eval e
    return $! do
        VBool b <- e
        return $ VBool (not b)
eval (An _ _ (Concat e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    return $! do
        VList vs1 <- e1
        v2 <- e2
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
eval e@(An loc _ (DotApp e1 e2)) = evaluateDotApplication e
eval (An _ _ (If e1 e2 e3)) = do
    e1 <- eval e1
    e2 <- eval e2
    e3 <- eval e3
    return $! do
        VBool b <- e1
        if b then e2 else e3
eval (An loc _ (Lambda ps e)) = do
    let functionId p = lambdaFunction (Lambda ps e) p
    binder <- bindAll ps 
    e <- eval e
    return $! do
        st <- CSPM.Evaluator.Monad.getState
        psid <- getParentScopeIdentifier
        let fid = functionId psid
        return $ VFunction fid $ \ vs -> return $ runEvaluator st $ do
            case binder vs of
                Just binds ->
                    updateParentScopeIdentifier (annonymousScopeId vs psid) $ 
                        addScopeAndBind binds e
                Nothing -> throwError $ patternMatchesFailureMessage loc ps vs
eval (An _ _ (Let decls e)) = do
    decls <- bindDecls decls
-- TODO: should analyse e in scope of decls
    e <- eval e
    return $! do
        nvs <- decls
        addScopeAndBindM nvs e
eval (An _ _ (Lit (Int i))) = return $! return $! VInt i
eval (An _ _ (Lit (Bool b))) = return $! return $! VBool b
eval (An _ _ (Lit (Char c))) = return $! return $! VChar c
eval (An _ _ (Lit (Loc l))) = return $! return $! VLoc l
eval (An _ _ (Lit (String s))) =
    let
        cs = map VChar s
    in return $! return $! VList cs
eval (An _ _ (List es)) = do
    es <- mapM eval es
    return $! sequence es >>= return . VList
eval (An _ _ (ListComp es stmts)) = do
    stmts <- evalStmts (\(VList xs) -> xs) stmts (map eval es)
    return $ stmts >>= return . VList
eval (An _ _ (ListEnumFrom e)) = do
    e <- eval e
    return $! do
        VInt lb <- e
        return $ VList $ map VInt [lb..]
eval (An _ _ (ListEnumFromTo e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    return $! do
        VInt lb <- e1
        VInt ub <- e2
        return $ VList $ map VInt [lb..ub]
eval (An _ _ (ListEnumFromComp e1 stmts)) = do
    stmts <- evalStmts (\ (VList xs) -> xs) stmts [do
        e1 <- eval e1
        return $ do
            VInt lb <- e1
            return $ map VInt [lb..]
        ]
    return $ do
        ss <- stmts
        return $! VList $ concat ss
eval (An _ _ (ListEnumFromToComp e1 e2 stmts)) = do
    stmts <- evalStmts (\ (VList xs) -> xs) stmts [do
        e1 <- eval e1
        e2 <- eval e2
        return $! do
            VInt lb <- e1
            VInt ub <- e2
            return $ map VInt [lb..ub]
        ]
    return $ do
        ss <- stmts
        return $! VList $ concat ss
eval (An _ _ (ListLength e)) = do
    e <- eval e
    return $! do
        VList xs <- e 
        return $ VInt (length xs)
eval (An _ _ (Map kvs)) = do
    xs <- mapM (\ (k, v) -> do
        k <- eval k
        v <- eval v
        return (k, v)) kvs
    return $ do
        xs <- mapM (\ (k, v) -> do
            k <- k
            v <- v
            return (k, v)) xs
        return $! VMap $ M.fromList xs
eval (An loc _ (MathsBinaryOp op e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    let fn = case op of
            Divide -> \ i1 i2 -> do
                case i2 of
                    0 -> do
                        scopeId <- getParentScopeIdentifier
                        throwError $ divideByZeroMessage loc scopeId
                    _ -> return $ VInt (i1 `div` i2)
            Minus -> \ i1 i2 -> return $ VInt (i1 - i2)
            Mod -> \ i1 i2 -> return $ VInt (i1 `mod` i2)
            Plus -> \ i1 i2 -> return $ VInt (i1 + i2)
            Times -> \ i1 i2 -> return $ VInt (i1 * i2)
    return $! do
        VInt i1 <- e1
        VInt i2 <- e2
        fn i1 i2
eval (An _ _ (MathsUnaryOp Negate e)) = do
    e <- eval e
    return $! do
        VInt i <- e
        return $ VInt (-i)
eval (An _ _ (Paren e)) = eval e
eval (An _ _ (Set es)) = do
    es <- mapM eval es
    return $! sequence es >>= return . VSet . S.fromList
eval (An _ _ (SetComp es stmts)) = do
    stmts <- evalStmts (\ (VSet s) -> S.toList s) stmts $! map eval es
    return $! do
        xs <- stmts
        return $ VSet (S.fromList xs)
eval (An _ _ (SetEnum es)) = do
    es <- mapM eval es
    return $! do
        evs <- sequence es
        ss <- mapM productionsSet evs
        return $ VSet (S.unions ss)
eval (An _ _ (SetEnumComp es stmts)) = do
    stmts <- evalStmts (\(VSet s) -> S.toList s) stmts $ map (\ e -> do
        e <- eval e
        return $! e >>= productionsSet) es
    return $! do
        ss <- stmts
        return $ VSet (S.unions ss)
eval (An _ _ (SetEnumFrom e)) = do
    e <- eval e
    return $! do
        VInt lb <- e
        return $ VSet $ S.IntSetFrom lb
eval (An _ _ (SetEnumFromTo e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    return $! do
        VInt lb <- e1
        VInt ub <- e2
        return $ VSet $ S.fromList $ map VInt [lb..ub]
eval (An _ _ (SetEnumFromComp e1 stmts)) = do
    stmts <- evalStmts (\ (VSet s) -> S.toList s) stmts [do
            e1 <- eval e1
            return $! do
                VInt lb <- e1
                return $! S.IntSetFrom lb
        ]
    return $! do
        ss <- stmts
        return $ VSet $ S.unions ss
eval (An _ _ (SetEnumFromToComp e1 e2 stmts)) = do
    stmts <- evalStmts (\ (VSet s) -> S.toList s) stmts [do
        e1 <- eval e1
        e2 <- eval e2
        return $! do
            VInt lb <- e1
            VInt ub <- e2
            return $! S.fromList (map VInt [lb..ub])
        ]
    return $! do 
        ss <- stmts
        return $ VSet $ S.unions ss
eval (An _ _ (Tuple es)) = do
    es <- mapM eval es
    return $! do
        vs <- sequence es
        return $ tupleFromList vs
eval (An _ _ (Var n)) | isNameDataConstructor n = return $ do
    (dc, _, _) <- dataTypeInfo n
    return dc
eval (An _ _ (Var n)) = return $! lookupVar n

eval (an@(An _ _ (Prefix e1 fs e2))) = evalPrefix an
eval (An _ _ (TimedPrefix n e)) = do
    e <- eval e
    maybeTimed (panic "Timed prefix in non-timed section") $ \ tockName fnName -> return $ do
        VProc p <- e
        parentScope <- getParentScopeIdentifier
        VFunction _ eventFunc <- lookupVar fnName
        let addTocker (POp PExternalChoice ps) = do
                ps' <- T.mapM addTocker ps
                return $! POp PExternalChoice ps'
            addTocker (PUnaryOp (PPrefixEventSet evs) p) =
-- sort out - destination might be diferent in events require different amounts
-- of time
                panic "TODO: sprtout"
            addTocker (PUnaryOp (PPrefix ev) p) = do
                let UserEvent ev' = ev
                VInt tockCount <- eventFunc [ev']
                return $! PUnaryOp (PPrefix ev) (makeTocker tockName tockCount p)
        p' <- addTocker p
        let
            tocker = PUnaryOp (PPrefix (tock tockName)) procCall
            mainProc = POp PExternalChoice (tocker <| p' <| Sq.empty)
            procCall = PProcCall (procName $ scopeId n [] parentScope) mainProc
        return $ VProc procCall

eval (An _ _ (AlphaParallel e1 e2 e3 e4)) = do
    e1 <- eval e1
    e2 <- timedCSPSyncSet $ eval e2
    e3 <- timedCSPSyncSet $ eval e3
    e4 <- eval e4
    return $! do
        VProc p1 <- e1
        VProc p2 <- e4
        VSet a1 <- e2
        VSet a2 <- e3
        return $ VProc $ POp (PAlphaParallel 
                (S.valueSetToEventSet a1 <| S.valueSetToEventSet a2 <| Sq.empty))
                (p1 <| p2 <| Sq.empty)
eval (An _ _ (Exception e1 e2 e3)) = do
    e1 <- eval e1
    e2 <- eval e2
    e3 <- eval e3
    return $! do
        VProc p1 <- e1
        VSet a <- e2
        VProc p2 <- e3
        return $ VProc $ PBinaryOp (PException (S.valueSetToEventSet a)) p1 p2
eval (An _ _ (ExternalChoice e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    op <- maybeTimed
        (return $ VProc . POp PExternalChoice)
        (\ tn _ -> return $ VProc . POp (PSynchronisingExternalChoice (tockSet tn)))
    return $! do
        VProc p1 <- e1
        VProc p2 <- e2
        return $! op (p1 <| p2 <| Sq.empty)
eval (An _ _ (GenParallel e1 e2 e3)) = do
    e1 <- eval e1
    e2 <- timedCSPSyncSet $ eval e2
    e3 <- eval e3
    return $! do
        VProc p1 <- e1
        VSet a <- e2
        VProc p2 <- e3
        let ps = p1 <| p2 <| Sq.empty
        return $ VProc $ POp (PGenParallel (S.valueSetToEventSet a)) ps
eval (An _ _ (GuardedExp guard proc)) = do
    guard <- eval guard
    proc <- eval proc
    stop <- maybeTimed
        (return $! lookupVar (builtInName "STOP"))
        (\ tn _ -> return $! do
            VFunction _ fn <- lookupVar (builtInName "TSTOP")
            fn [tockValue tn])
    return $! do
        VBool b <- guard
        if b then proc
        else stop
eval (An _ _ (Hiding e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    return $! do
        VProc p <- e1
        VSet s <- e2
        if S.empty s then return $ VProc p
        else return $ VProc $ PUnaryOp (PHide (S.valueSetToEventSet s)) p
eval (An _ _ (InternalChoice e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    return $! do
        VProc p1 <- e1
        VProc p2 <- e2
        return $ VProc $ POp PInternalChoice (p1 <| p2 <| Sq.empty)
eval (An _ _ (Interrupt e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    op <- maybeTimed
        (return PInterrupt)
        (\ tn _ -> return $ PSynchronisingInterrupt (tockSet tn))
    return $! do
        VProc p1 <- e1
        VProc p2 <- e2
        return $! VProc $ PBinaryOp op p1 p2
eval (An _ _ (Interleave e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    op <- maybeTimed
        (return PInterleave)
        (\ tn _ -> return $ PGenParallel (tockSet tn))
    return $! do
        VProc p1 <- e1
        VProc p2 <- e2
        return $ VProc $ POp op (p1 <| p2 <| Sq.empty)
eval (An _ _ (LinkParallel e1 ties stmts e2)) = do
    e1 <- eval e1
    ties <- evalTies stmts ties
    e2 <- eval e2
    return $! do
        VProc p1 <- e1
        VProc p2 <- e2
        ts <- ties
        return $ VProc $
            PBinaryOp (PLinkParallel (removeDuplicateTies ts)) p1 p2
eval (An _ _ (Project e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    return $! do
        VProc p <- e1
        VSet s <- e2
        return $ VProc $ PUnaryOp (PProject (S.valueSetToEventSet s)) p
eval (An _ _ (Rename e1 ties stmts)) = do
    e1 <- eval e1
    ties <- evalTies stmts ties
    return $! do
        VProc p1 <- e1
        ts <- ties
        return $ VProc $ if Sq.null ts then p1 else
            PUnaryOp (PRename (removeDuplicateTies ts)) p1
eval (An _ _ (SequentialComp e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    return $! do
        VProc p1 <- e1
        VProc p2 <- e2
        return $ VProc $ PBinaryOp PSequentialComp p1 p2
eval (An _ _ (SlidingChoice e1 e2)) = do
    e1 <- eval e1
    e2 <- eval e2
    return $! do
        VProc p1 <- e1
        VProc p2 <- e2
        return $ VProc $ PBinaryOp PSlidingChoice p1 p2
eval (An _ _ (SynchronisingExternalChoice e1 e2 e3)) = do
    e1 <- eval e1
    e2 <- timedCSPSyncSet $ eval e2
    e3 <- eval e3
    return $! do
        VProc p1 <- e1
        VSet a <- e2
        VProc p2 <- e3
        return $ VProc $ POp
            (PSynchronisingExternalChoice (S.valueSetToEventSet a))
            (p1 <| p2 <| Sq.empty)
eval (An _ _ (SynchronisingInterrupt e1 e2 e3)) = do
    e1 <- eval e1
    e2 <- timedCSPSyncSet $ eval e2
    e3 <- eval e3
    return $! do
        VProc p1 <- e1
        VSet a <- e2
        VProc p2 <- e3
        return $ VProc $
            PBinaryOp (PSynchronisingInterrupt (S.valueSetToEventSet a)) p1 p2

eval (An _ _ (ReplicatedAlphaParallel stmts e1 e2)) = do
    stmts <- evalStmts' (\(VSet s) -> S.toSeq s) stmts $ do
        e1 <- timedCSPSyncSet $ eval e1
        e2 <- eval e2
        return $! do
            VSet s <- e1
            VProc p <- e2
            return (S.valueSetToEventSet s, p)
    tstop <- maybeTSTOP
    return $! do
        aps <- stmts
        let (as, ps) = unzipSq aps
        tstop ps $ return $ VProc $ POp (PAlphaParallel as) ps
eval (An _ _ (ReplicatedExternalChoice stmts e)) = do
    stmts <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e)
    tstop <- maybeTSTOP
    op <- maybeTimed
        (return PExternalChoice)
        (\ tn _ -> return $ PSynchronisingExternalChoice (tockSet tn))
    return $! do
        ps <- stmts
        tstop ps (return $ VProc $ POp op ps)
eval (An _ _ (ReplicatedInterleave stmts e)) = do
    stmts <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e)
    tstop <- maybeTSTOP
    op <- maybeTimed
        (return PInterleave)
        (\tn _ -> return $ PGenParallel (tockSet tn))
    return $! do
        ps <- stmts
        tstop ps (return $ VProc $ POp op ps)
eval (e'@(An _ _ (ReplicatedInternalChoice stmts e))) = do
    stmts <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e)
    return $! do
        ps <- stmts
        if Sq.null ps then
            throwError' $ replicatedInternalChoiceOverEmptySetMessage e'
        else return $ VProc $ POp PInternalChoice ps
eval (An loc _ e'@(ReplicatedLinkParallel ties tiesStmts stmts e)) = do
    stmts <- evalStmts' (\(VList vs) -> Sq.fromList vs) stmts $ do
        ties <- evalTies tiesStmts ties
        e <- evalProc e
        return $! do
            ts <- ties
            p <- e
            return (ts, p)
    return $! do
        tsps <- stmts
        if Sq.null tsps then
            throwError' $ replicatedLinkParallelOverEmptySeqMessage e' loc
        else do
        let
            (tsps' Sq.:> (_, lastProc)) = Sq.viewr tsps
            mkLinkPar (ts, p1) p2 =
                PBinaryOp (PLinkParallel (removeDuplicateTies ts)) p1 p2
        return $ VProc $ F.foldr mkLinkPar lastProc tsps'
eval (An _ _ (ReplicatedParallel e1 stmts e2)) = do
    e1 <- timedCSPSyncSet $ eval e1
    stmts <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e2)
    tstop <- maybeTSTOP
    return $! do
        VSet s <- e1
        ps <- stmts
        tstop ps $ 
            return $ VProc $ POp (PGenParallel (S.valueSetToEventSet s)) ps
eval (An _ _ (ReplicatedSequentialComp stmts e)) = do
    stmts <- evalStmts' (\(VList vs) -> Sq.fromList vs) stmts (evalProc e)
    skip <- maybeTimed
        (return $ lookupVar (builtInName "SKIP"))
        (\tn _ -> return $ tSKIP tn)
    return $! do
        ps <- stmts
        if Sq.null ps then skip
        else return $ VProc $ F.foldr1 (PBinaryOp PSequentialComp) ps
eval (An _ _ (ReplicatedSynchronisingExternalChoice e1 stmts e2)) = do
    e1 <- timedCSPSyncSet $ eval e1
    stmts <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e2)
    tstop <- maybeTSTOP
    return $! do    
        VSet a <- e1
        ps <- stmts
        tstop ps $ return $ VProc $ POp
            (PSynchronisingExternalChoice (S.valueSetToEventSet a)) ps

eval e = panic ("No clause to eval "++show e)

evalProc :: TCExp -> AnalyserMonad (EvaluationMonad UProc)
evalProc e = do
    prog <- eval e
    return $! do
        VProc p <- prog
        return p

removeDuplicateTies :: Sq.Seq (Event, Event) -> Sq.Seq (Event, Event)
removeDuplicateTies = Sq.fromList . sortedNub . F.toList . Sq.unstableSort

evalTies :: [TCStmt] -> [(TCExp, TCExp)] ->
    AnalyserMonad (EvaluationMonad (Sq.Seq (Event, Event)))
evalTies stmts ties = do
    tss <- evalStmts (\(VSet s) -> S.toList s) stmts (map evalTie ties)
    return $ do
        vss <- tss
        return $! Sq.fromList $ concat vss
    where
        extendTie :: SrcSpan -> SrcSpan -> (Value, Value) -> Value ->
            EvaluationMonad (Event, Event)
        extendTie loc1 loc2 (evOld, evNew) ex = do
            ev1 <- combineDots loc1 evOld ex
            ev2 <- combineDots loc2 evNew ex
            return (valueEventToEvent ev1, valueEventToEvent ev2)

        evalTie :: (TCExp, TCExp) -> 
            AnalyserMonad (EvaluationMonad [(Event, Event)])
        evalTie (eOld, eNew) = do
            eOld' <- eval eOld
            eNew' <- eval eNew
            return $! do
                evOld <- eOld'
                evNew <- eNew'
                -- Obviously evOld and evNew could be channels, or prefixes
                -- of events so we compute the extensions.
                exsOld <- extensions evOld
                mapM (\ex -> extendTie (loc eOld) (loc eNew) (evOld, evNew) ex) exsOld

-- | Evaluates the statements, evaluating each prog in progs for each possible 
-- assingment to the generators that satisfies the qualifiers.
evalStmts :: (Value -> [Value]) -> [TCStmt] ->
    [AnalyserMonad (EvaluationMonad a)] -> 
    AnalyserMonad (EvaluationMonad [a])
evalStmts extract stmts progs =
    let
        -- | Progressively generates new values lazily
        evStmts [] = do
            progs <- sequence progs
            return $! sequence progs
        evStmts (An _ _ (Qualifier e):stmts) = do
            e <- eval e
            rest <- evStmts stmts
            return $! do
                VBool b <- e
                if b then rest else return []
        evStmts (An _ _ (Generator p e):stmts) = do
            e <- eval e
            binder <- bind p
            rest <- evStmts stmts
            return $! do
                v <- e
                vss <- mapM (\v -> do
                    case binder v of
                        Just binds -> do
                            pid <- getParentScopeIdentifier
                            updateParentScopeIdentifier (annonymousScopeId [v] pid) $
                                addScopeAndBind binds rest
                        Nothing -> return []) (extract v)
                return $ concat vss
    in
        evStmts stmts

-- | Evaluates the statements, evaluating `prog` for each possible 
-- assingment to the generators that satisfies the qualifiers.
evalStmts' :: (Value -> Sq.Seq Value) -> [TCStmt] ->
    AnalyserMonad (EvaluationMonad a) -> 
    AnalyserMonad (EvaluationMonad (Sq.Seq a))
evalStmts' extract stmts prog =
    let
        evStmts [] = do
            prog <- prog
            return $! prog >>= return . Sq.singleton
        evStmts (An _ _ (Qualifier e) : stmts) = do
            e <- eval e
            rest <- evStmts stmts
            return $! do
                VBool b <- e
                if b then rest else return Sq.empty
        evStmts (An _ _ (Generator p e) : stmts) = do
            e <- eval e
            binder <- bind p
            rest <- evStmts stmts
            return $! do
                v <- e
                F.foldlM (\ s v -> do
                    case binder v of
                        Just binds -> do
                            pid <- getParentScopeIdentifier
                            s' <- updateParentScopeIdentifier (annonymousScopeId [v] pid) $
                                    addScopeAndBind binds rest
                            return $ s Sq.>< s'
                        _ -> return s) Sq.empty (extract v)
    in evStmts stmts

unzipSq :: Sq.Seq (a,b) -> (Sq.Seq a, Sq.Seq b)
unzipSq sqs = F.foldr 
    (\ (a,b) (as, bs) -> (a <| as, b <| bs) ) (Sq.empty, Sq.empty) sqs

timedCSPSyncSet ::
    AnalyserMonad (EvaluationMonad Value) ->
    AnalyserMonad (EvaluationMonad Value)
timedCSPSyncSet prog = do
    maybeTimed prog $! \ tn _ -> do
        prog <- prog
        let set = S.fromList [tockValue tn]
        return $! do
            VSet a <- prog
            return $ VSet (S.union set a)
        
tSTOP :: Name -> EvaluationMonad Value
tSTOP tockName = do
    VFunction _ tstop <- lookupVar (builtInName "TSTOP")
    tstop [tockValue tockName]

tSKIP :: Name -> EvaluationMonad Value
tSKIP tockName = do
    VFunction _ tskip <- lookupVar (builtInName "TSKIP")
    tskip [tockValue tockName]

maybeTSTOP :: AnalyserMonad
    (Sq.Seq a -> EvaluationMonad Value -> EvaluationMonad Value)
maybeTSTOP =
    maybeTimed
        (return $ \ _ prog -> prog)
        (\ tn _ -> return $ \ sq p1 -> if Sq.null sq then tSTOP tn else p1)

tock :: Name -> Event
tock tn = UserEvent (tockValue tn)

tockValue :: Name -> Value
tockValue tn = VDot [VChannel tn]

tockSet :: Name -> EventSet
tockSet tn = Sq.singleton (tock tn)

makeTocker :: Name -> Int -> UProc -> UProc
makeTocker tn 0 p = p
makeTocker tn tocks p =
    PUnaryOp (PPrefix (tock tn)) (makeTocker tn (tocks-1) p)

dataTypeTypeName :: Type -> Name
dataTypeTypeName (TDatatype n) = n
dataTypeTypeName TEvent = builtInName "Events"
dataTypeTypeName t = panic $ show t ++ " is not a datatype type name."

-- | Evaluates a dot application, attempting to optimise it.
evaluateDotApplication :: TCExp -> AnalyserMonad (EvaluationMonad Value)
evaluateDotApplication (exp@(An loc _ (DotApp left right))) = do
    let
        leftMostDot (An _ _ (DotApp l r)) = (leftMost, args ++ [r])
            where (leftMost, args) = leftMostDot l
        leftMostDot x = (x, [])

        (leftMostConstructor, arguments) = leftMostDot exp

        findFields :: [Type] -> [TCExp] -> Maybe [TCExp]
        findFields [] _ = panic "Empty type list for find fields"
        findFields [t] [exp] | getType exp == t = Just [exp]
        findFields (t:ts) (exp:exps) | t == getType exp =
            case findFields ts exps of
                Just fs -> Just $ exp : fs
                Nothing -> Nothing
        findFields (t:ts) (An _ typ (DotApp l r) : exps) =
            -- We know t != typ, so we try splitting
            findFields (t:ts) (l:r:exps)
        findFields  _ _ = Nothing

        isDotable (TDotable _ _) = True
        isDotable (TExtendable _ _) = True
        isDotable _ = False

        catVDots v v'@(VDot (VDataType _ : vs)) = VDot [v, v']
        catVDots v v'@(VDot (VChannel _ : vs)) = VDot [v, v']
        catVDots v (VDot vs) = VDot (v:vs)
        catVDots v v' = VDot [v, v']

        fallback = do
            e1 <- eval left
            e2 <- eval right
            if not (isDotable (getType left)) then
                return $! do
                    v1 <- e1
                    v2 <- e2
                    return $! catVDots v1 v2
            else return $! do
                    v1 <- e1
                    v2 <- e2
                    combineDots loc v1 v2
    if isDataTypeOrEvent (getType exp) then
        case leftMostConstructor of
            An _ _ (Var n) -> do
                dataType <- dataTypeForName (dataTypeTypeName (getType exp))
                case M.lookup n (dataTypeConstructors dataType) of
                    Just clause | constructorFieldCount clause > 0
                            && and (constructorFieldSetIsTrivial clause) ->
                        case findFields (constructorFieldTypes clause) arguments of
                            Just fs -> do
                                computeFields <- mapM eval fs
                                let constructor =
                                        case getType exp of
                                            TEvent -> VChannel n
                                            TDatatype _ -> VDataType n
                                return $! do
                                    fs <- sequence computeFields
                                    return $! VDot $! constructor : fs
                            _ -> fallback
                    _ -> fallback
            _ -> fallback
        else fallback
