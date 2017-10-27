{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}
module CSPM.Evaluator.Expr (
    eval,
) where

import qualified Data.ByteString.Char8 as B
import Data.List (sort)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as St
import qualified Data.Traversable as T

import CSPM.Syntax.FreeVars
import CSPM.Syntax.Literals
import CSPM.Syntax.Names
import CSPM.Syntax.AST
import CSPM.Syntax.Types
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

import CSPM.Prelude

-- In order to keep lazy evaluation working properly only use pattern
-- matching when you HAVE to know the value. (Hence why we delay pattern
-- matching in BooleanBinaryOp And in case the first value is false.)

eval :: TCExp -> AnalyserMonad (EvaluationMonad Value)
eval (An loc _ (LocatedApp func args)) = do
    func <- eval func
    args <- mapM eval args
    recordTraces <- shouldRecordStackTraces
    return $! do
        vs <- sequence args
        VFunction frame func <- func
        if recordTraces then registerFrame frame loc (func vs) else func vs
eval (An loc _ (App func args)) = do
    func <- eval func
    args <- mapM eval args
    recordTraces <- shouldRecordStackTraces
    return $! do
        vs <- sequence args
        VFunction frame func <- func
        newFrame <- reinstantiateFrameWithArguments frame (instantiatedFrameMaybeArguments frame ++ [vs])
        if recordTraces then registerFrame newFrame loc (func vs) else func vs

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
        return $! makeBoolValue $ fn v1 v2
eval (An _ _ (BooleanUnaryOp Not e)) = do
    e <- eval e
    return $! do
        VBool b <- e
        return $ makeBoolValue (not b)
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
eval e@(An _ _ (DotApp _ _)) = evaluateDotApplication e
eval (An _ _ (If e1 e2 e3)) = do
    e1 <- eval e1
    e2 <- eval e2
    e3 <- eval e3
    return $! do
        VBool b <- e1
        if b then e2 else e3
eval exp@(An loc _ (Lambda ps e)) = do
    binder <- bindAll ps
    outerFrameInfo <- createLambdaFrame ps e return
    createLambdaFrame ps e $! \ _ -> do
        e <- eval e
        return $! do
            fid <- instantiateFrame outerFrameInfo
            createFunction fid $! \ vs ->
                case binder vs of
                    Just binds -> addScopeAndBind binds e
                    Nothing -> throwError $ patternMatchesFailureMessage loc ps vs
eval (An _ _ (Let decls e)) = do
    analyseRelevantVars decls $! do
        decls <- bindDecls decls
        e <- eval e
        return $! do
            nvs <- decls
            addScopeAndBindM nvs e
eval (An _ _ (Lit (Int i))) = return $! return $! VInt i
eval (An _ _ (Lit (Bool b))) = return $! return $! makeBoolValue b
eval (An _ _ (Lit (Char c))) = return $! return $! VChar c
eval (An _ _ (Lit (Loc l))) = return $! return $! VLoc l
eval (An _ _ (Lit (String s))) =
    let
        cs = map VChar (B.unpack s)
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
                    0 -> throwError' $ divideByZeroMessage loc
                    _ -> return $ VInt (i1 `div` i2)
            Minus -> \ i1 i2 -> return $ VInt (i1 - i2)
            Mod -> \ i1 i2 ->
                case i2 of
                    0 -> throwError' $ divideByZeroMessage loc
                    _ -> return $ VInt (i1 `mod` i2)
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
eval (An _ _ (TimedPrefix n e)) =
    maybeTimed (panic "Timed prefix in non-timed section") $ \ tockName fnName -> do
        createFunctionFrame n [] e $! \frameInfo -> do
            e <- eval e
            return $ do
                VProc p <- e
                pn <- makeProcessName frameInfo
                VFunction _ eventFunc <- lookupVar fnName
                let addTocker (POp PExternalChoice ps) = do
                        ps' <- T.mapM addTocker ps
                        return $! POp PExternalChoice ps'
                    addTocker (PUnaryOp (PPrefixEventSet evs) p) =
                        let ps = fmap (\ev -> PUnaryOp (PPrefix ev) p) (F.toList evs)
                        in addTocker (POp PExternalChoice ps)
                    addTocker (PUnaryOp (PPrefix ev) p) = do
                        let UserEvent ev' = ev
                        VInt tockCount <- eventFunc [ev']
                        return $! PUnaryOp (PPrefix ev) (makeTocker tockName tockCount p)
                    addTocker p = return p
                p' <- addTocker p
                let
                    tocker = PUnaryOp (PPrefix (tock tockName)) procCall
                    mainProc = POp PExternalChoice [tocker, p']
                    procCall = PProcCall pn mainProc
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
                [S.valueSetToEventSet a1, S.valueSetToEventSet a2])
                [p1, p2]
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
    stop <- maybeTimed
        (return $! lookupVar (builtInName "STOP"))
        (\ tn _ -> return $! do
            VFunction _ fn <- lookupVar (builtInName "TSTOP")
            fn [tockValue tn])
    let
        collectExternals (An _ _ (ExternalChoice e1 e2)) = e1 : collectExternals e2
        collectExternals e = [e]
        
        isStop (InstantiatedFrame _ frame@(BuiltinFunctionFrame {}) _ _) =
            builtinFunctionFrameFunctionName frame == builtInName "STOP"
        isStop _ = False
        
        reduce [] [] = do
            VProc stop <- stop
            return [stop]
        reduce [] ps = return ps
        reduce (PProcCall (ProcName pn) _ : ps) ps' | isStop pn = reduce ps ps'
        reduce (p : ps) ps' = reduce ps (p:ps')
        
        unProc (VProc p) = p
        
    e1 <- eval e1
    e2 <- mapM eval (collectExternals e2)
    op <- maybeTimed
        (return $ VProc . POp PExternalChoice)
        (\ tn _ -> return $ VProc . POp (PSynchronisingExternalChoice (tockSet tn)))
    return $! do
        VProc p1 <- e1
        ps <- sequence e2
        ps' <- reduce (p1 : map unProc ps) []
        return $! op $ reverse ps'
eval (An _ _ (GenParallel e1 e2 e3)) = do
    e1 <- eval e1
    e2 <- timedCSPSyncSet $ eval e2
    e3 <- eval e3
    return $! do
        VProc p1 <- e1
        VSet a <- e2
        VProc p2 <- e3
        return $ VProc $ POp (PGenParallel (S.valueSetToEventSet a)) [p1, p2]
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
        return $ VProc $ POp PInternalChoice [p1, p2]
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
        return $ VProc $ POp op [p1, p2]
eval (An loc _ (LinkParallel e1 ties stmts e2)) = do
    e1 <- eval e1
    ties <- evalTies stmts ties
    e2 <- eval e2
    return $! do
        VProc p1 <- e1
        VProc p2 <- e2
        ts <- ties
        let (lefts, rights) = unzip ts
            check evs p = case firstDuplicate $ sort evs of
                            Just ev -> throwError' $ linkParallelAmbiguous ev loc
                            Nothing -> p
        check lefts $ check rights $ return $ VProc $ PBinaryOp (PLinkParallel (removeDuplicateTies ts)) p1 p2
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
        case ts of
            [] -> return $! VProc p1
            _ -> return $! VProc $! PUnaryOp (PRename (removeDuplicateTies ts)) p1
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
            [p1, p2]
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
    stmts <- evalStmts (\(VSet s) -> S.toList s) stmts [do
        e1 <- timedCSPSyncSet $ eval e1
        e2 <- eval e2
        return $! do
            VSet s <- e1
            VProc p <- e2
            return (S.valueSetToEventSet s, p)]
    tstop <- maybeTSTOP
    return $! do
        aps <- stmts
        let (as, ps) = unzip aps
        tstop ps $! return $! VProc $! POp (PAlphaParallel as) ps
eval (An _ _ (ReplicatedExternalChoice stmts e)) = do
    stmts <- evalStmts (\(VSet s) -> S.toList s) stmts [evalProc e]
    tstop <- maybeTSTOP
    op <- maybeTimed
        (return PExternalChoice)
        (\ tn _ -> return $ PSynchronisingExternalChoice (tockSet tn))
    return $! do
        ps <- stmts
        tstop ps (return $! VProc $! POp op $! ps)
eval (An _ _ (ReplicatedInterleave stmts e)) = do
    stmts <- evalStmts (\(VSet s) -> S.toList s) stmts [evalProc e]
    tstop <- maybeTSTOP
    op <- maybeTimed
        (return PInterleave)
        (\tn _ -> return $ PGenParallel (tockSet tn))
    return $! do
        ps <- stmts
        tstop ps (return $! VProc $! POp op $! ps)
eval (e'@(An _ _ (ReplicatedInternalChoice stmts e))) = do
    stmts <- evalStmts (\(VSet s) -> S.toList s) stmts [evalProc e]
    return $! do
        ps <- stmts
        case ps of
            [] -> throwError' $ replicatedInternalChoiceOverEmptySetMessage e'
            _ ->return $! VProc $! POp PInternalChoice $! ps
eval (An loc _ e'@(ReplicatedLinkParallel ties tiesStmts stmts e)) = do
    stmts <- evalStmts (\(VList vs) -> vs) stmts [do
        ties <- evalTies tiesStmts ties
        e <- evalProc e
        return $! do
            ts <- ties
            p <- e
            let (lefts, rights) = unzip ts
                check evs p = case firstDuplicate $ sort evs of
                                Just ev -> throwError' $ linkParallelAmbiguous ev loc
                                Nothing -> p
            check lefts $ check rights $ return (ts, p)
        ]
    let mkLinkPar [(_, p)] = p
        mkLinkPar ((ts, p1):ps) =
            PBinaryOp (PLinkParallel (removeDuplicateTies ts)) p1 (mkLinkPar ps)
    return $! do
        tsps <- stmts
        case tsps of
            [] -> throwError' $ replicatedLinkParallelOverEmptySeqMessage e' loc
            _ -> return $! VProc $! mkLinkPar tsps
eval (An _ _ (ReplicatedParallel e1 stmts e2)) = do
    e1 <- timedCSPSyncSet $ eval e1
    stmts <- evalStmts (\(VSet s) -> S.toList s) stmts [evalProc e2]
    tstop <- maybeTSTOP
    return $! do
        VSet s <- e1
        ps <- stmts
        tstop ps $ return $ VProc $ POp (PGenParallel (S.valueSetToEventSet s))
            (ps)
eval (An _ _ (ReplicatedSequentialComp stmts e)) = do
    stmts <- evalStmts (\(VList vs) -> vs) stmts [evalProc e]
    skip <- maybeTimed
        (return $ lookupVar (builtInName "SKIP"))
        (\tn _ -> return $ tSKIP tn)
    return $! do
        ps <- stmts
        case ps of
            [] -> skip
            _ -> return $ VProc $ foldr1 (PBinaryOp PSequentialComp) ps
eval (An _ _ (ReplicatedSynchronisingExternalChoice e1 stmts e2)) = do
    e1 <- timedCSPSyncSet $ eval e1
    stmts <- evalStmts (\(VSet s) -> S.toList s) stmts [evalProc e2]
    tstop <- maybeTSTOP
    return $! do    
        VSet a <- e1
        ps <- stmts
        tstop ps $ return $ VProc $ POp
            (PSynchronisingExternalChoice (S.valueSetToEventSet a))
            (ps)

eval e = panic ("No clause to eval "++show e)

evalProc :: TCExp -> AnalyserMonad (EvaluationMonad Proc)
evalProc e = do
    prog <- eval e
    return $! do
        VProc p <- prog
        return p

removeDuplicateTies :: [(Event, Event)] -> [(Event, Event)]
removeDuplicateTies = sortedNub . sort

evalTies :: [TCStmt] -> [(TCExp, TCExp)] ->
    AnalyserMonad (EvaluationMonad [(Event, Event)])
evalTies stmts ties = do
    tss <- evalStmts (\(VSet s) -> S.toList s) stmts (map evalTie ties)
    return $ do
        vss <- tss
        return $! concat vss
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

-- TODO: modify so that if the statement generators are independent, only
-- compute each set once, rather than once for each value.

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
            rest <- createVariableFrame' p $ evStmts stmts
            return $! do
                v <- e
                vss <- mapM (\v -> do
                    case binder v of
                        Just binds -> addScopeAndBind binds rest
                        Nothing -> return []) (extract v)
                return $ concat vss

        isGenerator (An _ _ (Generator _ _)) = True
        isGenerator _ = False

        generators = filter isGenerator stmts
        generatorFvs = St.fromList (freeVars generators)

        generatorsIndependent = and $!
            map (not . flip St.member generatorFvs) (boundNames generators)

        evGeneratorSets :: [TCStmt] -> AnalyserMonad (EvaluationMonad [Value])
        evGeneratorSets [] = return $! return []
        evGeneratorSets (An _ _ (Qualifier _):stmts) = evGeneratorSets stmts
        evGeneratorSets (An _ _ (Generator _ e):stmts) = do
            e <- eval e
            rest <- evGeneratorSets stmts
            return $! do
                v <- e
                vs <- rest
                return $! v : vs

        evBinders [] = do
            progs <- sequence progs
            return $! \ _ -> sequence progs
        evBinders (An _ _ (Qualifier e):stmts) = do
            e <- eval e
            rest <- evBinders stmts
            return $! \ sets -> do
                VBool b <- e
                if b then rest sets else return []
        evBinders (An _ _ (Generator p _):stmts) = do
            binder <- bind p
            rest <- createVariableFrame' p $ evBinders stmts
            return $! \ (set:sets) -> do
                vss <- mapM (\v -> do
                    case binder v of
                        Just binds -> addScopeAndBind binds (rest sets)
                        Nothing -> return []) (extract set)
                return $ concat vss
    in
        -- If there are multiple generators that are independent, split the
        -- evaluation into two stages: firstly compute the sets, and then take
        -- the cartesian product and do the binding etc. This has the advantage
        -- of only computing the sets once each, rather than computing the
        -- subsequent sets for *each* value of the first generator.
        if generatorsIndependent && length generators > 1 then do
            setGenerator <- evGeneratorSets stmts
            binderGenerator <- evBinders stmts
            return $! do
                sets <- setGenerator
                binderGenerator sets
        else
            evStmts stmts

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
    ([a] -> EvaluationMonad Value -> EvaluationMonad Value)
maybeTSTOP =
    maybeTimed
        (return $ \ _ prog -> prog)
        (\ tn _ -> return $ \ xs p1 ->
            case xs of 
                [] -> tSTOP tn
                _ -> p1)

tock :: Name -> Event
tock tn = UserEvent (tockValue tn)

tockValue :: Name -> Value
tockValue tn = VDot [VChannel tn]

tockSet :: Name -> EventSet
tockSet tn = [tock tn]

makeTocker :: Name -> Int -> Proc -> Proc
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
        -- We must be inside a polymorphic function, so cannot be sure what type this will be. Hence, it is unsafe
        -- to assume it is not dotable. Thus, we assume the worst.
        isDotable (TVar _) = True
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
