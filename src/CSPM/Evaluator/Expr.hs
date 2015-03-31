{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CSPM.Evaluator.Expr (
    eval,
) where

import qualified Data.Foldable as F
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence ((<|))
import qualified Data.Sequence as Sq

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.DeclBind
import CSPM.Evaluator.Dot
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import Util.Annotated
import Util.Exception
import Util.List

-- In order to keep lazy evaluation working properly only use pattern
-- matching when you HAVE to know the value. (Hence why we delay pattern
-- matching in BooleanBinaryOp And in case the first value is false.)

eval :: TCExp -> EvaluationMonad Value
eval (An loc _ a) = setCurrentExpressionLocation loc (evalExpr a)

evalExpr :: Exp Name -> EvaluationMonad Value
evalExpr (App func args) = do
    vs <- mapM eval args
    VFunction _ f <- eval func
    f vs
evalExpr (BooleanBinaryOp op e1 e2) = do
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
evalExpr (BooleanUnaryOp op e) = do
    VBool b <- eval e
    case op of
        Not -> return $ VBool (not b)
evalExpr (Concat e1 e2) = do
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
evalExpr (DotApp e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        combineDots v1 v2
evalExpr (If e1 e2 e3) = do
    VBool b <- eval e1
    if b then eval e2 else eval e3
evalExpr (Lambda ps e) = do
    st <- getState
    psid <- getParentScopeIdentifier
    let fid = lambdaFunction (Lambda ps e) psid
    return $ VFunction fid $ \ vs -> return $ runEvaluator st $ do
        let (matches, binds) = bindAll ps vs
        if matches then do
            p <- getParentScopeIdentifier
            updateParentScopeIdentifier (annonymousScopeId vs psid) $ 
                addScopeAndBind binds (eval e)
        else do
            loc <- getCurrentExpressionLocation
            throwError $ patternMatchesFailureMessage loc ps vs
evalExpr (Let decls e) = do
    nvs <- bindDecls decls
    addScopeAndBindM nvs (eval e)
evalExpr (Lit (Int i)) = return $! VInt i
evalExpr (Lit (Bool b)) = return $! VBool b
evalExpr (Lit (Char c)) = return $! VChar c
evalExpr (Lit (String s)) =
    let
        cs = map VChar s
    in return $! VList cs
evalExpr (List es) = mapM eval es >>= return . VList
evalExpr (ListComp es stmts) = do
        xs <- evalStmts (\(VList xs) -> xs) stmts (mapM eval es)
        return $ VList xs
evalExpr (ListEnumFrom e) = do
    VInt lb <- eval e
    return $ VList (map VInt [lb..])
evalExpr (ListEnumFromTo e1 e2) = do
    VInt lb <- eval e1
    VInt ub <- eval e2
    return $ VList (map VInt [lb..ub])
evalExpr (ListEnumFromComp e1 stmts) = do
    ss <- evalStmts (\ (VList xs) -> xs) stmts $ do
            VInt lb <- eval e1
            return $ map VInt [lb..]
    return $ VList ss
evalExpr (ListEnumFromToComp e1 e2 stmts) = do
    ss <- evalStmts (\ (VList xs) -> xs) stmts $ do
            VInt lb <- eval e1
            VInt ub <- eval e2
            return $ map VInt [lb..ub]
    return $ VList ss
evalExpr (ListLength e) = do
    VList xs <- eval e 
    return $ VInt (length xs)
evalExpr (Map kvs) = do
    xs <- mapM (\ (k, v) -> do
        k <- eval k
        v <- eval v
        return (k, v)) kvs
    return $ VMap $ M.fromList xs
evalExpr (MathsBinaryOp op e1 e2) = do
    VInt i1 <- eval e1
    VInt i2 <- eval e2
    case op of
        Divide -> do
            scopeId <- getParentScopeIdentifier
            loc <- getCurrentExpressionLocation
            return $ VInt $
                    case i2 of
                        0 -> throwError $ divideByZeroMessage loc scopeId
                        _ -> i1 `div` i2
        Minus -> return $ VInt (i1 - i2)
        Mod -> return $ VInt (i1 `mod` i2)
        Plus -> return $ VInt (i1 + i2)
        Times -> return $ VInt (i1 * i2)
evalExpr (MathsUnaryOp op e) = do
    VInt i <- eval e
    case op of
        Negate -> return $ VInt (-i)
evalExpr (Paren e) = eval e
evalExpr (Set es) = mapM eval es >>= return . VSet . S.fromList
evalExpr (SetComp es stmts) = do
    xs <- evalStmts (\(VSet s) -> S.toList s) stmts (mapM eval es)
    return $ VSet (S.fromList xs)
evalExpr (SetEnum es) = do
    evs <- mapM eval es
    ss <- mapM productionsSet evs
    return $ VSet (S.unions ss)
evalExpr (SetEnumComp es stmts) = do
    ss <- evalStmts (\(VSet s) -> S.toList s) stmts 
                    (mapM (\e -> eval e >>= productionsSet) es)
    return $ VSet (S.unions ss)
evalExpr (SetEnumFrom e) = do
    VInt lb <- eval e
    return $ VSet (S.IntSetFrom lb)
evalExpr (SetEnumFromTo e1 e2) = do
    VInt lb <- eval e1
    VInt ub <- eval e2
    return $ VSet (S.fromList (map VInt [lb..ub]))
evalExpr (SetEnumFromComp e1 stmts) = do
    ss <- evalStmts (\ (VSet s) -> S.toList s) stmts $ do
            VInt lb <- eval e1
            return [S.IntSetFrom lb]
    return $ VSet $ S.unions ss
evalExpr (SetEnumFromToComp e1 e2 stmts) = do
    ss <- evalStmts (\ (VSet s) -> S.toList s) stmts $ do
            VInt lb <- eval e1
            VInt ub <- eval e2
            return [S.fromList (map VInt [lb..ub])]
    return $ VSet $ S.unions ss
evalExpr (Tuple es) = mapM eval es >>= return . tupleFromList
evalExpr (Var n) | isNameDataConstructor n = do
    (dc, _, _) <- dataTypeInfo n
    return dc
evalExpr (Var n) = lookupVar n

-- This is the most complicated process because it is actually a shorthand
-- for external choice and internal choice.
evalExpr (Prefix e1 fs e2) =
    let
        evalInputField :: Value -> [Field Name] -> TCPat -> S.ValueSet -> 
            (Value -> [Field Name] -> EvaluationMonad UProc) ->
            EvaluationMonad (Sq.Seq UProc)
        evalInputField evBase fs p s evalRest = do
            mps <- mapM (\v -> do
                let (matches, binds) = bind p v
                if matches then do
                    ev' <- combineDots evBase v
                    pid <- getParentScopeIdentifier
                    p <- updateParentScopeIdentifier (annonymousScopeId [v] pid) $
                            addScopeAndBind binds $ evalRest ev' fs
                    return $ Just p
                else return Nothing) (S.toList s)
            return $ Sq.fromList $ catMaybes mps
        
        -- | Evalutates an input field, deducing the correct set of values
        -- to input over.
        evalInputField2 :: Value -> [Field Name] -> Pat Name -> 
            (Value -> [Field Name] -> EvaluationMonad UProc) ->
            (Sq.Seq UProc -> UProc) -> EvaluationMonad UProc
        evalInputField2 evBase fs p evalRest procConstructor = 
            let
                -- | The function to use to generate the options. If this
                -- is the last field AND the last pattern in the current
                -- field it uses 'extensions' to extend to a fully formed 
                -- event, otherwise we use 'oneFieldExtensions' to extend 
                -- by precisely one field.
                extensionsOperator :: 
                    [Pat Name] -> Value -> EvaluationMonad [Value]
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
                evExtensions :: Value -> [Pat Name] -> [(Name, Value)] -> 
                    EvaluationMonad (Sq.Seq UProc)
                evExtensions evBase [] bs = do
                    pid <- getParentScopeIdentifier
                    p <- updateParentScopeIdentifier (annonymousScopeId (map snd bs) pid) $
                            addScopeAndBind bs $ evalRest evBase fs
                    return $ Sq.singleton p
                evExtensions evBase (PVar n:ps) bs | isNameDataConstructor n = do
                    (dc, _, _) <- dataTypeInfo n
                    evBase' <- combineDots evBase dc
                    evExtensions evBase' ps bs
                evExtensions evBase (p:ps) bs = do
                    vs <- extensionsOperator (p:ps) evBase
                    mps <- mapM (\v -> do
                            let (matches, bs') = bind p v
                            if matches then do
                                evBase' <- combineDots evBase v
                                proc <- evExtensions evBase' ps (bs++bs')
                                return $ Just proc
                            else return Nothing) vs
                    return $ F.msum $ catMaybes mps
            in do
                ps <- evExtensions evBase (patToFields p) []
                return $ procConstructor ps

        evalNonDetFields :: Value -> [Field Name] -> EvaluationMonad UProc
        evalNonDetFields evBase (NonDetInput p (Just e):fs) = do
            VSet s <- eval e
            ps <- evalInputField evBase fs p s evalNonDetFields
            if Sq.null ps then
                throwError' $ replicatedInternalChoiceOverEmptySetMessage (unAnnotate e)
            else return $ POp PInternalChoice ps
        evalNonDetFields evBase (NonDetInput p Nothing:fs) = do
            POp _ ps <- evalInputField2 evBase fs (unAnnotate p) evalNonDetFields (POp PInternalChoice)
            if Sq.null ps then
                throwError' $ replicatedInternalChoiceOverEmptySetMessage' (unAnnotate p)
            else return $ POp PInternalChoice ps
        evalNonDetFields evBase fs = evalFields evBase fs

        evalFields :: Value -> [Field Name] -> EvaluationMonad UProc
        evalFields ev [] = do
            p <- evalProc e2
            return $ PUnaryOp (PPrefix (valueEventToEvent ev)) p
        evalFields evBase (Output e:fs) = do
            v <- eval e
            ev' <- combineDots evBase v
            evalFields ev' fs
        evalFields evBase (Input p (Just e):fs) = do
            VSet s <- eval e
            ps <- evalInputField evBase fs p s evalFields
            return $ POp PExternalChoice ps
        evalFields evBase (Input p Nothing:fs) =
            evalInputField2 evBase fs (unAnnotate p) evalFields (POp PExternalChoice)
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
        p <- evalNonDetFields ev (map unAnnotate fs)
        return $ VProc p --(simplify p)

evalExpr (TimedPrefix n e) = do
    -- Evaluate the prefix process
    p <- evalProc e
    Just (eventFunc, tockName) <- gets timedSection
    parentScope <- getParentScopeIdentifier
    let addTocker (POp PExternalChoice ps) =
            POp PExternalChoice (fmap addTocker ps)
        addTocker (PUnaryOp (PPrefix ev) p) =
            PUnaryOp (PPrefix ev) (makeTocker tockName (eventFunc ev) p)
        p' = addTocker p
        tocker = PUnaryOp (PPrefix (tock tockName)) procCall
        mainProc = POp PExternalChoice (tocker <| p' <| Sq.empty)
        procCall = PProcCall (procName $ scopeId n [] parentScope) mainProc
    return $ VProc procCall

evalExpr (AlphaParallel e1 e2 e3 e4) = do
    p1 <- evalProc e1
    p2 <- evalProc e4
    VSet a1 <- timedCSPSyncSet $ eval e2
    VSet a2 <- timedCSPSyncSet $ eval e3
    return $ VProc $ POp (PAlphaParallel 
            (S.valueSetToEventSet a1 <| S.valueSetToEventSet a2 <| Sq.empty))
            (p1 <| p2 <| Sq.empty)
evalExpr (Exception e1 e2 e3) = do
    p1 <- evalProc e1
    VSet a <- eval e2
    p2 <- evalProc e3
    return $ VProc $ PBinaryOp (PException (S.valueSetToEventSet a)) p1 p2
evalExpr (ExternalChoice e1 e2) = do
    p1 <- evalProc e1
    p2 <- evalProc e2
    let ps = (p1 <| p2 <| Sq.empty)
    maybeTimedCSP
        (return $ VProc $ POp PExternalChoice ps)
        (\ tn _ -> return $ VProc $
            POp (PSynchronisingExternalChoice (tockSet tn)) ps)
evalExpr (GenParallel e1 e2 e3) = do
    ps <- evalProcs [e1, e3]
    VSet a <- timedCSPSyncSet $ eval e2
    return $ VProc $ POp (PGenParallel (S.valueSetToEventSet a)) ps
evalExpr (GuardedExp guard proc) = do
    VBool b <- eval guard
    if b then eval proc
    else maybeTimedCSP
            (lookupVar (builtInName "STOP"))
            (\ _ _ -> do
                VFunction _ tstop <- lookupVar (builtInName "TSTOP")
                tstop [])
evalExpr (Hiding e1 e2) = do
    p <- evalProc e1
    VSet s <- eval e2
    if S.empty s then return $ VProc p
    else return $ VProc $ PUnaryOp (PHide (S.valueSetToEventSet s)) p
evalExpr (InternalChoice e1 e2) = do
    ps <- evalProcs [e1, e2]
    return $ VProc $ POp PInternalChoice ps
evalExpr (Interrupt e1 e2) = do
    p1 <- evalProc e1
    p2 <- evalProc e2
    maybeTimedCSP
        (return $ VProc $ PBinaryOp PInterrupt p1 p2)
        (\ tn _ -> return $ VProc $
            PBinaryOp (PSynchronisingInterrupt (tockSet tn)) p1 p2)
evalExpr (Interleave e1 e2) = do
    ps <- evalProcs [e1, e2]
    maybeTimedCSP
        (return $ VProc $ POp PInterleave ps)
        (\ tn _ -> return $ VProc $ POp (PGenParallel (tockSet tn)) ps)
evalExpr (LinkParallel e1 ties stmts e2) = do
    p1 <- evalProc e1
    p2 <- evalProc e2
    ts <- evalTies stmts ties
    return $ VProc $
        PBinaryOp (PLinkParallel (removeDuplicateTies ts)) p1 p2
evalExpr (Project e1 e2) = do
    p <- evalProc e1
    VSet s <- eval e2
    return $ VProc $ PUnaryOp (PProject (S.valueSetToEventSet s)) p
evalExpr (Rename e1 ties stmts) = do
    p1 <- evalProc e1
    ts <- evalTies stmts ties
    return $ VProc $ if Sq.null ts then p1 else
        PUnaryOp (PRename (removeDuplicateTies ts)) p1
evalExpr (SequentialComp e1 e2) = do
    p1 <- evalProc e1
    p2 <- evalProc e2
    return $ VProc $ PBinaryOp PSequentialComp p1 p2
evalExpr (SlidingChoice e1 e2) = do
    p1 <- evalProc e1
    p2 <- evalProc e2
    return $ VProc $ PBinaryOp PSlidingChoice p1 p2
evalExpr (SynchronisingExternalChoice e1 e2 e3) = do
    ps <- evalProcs [e1, e3]
    VSet a <- timedCSPSyncSet $ eval e2
    return $ VProc $ POp
        (PSynchronisingExternalChoice (S.valueSetToEventSet a)) ps
evalExpr (SynchronisingInterrupt e1 e2 e3) = do
    p1 <- evalProc e1
    VSet a <- timedCSPSyncSet $ eval e2
    p2 <- evalProc e3
    return $ VProc $
        PBinaryOp (PSynchronisingInterrupt (S.valueSetToEventSet a)) p1 p2

evalExpr (ReplicatedAlphaParallel stmts e1 e2) = do
    aps <- evalStmts' (\(VSet s) -> S.toSeq s) stmts $ do
        VSet s <- timedCSPSyncSet $ eval e1
        p <- evalProc e2
        return (S.valueSetToEventSet s, p)
    let (as, ps) = unzipSq aps
    maybeTSTOP ps $ return $ VProc $ POp (PAlphaParallel as) ps
evalExpr (ReplicatedExternalChoice stmts e) = do
    ps <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e)
    maybeTimedCSP
        (return $ VProc $ POp PExternalChoice ps)
        (\ tn _ -> maybeTSTOP ps $ return $ VProc $
            POp (PSynchronisingExternalChoice (tockSet tn)) ps)
evalExpr (ReplicatedInterleave stmts e) = do
    ps <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e)
    maybeTimedCSP
        (return $ VProc $ POp PInterleave ps)
        (\ tn _ -> maybeTSTOP ps $
            return $ VProc $ POp (PGenParallel (tockSet tn)) ps)
evalExpr (e'@(ReplicatedInternalChoice stmts e)) = do
    ps <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e)
    if Sq.null ps then
        throwError' $ replicatedInternalChoiceOverEmptySetMessage e'
    else return $ VProc $ POp PInternalChoice ps
evalExpr (e'@(ReplicatedLinkParallel ties tiesStmts stmts e)) = do
    tsps <- evalStmts' (\(VList vs) -> Sq.fromList vs) stmts $ do
        ts <- evalTies tiesStmts ties
        p <- evalProc e
        return (ts, p)
    if Sq.null tsps then
        throwError' $ replicatedLinkParallelOverEmptySeqMessage e'
    else do
    let
        (tsps' Sq.:> (_, lastProc)) = Sq.viewr tsps
        mkLinkPar (ts, p1) p2 =
            PBinaryOp (PLinkParallel (removeDuplicateTies ts)) p1 p2
    return $ VProc $ F.foldr mkLinkPar lastProc tsps'
evalExpr (ReplicatedParallel e1 stmts e2) = do
    VSet s <- timedCSPSyncSet $ eval e1
    ps <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e2)
    maybeTSTOP ps $ 
        return $ VProc $ POp (PGenParallel (S.valueSetToEventSet s)) ps
evalExpr (ReplicatedSequentialComp stmts e) = do
    ps <- evalStmts' (\(VList vs) -> Sq.fromList vs) stmts (evalProc e)
    if Sq.null ps then
        maybeTimedCSP
            (lookupVar (builtInName "SKIP"))
            (\ _ _ -> do
                VFunction _ tskip <- lookupVar (builtInName "TSKIP")
                tskip [])
    else return $ VProc $ F.foldr1 (PBinaryOp PSequentialComp) ps
evalExpr (ReplicatedSynchronisingExternalChoice e1 stmts e2) = do
    VSet a <- timedCSPSyncSet $ eval e1
    ps <- evalStmts' (\(VSet s) -> S.toSeq s) stmts (evalProc e2)
    maybeTSTOP ps $ return $ VProc $ POp
        (PSynchronisingExternalChoice (S.valueSetToEventSet a)) ps

evalExpr e = panic ("No clause to eval "++show e)

evalProcs :: [TCExp] -> EvaluationMonad (Sq.Seq UProc)
evalProcs as = mapM evalProc as >>= return . Sq.fromList

evalProc :: TCExp -> EvaluationMonad UProc
evalProc a = eval a >>= \v -> case v of
    VProc x -> return x
    _       -> panic "Type checker error"

removeDuplicateTies :: Sq.Seq (Event, Event) -> Sq.Seq (Event, Event)
removeDuplicateTies = Sq.fromList . sortedNub . F.toList . Sq.unstableSort

evalTies :: [TCStmt] -> [(TCExp, TCExp)] -> EvaluationMonad (Sq.Seq (Event, Event))
evalTies stmts ties = do
    tss <- evalStmts (\(VSet s) -> S.toList s) stmts (mapM evalTie ties)
    return $ Sq.fromList (concat tss)
    where
        extendTie :: (Value, Value) -> Value -> EvaluationMonad (Event, Event)
        extendTie (evOld, evNew) ex = do
            ev1 <- combineDots evOld ex
            ev2 <- combineDots evNew ex
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
                if matches then do
                    pid <- getParentScopeIdentifier
                    updateParentScopeIdentifier (annonymousScopeId [v] pid) $
                        addScopeAndBind binds (evStmts stmts)
                else return []) (extract v)
            return $ concat vss
    in
        evStmts (map unAnnotate anStmts)

-- | Evaluates the statements, evaluating `prog` for each possible 
-- assingment to the generators that satisfies the qualifiers.
evalStmts' :: (Value -> Sq.Seq Value) -> [TCStmt] -> EvaluationMonad a -> 
            EvaluationMonad (Sq.Seq a)
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
                    pid <- getParentScopeIdentifier
                    s' <- updateParentScopeIdentifier (annonymousScopeId [v] pid) $
                                addScopeAndBind binds (evStmts stmts)
                    return $ s Sq.>< s'
                else return s) Sq.empty (extract v)
    in evStmts (map unAnnotate anStmts)

unzipSq :: Sq.Seq (a,b) -> (Sq.Seq a, Sq.Seq b)
unzipSq sqs = F.foldr 
    (\ (a,b) (as, bs) -> (a <| as, b <| bs) ) (Sq.empty, Sq.empty) sqs

timedCSPSyncSet :: EvaluationMonad Value -> EvaluationMonad Value
timedCSPSyncSet prog = do
    VSet a <- prog
    maybeTimedCSP
        (return $ VSet a)
        (\ tn _ -> return $ VSet (S.union (S.fromList [tockValue tn]) a))

tSTOP :: EvaluationMonad Value
tSTOP = do
    VFunction _ tstop <- lookupVar (builtInName "TSTOP")
    tstop []

maybeTSTOP :: Sq.Seq a -> EvaluationMonad Value -> EvaluationMonad Value
maybeTSTOP sq p1 =
    maybeTimedCSP p1 (\ _ _ -> if Sq.null sq then tSTOP else p1)

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
