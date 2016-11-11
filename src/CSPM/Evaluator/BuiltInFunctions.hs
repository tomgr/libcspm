module CSPM.Evaluator.BuiltInFunctions (
    injectBuiltInFunctions,
    builtInName
) where

import Data.Array
import Data.List
import Control.Monad.ST
import qualified Data.ByteString.Char8 as B
import Data.Hashable
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as B
import qualified Data.Set as St
import qualified Data.Map as M

import CSPM.Syntax.Names
import CSPM.Evaluator.AnalyserMonad
import CSPM.Evaluator.Dot
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Profiler
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import CSPM.Prelude
import qualified Data.Graph.ST as G
import Util.Annotated
import Util.Exception
import Util.List
import Util.Prelude
import Util.PrettyPrint

builtinProcName :: FrameInformation -> [[Value]] -> ProcName
builtinProcName f vss = procName (instantiateBuiltinFrameWithArguments f vss)

builtInFunctions :: AnalyserMonad (EvaluationMonad [(Name, Value)])
builtInFunctions = do
    builtinFrames <- mapM (createBuiltinFunctionFrame . name) (builtins True)

    let
        frameMap = M.fromList $ map
                        (\ f -> (builtinFunctionFrameFunctionName f, f))
                        builtinFrames
        frameForBuiltin :: B.ByteString -> FrameInformation
        frameForBuiltin s =
            case M.lookup (builtInName s) frameMap of
                Nothing -> panic "Could not find builtin"
                Just n -> n

        cspm_union [VSet s1, VSet s2] = S.union s1 s2
        cspm_inter [VSet s1, VSet s2] = S.intersection s1 s2
        cspm_diff [VSet s1, VSet s2] = S.difference s1 s2
        cspm_Union [VSet s] = S.unions (map (\ (VSet s) -> s) (S.toList s))
        cspm_Inter [VSet s] = 
            S.intersections (map (\ (VSet s) -> s) (S.toList s))
        cspm_member [v, VSet s] = makeBoolValue $ S.member v s
        cspm_card [VSet s] = VInt $ fromIntegral $ S.card s
        cspm_empty [VSet s] = makeBoolValue $ S.empty s
        cspm_set [VList xs] = S.fromList xs
        cspm_Set [VSet s] = S.powerset s
        -- | Set of all sequences over s
        cspm_Seq [VSet s] = S.allSequences s
        cspm_seq [VSet s] = S.toList s
        cspm_transpose [VSet s] = VSet $ 
            S.fromList [VTuple (listArray (0,1) [arr!1, arr!0]) | VTuple arr <- S.toList s]
        cspm_relational_image =
            let frameInfo = frameForBuiltin "relational_image"
            in \ [VSet s] ->
                let f = relationalImage s 
                    fid = instantiateBuiltinFrameWithArguments frameInfo [[VSet s]]
                in VFunction fid (\[x] -> f x >>= return . VSet)
        cspm_mtransclose [VSet s1, VSet s2] = fdrSymmetricTransitiveClosure s1 s2
        cspm_relational_inverse_image s = cspm_relational_image [cspm_transpose s]
        cspm_show [v] =
            VList (map VChar (show (prettyPrint v)))
        cspm_error loc [err] =
            throwError' $ \ _ -> mkErrorMessage loc $
                text "Error:" <+> prettyPrint err

        cspm_mapFromList [VList s] = VMap $
            M.fromList [(arr!0, arr!1) | VTuple arr <- s]
        cspm_mapLookup loc [VMap m, k] =
            case M.lookup k m of
                Just v -> return v
                Nothing -> throwError' $ keyNotInDomainOfMapMessage loc
        cspm_mapMember [VMap m, k] =
            case M.lookup k m of
                Just v -> trueValue
                Nothing -> falseValue
        cspm_mapToList [VMap m] = VList $
            [VTuple (listArray (0,1) [k, v]) | (k, v) <- M.toList m]
        cspm_mapUpdate [VMap m, k, v] = VMap $ M.insert k v m
        cspm_mapUpdateMultiple [VMap m, VList s] = VMap $
            foldr (uncurry M.insert) m [(arr!0, arr!1) | VTuple arr <- s]
        cspm_mapDelete [VMap m, k] = VMap $ M.delete k m
        cspm_Map [VSet k, VSet v] = S.allMaps k v

        map_funcs = [
                ("mapFromList", cspm_mapFromList),
                ("mapMember", cspm_mapMember),
                ("mapToList", cspm_mapToList),
                ("mapUpdate", cspm_mapUpdate),
                ("mapUpdateMultiple", cspm_mapUpdateMultiple),
                ("mapDelete", cspm_mapDelete)
            ]
        
        cspm_length [VList xs] = VInt $ length xs
        cspm_null [VList xs] = makeBoolValue $ null xs
        cspm_head loc [VList []] = throwError' $ headEmptyListMessage loc
        cspm_head _ [VList (x:xs)] = return x
        cspm_tail loc [VList []] = throwError' $ tailEmptyListMessage loc
        cspm_tail _ [VList (x:xs)] = return $ VList xs
        cspm_concat [VList xs] = concat (map (\(VList ys) -> ys) xs)
        cspm_elem [v, VList vs] = makeBoolValue $ v `elem` vs
        csp_chaos_frame = frameForBuiltin "CHAOS"
        csp_chaos [VSet a] = VProc chaosCall
            where
                chaosCall = PProcCall n p
                -- | We convert the set into an explicit set as this makes
                -- comparisons faster than leaving it as a set represented as
                -- (for instance) a CompositeSet of CartProduct sets.
                n = builtinProcName csp_chaos_frame
                        [[VSet $ S.fromList $ S.toList a]]
                p = POp (PChaos (S.valueSetToEventSet a)) []
        csp_run_frame = frameForBuiltin "RUN"
        csp_run [VSet a] = VProc runCall
            where
                runCall = PProcCall n p
                -- | We convert the set into an explicit set as this makes
                -- comparisons faster than leaving it as a set represented as
                -- (for instance) a CompositeSet of CartProduct sets.
                n = builtinProcName csp_run_frame
                        [[VSet $ S.fromList $ S.toList a]]
                p = POp (PRun (S.valueSetToEventSet a)) []
        csp_loop_frame = frameForBuiltin "loop"
        csp_loop [VProc p] =
            let pn = builtinProcName csp_loop_frame [[VProc p]]
                procCall = PProcCall pn (PBinaryOp PSequentialComp p procCall)
            in VProc procCall
    
        checkBufferBound loc cap p | cap <= 0 = throwError $ bufferCapacityInsufficient cap loc Nothing
        checkBufferBound _ _ p = p
            
        checkForAmbiguousBufferEvents loc evs p = 
            case firstDuplicate $ sort evs of
                Nothing -> return p
                Just ev -> throwError $ bufferEventAmbiguous (UserEvent ev) loc Nothing
        csp_refusing_buffer_frame = frameForBuiltin "BUFFER"
        csp_refusing_buffer loc [VInt bound, VSet pairs] =
                checkBufferBound loc bound $
                checkForAmbiguousBufferEvents loc (concat [[arr!0, arr!1] | VTuple arr <- S.toList pairs])
                    (VProc bufferCall)
            where
                bufferCall = PProcCall n p
                -- | We convert the set into an explicit set as this makes
                -- comparisons faster than leaving it as a set represented as
                -- (for instance) a CompositeSet of CartProduct sets.
                n = builtinProcName csp_refusing_buffer_frame [[VInt bound, VSet pairs]]
                events = [(UserEvent (arr!0), UserEvent (arr!1)) | VTuple arr <- S.toList pairs]
                p = POp (PBuffer BufferStandard bound events) []
        csp_exploding_buffer_frame = frameForBuiltin "WEAK_BUFFER"
        csp_exploding_buffer loc [VInt bound, explode, VSet pairs] =
                checkBufferBound loc bound $
                checkForAmbiguousBufferEvents loc (explode : concat [[arr!0, arr!1] | VTuple arr <- S.toList pairs])
                    (VProc bufferCall)
            where
                bufferCall = PProcCall n p
                -- | We convert the set into an explicit set as this makes
                -- comparisons faster than leaving it as a set represented as
                -- (for instance) a CompositeSet of CartProduct sets.
                n = builtinProcName csp_exploding_buffer_frame [[VInt bound, explode, VSet pairs]]
                events = [(UserEvent (arr!0), UserEvent (arr!1)) | VTuple arr <- S.toList pairs]
                p = POp (PBuffer (BufferSignalWhenFull (UserEvent explode)) bound events) []
                
        csp_signal_buffer_frame = frameForBuiltin "SIGNAL_BUFFER"
        csp_signal_buffer loc [VInt bound, explode, empty, VSet pairs] =
                checkBufferBound loc bound $
                checkForAmbiguousBufferEvents loc
                    (explode : empty : concat [[arr!0, arr!1] | VTuple arr <- S.toList pairs])
                    (VProc bufferCall)
            where
                bufferCall = PProcCall n p
                -- | We convert the set into an explicit set as this makes
                -- comparisons faster than leaving it as a set represented as
                -- (for instance) a CompositeSet of CartProduct sets.
                n = builtinProcName csp_exploding_buffer_frame [[VInt bound, explode, empty, VSet pairs]]
                events = [(UserEvent (arr!0), UserEvent (arr!1)) | VTuple arr <- S.toList pairs]
                bm = BufferAlwaysSignal { fullSignal = UserEvent explode, emptySignal = UserEvent empty }
                p = POp (PBuffer bm bound events) []

        cspm_extensions [v] = do
            exs <- extensions v
            return $ VSet $ S.fromList exs
        cspm_productions [v] = do
            exs <- productions v
            return $ VSet $ S.fromList exs

        csp_prioritise _ loc [_, VList []] =
            throwError' $ prioritiseEmptyListMessage loc
        csp_prioritise cache _ [VProc p, VList alphas] =
            let sets = map (\ (VSet s) -> S.valueSetToEventSet s) alphas
                pop = Prioritise cache sets
            in return $ VProc $ PUnaryOp (POperator pop) p
        csp_prioritise_partialorder loc [VProc p, VSet prioritisedEvents,
                VSet order, VSet maximal] = do
            let orderList = [(UserEvent (t!0), UserEvent (t!1)) | VTuple t <- S.toList order]
            order <- computePrioritisePartialOrder loc orderList
                        [UserEvent ev | ev <- S.toList maximal]
                        [UserEvent ev | ev <- S.toList prioritisedEvents]
            let pop = PartialOrderPrioritise order
            return $ VProc $ PUnaryOp (POperator pop) p
        csp_timed_priority [tockVal] =
            let 
                frameInfo = frameForBuiltin "timed_priority"
                prioritiseId = instantiateBuiltinFrameWithArguments frameInfo [[tockVal]]
                prioritiser [VProc p] = 
                    return $ VProc $ PUnaryOp (POperator pop) p
                    where pop = Prioritise True [[], [UserEvent tockVal]]
            in VFunction prioritiseId prioritiser

        csp_failure_watchdog [VProc p, VSet implementationEvents, ev] =
            VProc $ PUnaryOp (POperator (FailureWatchdog
                (S.valueSetToEventSet implementationEvents)
                (UserEvent ev))) p
        csp_trace_watchdog [VProc p, VSet implementationEvents, ev] =
            VProc $ PUnaryOp (POperator (TraceWatchdog
                (S.valueSetToEventSet implementationEvents)
                (UserEvent ev))) p

        -- | Functions that return sets
        set_funcs = [
            ("union", cspm_union), ("inter", cspm_inter), 
            ("diff", cspm_diff), ("Union", cspm_Union), 
            ("Inter", cspm_Inter), ("set", cspm_set), 
            ("Set", cspm_Set), ("Seq", cspm_Seq),
            ("Map", cspm_Map), ("mtransclose", cspm_mtransclose)
            ]
        
        -- | Functions that return sequences
        seq_funcs = [
            ("seq", cspm_seq), ("concat", cspm_concat)
            ]
        
        -- | Functions that mutate processes (like compression functions).
        proc_operators = [
            ("chase", Chase True),
            ("chase_nocache", Chase False),
            ("dbisim", DelayBisim),
            ("deter", Determinise),
            ("diamond", Diamond),
            ("explicate", Explicate False),
            ("lazyenumerate", Explicate True),
            ("normal", Normalize False),
            ("lazynorm", Normalize True),
            ("model_compress", ModelCompress),
            ("sbisim", StrongBisim),
            ("tau_loop_factor", TauLoopFactor),
            ("wbisim", WeakBisim)
            ]
        
        -- | Functions that return something else
        other_funcs = [
            ("length", cspm_length), ("null", cspm_null), 
            ("elem", cspm_elem),
            ("member", cspm_member), ("card", cspm_card),
            ("empty", cspm_empty), ("CHAOS", csp_chaos),
            ("RUN", csp_run),
            ("loop", csp_loop), ("relational_image", cspm_relational_image),
            ("relational_inverse_image", cspm_relational_inverse_image),
            ("transpose", cspm_transpose), ("show", cspm_show),
            ("failure_watchdog", csp_failure_watchdog),
            ("trace_watchdog", csp_trace_watchdog),
            ("TSTOP", csp_tstop), ("TSKIP", csp_tskip),
            ("WAIT", csp_wait), ("timed_priority", csp_timed_priority)
            ]

        -- | Functions that require a monadic context.
        monadic_funcs = [
            ("productions", cspm_productions), ("extensions", cspm_extensions)
            ]

        locatedFunctions = [
            ("head", cspm_head), ("tail", cspm_tail),
            ("error", cspm_error),
            ("prioritise", csp_prioritise True),
            ("prioritise_nocache", csp_prioritise False),
            ("prioritisepo", csp_prioritise_partialorder),
            ("mapLookup", cspm_mapLookup),
            ("WEAK_BUFFER", csp_exploding_buffer), ("BUFFER", csp_refusing_buffer),
            ("SIGNAL_BUFFER", csp_signal_buffer)
            ]

        procs = [
            ("STOP", csp_stop),
            ("SKIP", csp_skip),
            ("DIV", csp_div)
            ]
        
        csp_skip_id = builtinProcName (frameForBuiltin "SKIP") []
        csp_stop_id = builtinProcName (frameForBuiltin "STOP") []
        csp_div_id = builtinProcName (frameForBuiltin "DIV") []
        csp_stop = PProcCall csp_stop_id (POp PExternalChoice [])
        csp_skip =
            PProcCall csp_skip_id (PUnaryOp (PPrefix Tick) csp_stop)
        csp_div = PProcCall csp_div_id (POp PInternalChoice [csp_div])

        csp_tstop [tockVal@(VDot [VChannel tn])] =
            let
                pid = builtinProcName (frameForBuiltin "STOP") [[VChannel tn]]
                proc = PUnaryOp (PPrefix (UserEvent tockVal)) pc
                pc = PProcCall pid proc
            in VProc pc

        csp_tskip [tockVal] =
            let
                VDot [VChannel tn] = tockVal
                pid = builtinProcName (frameForBuiltin "SKIP")  [[VChannel tn]]
                proc = PUnaryOp (PPrefix (UserEvent tockVal)) pc
                pc = PProcCall pid $ POp PExternalChoice [proc, csp_skip]
            in VProc pc

        csp_wait_frame = frameForBuiltin "WAIT"
        csp_wait [tockVal] =
            let
                VDot [VChannel tn] = tockVal
                VProc tskip = csp_tskip [tockVal]

                mkTocker 0 = tskip
                mkTocker n =
                    PUnaryOp (PPrefix (UserEvent tockVal)) (mkTocker (n-1))

                waiterId = instantiateBuiltinFrameWithArguments csp_wait_frame
                                [[tockVal]]
                waiter [VInt tocks] =
                    let
                        pid = builtinProcName csp_wait_frame [[tockVal], [VInt tocks]]
                    in VProc $ PProcCall pid $ mkTocker tocks
            in VFunction waiterId (return . waiter)

        mkProc (s, p) = return (builtInName s, VProc p)
        
        cspm_true = ("true", trueValue)
        cspm_false = ("false", falseValue)
        cspm_True = ("True", trueValue)
        cspm_False = ("False", falseValue)
        
        cspm_Bool = ("Bool", VSet (S.fromList [snd cspm_true, snd cspm_false]))
        cspm_Int = ("Int", VSet S.Integers)
        cspm_Char = ("Char", VSet (S.fromList (map VChar [minBound :: Char ..])))
        cspm_Proc = ("Proc", VSet S.Processes)
        cspm_Events = ("Events", VSet (S.fromList []))
        
        cspm_emptyMap = ("emptyMap", VMap M.empty)

        constants = [
            cspm_true, cspm_false, cspm_True, cspm_False,
            cspm_Bool, cspm_Int, cspm_Proc, cspm_Events, cspm_Char,
            cspm_emptyMap
            ]
        
        mkConstant (s, v) = return (builtInName s, v)

        mkLocatedFunction (s, f) = do
            let n = builtInName s
                frameInfo = frameForBuiltin s
                outerFid = instantiateBuiltinFrameWithArguments frameInfo []
                innerFid loc = instantiateBuiltinFrameWithArguments frameInfo [[VLoc loc]]
            innerFn <- profile frameInfo $ \ loc args -> f loc args
            let outerFn [VLoc loc] = return $ VFunction (innerFid loc) $ innerFn loc
            return $! (n, VFunction outerFid outerFn)

        mkFunc (s, f) = mkMonadicFunc (s, \vs -> return $ f vs)

        mkMonadicFunc :: (B.ByteString, [Value] -> EvaluationMonad Value) ->
            AnalyserMonad (Name, Value)
        mkMonadicFunc (s, f) = do
            let n = builtInName s
                frameInfo = frameForBuiltin s
                fid = instantiateBuiltinFrameWithArguments frameInfo []
            fn <- profile frameInfo $ \ args -> f args
            return $! (n, VFunction fid fn)

        evaluateProcOperator pop [p] = VProc $ 
            -- We defer matching of the arguments here to allow definitions like
            -- P = normal(P) to be evaluated. This allows the compiler to
            -- produce a more sensible error message.
            case p of
                VProc p -> PUnaryOp (POperator pop) p

    fs1 <- mapM mkFunc (
            map (\ (n, f) -> (n, VSet . f)) set_funcs
            ++ map (\ (n, f) -> (n, VList . f)) seq_funcs
            ++ map (\ (n, po) -> (n, evaluateProcOperator po)) proc_operators
            ++ other_funcs ++ map_funcs)
    fs2 <- mapM mkMonadicFunc monadic_funcs
    fs5 <- mapM mkLocatedFunction locatedFunctions
    return $! do
        -- There is no need to profile the builtin constants, as they are free
        fs3 <- mapM mkProc procs
        fs4 <- mapM mkConstant constants
        return $! fs1++fs2++fs3++fs4++fs5

injectBuiltInFunctions :: EvaluationMonad a -> AnalyserMonad (EvaluationMonad a)
injectBuiltInFunctions prog = do
    fs <- builtInFunctions
    return $! do
        funcs <- fs
        addScopeAndBind funcs prog

-- | Takes a set and returns a function from value to set of values that are
-- mapped to.
relationalImage :: S.ValueSet -> Value -> EvaluationMonad S.ValueSet
relationalImage s = 
    let tuples = [(hash (arr!0), hash(arr!1), arr!0, arr!1) | VTuple arr <- S.toList s]
        hashCmp (hx1,_,x1,_) (hx2,_,x2,_) = compare hx1 hx2 `thenCmp` compare x1 x2
        hashEq (hx1,_,x1,_) (hx2,_,x2,_) = hx1 == hx2 && x1 == x2
        sortedTuples = sortBy hashCmp tuples
        groupedTuples = groupBy hashEq sortedTuples
        mkResult xss = 
            let (hg, _, xg, _) = head xss
            -- Our keys are hashes and values as this is slightly faster
            in ((hg, xg), S.fromList (map (\(_,_,_,y) -> y) xss))
        m = M.fromList (map mkResult groupedTuples)
        lookup v = return (M.findWithDefault S.emptySet (hash v, v) m)
    in lookup

-- | Compute the 'min transitive closure' of a relation. FDR actually returns
-- something rather odd in this case. In particular, it takes a relation
-- (specified as a set of pairs), and a set. It then computes the symmetric,
-- transitive closure of the relation (note NOT reflexive). It returns a
-- relation where each element x of the second set is mapped to a representative
-- value of the second set (if one exists). As this is not reflexive it omits
-- all pairs of the form (x,x).
--
-- NOTE: FDR actually represents the second relation as (rep(x), x), for some
-- reason.
fdrSymmetricTransitiveClosure :: S.ValueSet -> S.ValueSet -> S.ValueSet
fdrSymmetricTransitiveClosure vs1 vs2 = 
    let computeRepresentatives = do
            let es = [(arr!0, arr!1) | VTuple arr <- S.toList vs1]
                esSym = es ++ [(a,b) | (b,a) <- es]
                nodes = map fst esSym
            g <- G.newGraph nodes esSym
            rs <- G.nonReflexiveRepresentativesForNodes g
            return $! [tupleFromList [a,b] | (b,a) <- rs, S.member a vs2, S.member b vs2]
    in S.fromList $ runST computeRepresentatives

computePrioritisePartialOrder :: SrcSpan -> [(Event, Event)] -> [Event] ->
    [Event] -> EvaluationMonad [(Event, Event)]
computePrioritisePartialOrder loc evs maxEvents prioritsedEvents =
    let
        maxEventSet = St.fromList maxEvents
        prioritsedEventsSet = St.fromList prioritsedEvents
        allEvents = sortedNub $ sort $ concat $ prioritsedEvents : maxEvents :
            [[ev1, ev2] | (ev1, ev2) <- evs]
        extraEdges =
            [(Tau, ev) | ev <- allEvents, not (St.member ev maxEventSet)]
            ++ [(Tick, ev) | ev <- allEvents, not (St.member ev maxEventSet)]
        allEdges = evs ++ extraEdges

        cyclicSccs graphSccs = filter isCyclicScc graphSccs
            where
                isCyclicScc (G.AcyclicSCC _) = False
                isCyclicScc (G.CyclicSCC _) = True

        transitiveClosureEdges = runST $ do
            graph <- G.newGraphNoDupeNodes (Tau : Tick : allEvents) allEdges
            sccs <- G.sccs graph
            case cyclicSccs sccs of
                [] -> do
                    -- The sccs are in reverse topological order (i.e. if an SCC x has
                    -- an edge to a SCC y), then x preceeds y. Thus, we reverse the
                    -- ordering. This means by the time we compute the edges for a node
                    -- x that has an edge to a node y, we have already computed the
                    -- closure of y.
                    let topSortedNodes = reverse [x | G.AcyclicSCC x <- sccs]

                    htable <- H.new :: ST s (B.HashTable s Event [Event])
                    edges <- mapM (\ n -> do
                        successors <- G.successorNodes graph n
                        newSuccessors <- mapM (\ s -> do
                                Just xs <- H.lookup htable s
                                return xs
                            ) successors
                        let xs = sortedNub $ sort $ successors ++ concat newSuccessors
                        H.insert htable n xs
                        return [(n, x) | x <- xs]
                        ) topSortedNodes
                    return $ Right $ concat edges
                G.CyclicSCC xs : _ ->
                    return $ Left xs

        edgeIsInValid (ev1, ev2) = St.member ev2 maxEventSet

        eventsMissingFromPrioritised = St.toList $
            St.difference (St.fromList allEvents) prioritsedEventsSet
    in
        case eventsMissingFromPrioritised of
            [] ->
                case transitiveClosureEdges of
                    Left cyclicScc ->
                        throwError' $ prioritisePartialOrderCyclicOrder cyclicScc loc
                    Right edges -> 
                        case filter edgeIsInValid edges of
                            [] -> return [(v1, v2) | (v1, v2) <- edges]
                            (_, e) : _ -> throwError' $ prioritiseNonMaximalElement e loc
            _ ->
                 throwError' $ prioritisePartialOrderEventsMissing
                    prioritsedEvents eventsMissingFromPrioritised loc
