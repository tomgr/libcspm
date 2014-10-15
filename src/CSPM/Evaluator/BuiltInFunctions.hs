module CSPM.Evaluator.BuiltInFunctions (
    injectBuiltInFunctions,
    builtInName
) where

import Data.Array
import Data.List
import Control.Monad
import Control.Monad.ST
import Data.Hashable
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as B
import qualified Data.Set as St
import qualified Data.Map as M
import qualified Data.Sequence as Sq

import CSPM.DataStructures.Names
import CSPM.Evaluator.Dot
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Profiler
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import CSPM.Prelude
import qualified Data.Graph.ST as G
import Util.Exception
import Util.Prelude
import Util.PrettyPrint

builtInFunctions :: EvaluationMonad [(Name, Value)]
builtInFunctions = do
    registerCall <- maybeRegisterCall
    let
        cspm_union [VSet s1, VSet s2] = S.union s1 s2
        cspm_inter [VSet s1, VSet s2] = S.intersection s1 s2
        cspm_diff [VSet s1, VSet s2] = S.difference s1 s2
        cspm_Union [VSet s] = S.unions (map (\ (VSet s) -> s) (S.toList s))
        cspm_Inter [VSet s] = 
            S.intersections (map (\ (VSet s) -> s) (S.toList s))
        cspm_member [v, VSet s] = VBool $ S.member v s
        cspm_card [VSet s] = VInt $ fromIntegral $ S.card s
        cspm_empty [VSet s] = VBool $ S.empty s
        cspm_set [VList xs] = S.fromList xs
        cspm_Set [VSet s] = S.powerset s
        -- | Set of all sequences over s
        cspm_Seq [VSet s] = S.allSequences s
        cspm_seq [VSet s] = S.toList s
        cspm_transpose [VSet s] = VSet $ 
            S.fromList [VTuple (listArray (0,1) [arr!1, arr!0]) | VTuple arr <- S.toList s]
        cspm_relational_image [VSet s] = 
            let f = relationalImage s 
                fid = builtInFunction (builtInName "relational_image") [VSet s]
            in VFunction fid (\[x] -> f x >>= return . VSet)
        cspm_mtransclose [VSet s1, VSet s2] = fdrSymmetricTransitiveClosure s1 s2
        cspm_relational_inverse_image s = cspm_relational_image [cspm_transpose s]
        cspm_show [v] =
            VList (map VChar (show (prettyPrint v)))
        cspm_error [err] = throwError' $ \ srcspan _ -> mkErrorMessage srcspan $
            text "Error:" <+> prettyPrint err

        cspm_mapFromList [VList s] = VMap $
            M.fromList [(arr!0, arr!1) | VTuple arr <- s]
        cspm_mapLookup [VMap m, k] =
            case M.lookup k m of
                Just v -> return v
                Nothing -> throwError' keyNotInDomainOfMapMessage
        cspm_mapMember [VMap m, k] =
            case M.lookup k m of
                Just v -> VBool True
                Nothing -> VBool False
        cspm_mapToList [VMap m] = VList $
            [VTuple (listArray (0,1) [k, v]) | (k, v) <- M.toList m]
        cspm_mapUpdate [VMap m, k, v] = VMap $ M.insert k v m
        cspm_mapUpdateMultiple [VMap m, VList s] = VMap $
            foldr (uncurry M.insert) m [(arr!0, arr!1) | VTuple arr <- s]
        cspm_Map [VSet k, VSet v] = S.allMaps k v

        map_funcs = [
                ("mapFromList", cspm_mapFromList),
                ("mapMember", cspm_mapMember),
                ("mapToList", cspm_mapToList),
                ("mapUpdate", cspm_mapUpdate),
                ("mapUpdateMultiple", cspm_mapUpdateMultiple)
            ]
        
        cspm_length [VList xs] = VInt $ length xs
        cspm_null [VList xs] = VBool $ null xs
        cspm_head [VList []] = throwError' headEmptyListMessage
        cspm_head [VList (x:xs)] = return x
        cspm_tail [VList []] = throwError' tailEmptyListMessage
        cspm_tail [VList (x:xs)] = return $ VList xs
        cspm_concat [VList xs] = concat (map (\(VList ys) -> ys) xs)
        cspm_elem [v, VList vs] = VBool $ v `elem` vs
        csp_chaos [VSet a] = VProc chaosCall
            where
                chaosCall = PProcCall n p
                -- | We convert the set into an explicit set as this makes
                -- comparisons faster than leaving it as a set represented as
                -- (for instance) a CompositeSet of CartProduct sets.
                n = procName $ scopeId (builtInName "CHAOS") [[VSet $ S.fromList $ S.toList a]] Nothing
                evSet = S.valueSetToEventSet a
                branches :: Sq.Seq UProc
                branches = fmap (\ ev -> PUnaryOp (PPrefix ev) chaosCall) evSet
                p = POp PInternalChoice $
                    Sq.fromList [csp_stop, POp PExternalChoice branches]
        csp_loop [VProc p] =
            let pn = procName $ scopeId (builtInName "loop") [[VProc p]] Nothing
                procCall = PProcCall pn (PBinaryOp PSequentialComp p procCall)
            in VProc procCall

        cspm_extensions [v] = do
            exs <- extensions v
            return $ VSet $ S.fromList exs
        cspm_productions [v] = do
            exs <- productions v
            return $ VSet $ S.fromList exs

        csp_prioritise _ [_, VList []] = throwError' prioritiseEmptyListMessage
        csp_prioritise cache [VProc p, VList alphas] =
            let sets = map (\ (VSet s) -> S.valueSetToEventSet s) alphas
                pop = Prioritise cache (Sq.fromList sets)
            in return $ VProc $ PUnaryOp (POperator pop) p
        csp_prioritise_partialorder [VProc p, VSet order, VSet maximal] = do
            let orderList = [(UserEvent (t!0), UserEvent (t!1)) | VTuple t <- S.toList order]
            order <- computePrioritisePartialOrder orderList
                        [UserEvent ev | ev <- S.toList maximal]
            let pop = PartialOrderPrioritise order
            return $ VProc $ PUnaryOp (POperator pop) p
        csp_timed_priority [VProc p] = do
            Just (_, tn) <- gets timedSection
            let tock = UserEvent $ VDot [VChannel tn]
                pop = Prioritise True $ Sq.fromList $
                        [Sq.empty, Sq.singleton tock]
            return $ VProc $ PUnaryOp (POperator pop) p

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
            ("loop", csp_loop), ("relational_image", cspm_relational_image),
            ("relational_inverse_image", cspm_relational_inverse_image),
            ("transpose", cspm_transpose), ("show", cspm_show),
            ("failure_watchdog", csp_failure_watchdog),
            ("trace_watchdog", csp_trace_watchdog)
            ]

        -- | Functions that require a monadic context.
        monadic_funcs = [
            ("head", cspm_head), ("tail", cspm_tail), 
            ("productions", cspm_productions), ("extensions", cspm_extensions),
            ("error", cspm_error), ("TSTOP", csp_tstop), ("TSKIP", csp_tskip),
            ("timed_priority", csp_timed_priority),
            ("prioritise", csp_prioritise True),
            ("prioritise_nocache", csp_prioritise False),
            ("prioritisepo", csp_prioritise_partialorder),
            ("WAIT", csp_wait), ("mapLookup", cspm_mapLookup)
            ]

        mkFunc (s, f) = mkMonadicFunc (s, \vs -> return $ f vs)
        mkMonadicFunc (s, f) = do
            let n = builtInName s
                f' vs = registerCall n (f vs)
            return $! (n, VFunction (builtInFunction n []) f')

        procs = [
            ("STOP", csp_stop),
            ("SKIP", csp_skip)
            ]
        
        csp_skip_id = scopeId (builtInName "SKIP") [] Nothing
        csp_stop_id = scopeId (builtInName "STOP") [] Nothing
        csp_stop =
            PProcCall (procName csp_stop_id) (POp PExternalChoice Sq.empty)
        csp_skip =
            PProcCall (procName csp_skip_id) (PUnaryOp (PPrefix Tick) csp_stop)

        csp_tstop [] = do
            Just (_, tn) <- gets timedSection
            let
                pid = procName (scopeId tn [] (Just csp_stop_id))
                proc = PUnaryOp (PPrefix (UserEvent $ VDot [VChannel tn])) pc
                pc = PProcCall pid proc
            return $ VProc pc

        csp_tskip [] = do
            Just (_, tn) <- gets timedSection
            let
                pid = procName (scopeId tn [] (Just csp_skip_id))
                proc = PUnaryOp (PPrefix (UserEvent $ VDot [VChannel tn])) pc
                pc = PProcCall pid $ POp PExternalChoice $
                    proc Sq.<| csp_skip Sq.<| Sq.empty
            return $ VProc pc

        csp_wait [VInt tocks] = do
            Just (_, tn) <- gets timedSection
            VProc tskip <- csp_tskip []
            let
                mkTocker 0 = tskip
                mkTocker n =
                    PUnaryOp (PPrefix (UserEvent $ VDot [VChannel tn]))
                        (mkTocker (n-1))
                waitId = scopeId (builtInName "WAIT") [[VInt tocks]] Nothing
                pid = procName (scopeId tn [] (Just waitId))
            return $ VProc $ PProcCall pid $ mkTocker tocks

        mkProc (s, p) = return (builtInName s, VProc p)
        
        cspm_true = ("true", VBool True)
        cspm_false = ("false", VBool False)
        cspm_True = ("True", VBool True)
        cspm_False = ("False", VBool False)
        
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
    fs3 <- mapM mkProc procs
    fs4 <- mapM mkConstant constants
    return $! fs1++fs2++fs3++fs4

injectBuiltInFunctions :: EvaluationMonad a -> EvaluationMonad a
injectBuiltInFunctions prog = do
    fs <- builtInFunctions
    addScopeAndBind fs prog

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

computePrioritisePartialOrder :: [(Event, Event)] -> [Event] ->
    EvaluationMonad (Sq.Seq (Event, Event))
computePrioritisePartialOrder evs maxEvents =
    let
        maxEventSet = St.fromList maxEvents
        allEvents = nub $ sort $ concat $ maxEvents : [[ev1, ev2] | (ev1, ev2) <- evs]
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
                        let xs = nub $ sort $ successors ++ concat newSuccessors
                        H.insert htable n xs
                        return [(n, x) | x <- xs]
                        ) topSortedNodes
                    return $ Right $ concat edges
                G.CyclicSCC xs : _ ->
                    return $ Left xs

        edgeIsInValid (ev1, ev2) = St.member ev2 maxEventSet
    in
        case transitiveClosureEdges of
            Left cyclicScc -> throwError' (prioritisePartialOrderCyclicOrder cyclicScc)
            Right edges -> 
                case filter edgeIsInValid edges of
                    [] -> return $ Sq.fromList [(v1, v2) | (v1, v2) <- edges]
                    (_, e) : _ -> throwError' (prioritiseNonMaximalElement e)
