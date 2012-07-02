module CSPM.Evaluator.BuiltInFunctions (
    injectBuiltInFunctions,
    builtInName
) where

import Data.Array
import Data.List
import Control.Monad.ST
import Data.Hashable
import qualified Data.Map as M
import qualified Data.Sequence as Sq

import CSPM.DataStructures.Names
import CSPM.Evaluator.Dot
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import CSPM.Prelude
import qualified Data.Graph.ST as G
import Util.Exception
import Util.Prelude

bMap = M.fromList [(stringName b, name b) | b <- builtins True]
builtInName s = M.findWithDefault (panic "builtin not found") s bMap

builtInFunctions :: [(Name, Value)]
builtInFunctions = 
    let
        nameForString s = builtInName s
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
            let f = relationalImage s in VFunction (\[x] -> f x >>= return . VSet)
        cspm_mtransclose [VSet s1, VSet s2] = fdrSymmetricTransitiveClosure s1 s2
        cspm_relational_inverse_image s = cspm_relational_image [cspm_transpose s]
        
        cspm_length [VList xs] = VInt $ length xs
        cspm_null [VList xs] = VBool $ null xs
        cspm_head [VList []] = throwError headEmptyListMessage
        cspm_head [VList (x:xs)] = x
        cspm_tail [VList []] = throwError tailEmptyListMessage
        cspm_tail [VList (x:xs)] = xs
        cspm_concat [VList xs] = concat (map (\(VList ys) -> ys) xs)
        cspm_elem [v, VList vs] = VBool $ v `elem` vs
        csp_chaos [VSet a] = VProc chaosCall
            where
                chaosCall = PProcCall n p
                n = procId (nameForString "CHAOS") [[VSet a]] Nothing
                evSet = S.valueSetToEventSet a
                branches :: Sq.Seq UProc
                branches = fmap (\ ev -> PUnaryOp (PPrefix ev) chaosCall) evSet
                stopProc = PProcCall (procId (nameForString "STOP") [] Nothing) csp_stop
                p = POp PInternalChoice (stopProc Sq.<| branches)
        csp_loop [VProc p] = VProc (PUnaryOp PSeqCompLoop p)

        cspm_extensions [v] = do
            exs <- extensions v
            return $ VSet $ S.fromList exs
        cspm_productions [v] = do
            exs <- productions v
            return $ VSet $ S.fromList exs

        -- | Functions that return sets
        set_funcs = [
            ("union", cspm_union), ("inter", cspm_inter), 
            ("diff", cspm_diff), ("Union", cspm_Union), 
            ("Inter", cspm_Inter), ("set", cspm_set), 
            ("Set", cspm_Set), ("Seq", cspm_Seq),
            ("mtransclose", cspm_mtransclose)
            ]
        
        -- | Functions that return sequences
        seq_funcs = [
            ("seq", cspm_seq), ("tail", cspm_tail), ("concat", cspm_concat)
            ]
        
        -- | Functions that mutate processes (like compression functions).
        proc_operators = [
            ("chase", Chase True),
            ("chase_nocache", Chase False),
            ("diamond", Diamond),
            ("explicate", Explicate),
            ("normal", Normalize),
            ("model_compress", ModelCompress),
            ("sbisim", StrongBisim),
            ("tau_loop_factor", TauLoopFactor),
            ("wbisim", WeakBisim)
            ]
        
        -- | Functions that return something else
        other_funcs = [
            ("length", cspm_length), ("null", cspm_null), 
            ("head", cspm_head), ("elem", cspm_elem),
            ("member", cspm_member), ("card", cspm_card),
            ("empty", cspm_empty), ("CHAOS", csp_chaos),
            ("loop", csp_loop), ("relational_image", cspm_relational_image),
            ("relational_inverse_image", cspm_relational_inverse_image),
            ("transpose", cspm_transpose)
            ]

        -- | Functions that require a monadic context.
        monadic_funcs = [
            ("productions", cspm_productions), ("extensions", cspm_extensions)
            ]
        
        mkFunc (s, f) = (nameForString s, VFunction (\ vs -> return (f vs)))
        mkMonadicFunc (s, f) = (nameForString s, VFunction f)

        procs = [
            ("STOP", csp_stop),
            ("SKIP", csp_skip)
            ]
        
        csp_skip_id = procId (nameForString "SKIP") [] Nothing
        csp_stop_id = procId (nameForString "STOP") [] Nothing
        -- We actually inline stop, for efficiency
        csp_stop = PProcCall csp_stop_id (POp PExternalChoice Sq.empty)
        csp_skip = PProcCall csp_skip_id (PUnaryOp (PPrefix Tick) csp_stop)
        
        mkProc (s, p) = (nameForString s, VProc p)
        
        cspm_true = ("true", VBool True)
        cspm_false = ("false", VBool False)
        cspm_True = ("True", VBool True)
        cspm_False = ("False", VBool False)
        
        cspm_Bool = ("Bool", VSet (S.fromList [snd cspm_true, snd cspm_false]))
        cspm_Int = ("Int", VSet S.Integers)
        cspm_Proc = ("Proc", VSet S.Processes)
        cspm_Events = ("Events", VSet (S.fromList []))

        constants = [
            cspm_true, cspm_false, cspm_True, cspm_False,
            cspm_Bool, cspm_Int, cspm_Proc, cspm_Events
            ]
        
        mkConstant (s, v) = (nameForString s, v)
    in
        map mkFunc (
            map (\ (n, f) -> (n, VSet . f)) set_funcs
            ++ map (\ (n, f) -> (n, VList . f)) seq_funcs
            ++ map (\ (n, po) -> 
                        (n,\[VProc p]-> VProc $ PUnaryOp (POperator po) p)) proc_operators
            ++ other_funcs)
        ++ map mkMonadicFunc monadic_funcs
        ++ map mkProc procs
        ++ map mkConstant constants

injectBuiltInFunctions :: EvaluationMonad a -> EvaluationMonad a
injectBuiltInFunctions = addScopeAndBind builtInFunctions

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
            rs <- G.reflexiveTransitiveRepresentatives g
            return $! [tupleFromList [a,b] | (b,a) <- rs, S.member a vs2, S.member b vs2]
    in S.fromList $ runST computeRepresentatives
