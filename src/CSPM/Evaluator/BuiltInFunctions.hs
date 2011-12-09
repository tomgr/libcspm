module CSPM.Evaluator.BuiltInFunctions where

import Control.Monad

import qualified CSPM.Compiler.Set as CS
import CSPM.DataStructures.Names
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import Util.Exception

builtInFunctions :: [(Name, Value)]
builtInFunctions = 
    let
        cspm_union [VSet s1, VSet s2] = S.union s1 s2
        cspm_inter [VSet s1, VSet s2] = S.intersection s1 s2
        cspm_diff [VSet s1, VSet s2] = S.difference s1 s2
        cspm_Union [VSet s] = S.unions (map (\ (VSet s) -> s) (S.toList s))
        cspm_Inter [VSet s] = 
            S.intersections (map (\ (VSet s) -> s) (S.toList s))
        cspm_member [v, VSet s] = VBool $ S.member v s
        cspm_card [VSet s] = VInt $ S.card s
        cspm_empty [VSet s] = VBool $ S.empty s
        cspm_set [VList xs] = S.fromList xs
        cspm_Set [VSet s] = S.powerset s
        -- | Set of all sequences over s
        cspm_Seq [VSet s] = S.allSequences s
        cspm_seq [VSet s] = S.toList s
        
        
        cspm_length [VList xs] = VInt $ (toInteger (length xs))
        cspm_null [VList xs] = VBool $ null xs
        cspm_head [VList []] = throwError headEmptyList
        cspm_head [VList (x:xs)] = x
        cspm_tail [VList []] = throwError tailEmptyList
        cspm_tail [VList (x:xs)] = xs
        cspm_concat [VList xs] = concat (map (\(VList ys) -> ys) xs)
        cspm_elem [v, VList vs] = VBool $ v `elem` vs
        csp_chaos [VSet a] = VProc $ PProcCall n (Just p)
            where
                n = procId (Name "CHAOS") [[VSet a]]
                evSet = S.valueSetToEventSet a
                branches = [PPrefix ev (PProcCall n Nothing) | ev <- CS.toList evSet]
                stopProc = PProcCall (procId (Name "STOP") []) (Just csp_stop)
                p = PInternalChoice (stopProc:branches)
        
        -- | Functions that return sets
        set_funcs = [
            ("union", cspm_union), ("inter", cspm_inter), 
            ("diff", cspm_diff), ("Union", cspm_Union), 
            ("Inter", cspm_Inter), ("set", cspm_set), 
            ("Set", cspm_Set), ("Seq", cspm_Seq)
            ]
        
        -- | Functions that return sequences
        seq_funcs = [
            ("seq", cspm_seq), ("tail", cspm_tail), ("concat", cspm_concat)
            ]
        
        -- | Functions that mutate processes (like compression functions).
        proc_operators = [
            ("chase", Chase),
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
            ("empty", cspm_empty), ("CHAOS", csp_chaos)
            ]
        
        mkFunc (s, f) = (Name s, VFunction (\ vs -> return (f vs)))
        
        procs = [
            (csp_stop_id, csp_stop),
            (csp_skip_id, PPrefix Tick (PProcCall csp_stop_id (Just csp_stop)))
            ]
        
        csp_skip_id = procId (Name "SKIP") []
        csp_stop_id = procId (Name "STOP") []
        csp_stop = PExternalChoice []
        
        mkProc (s, p) = (Name s, VProc p)
        
        cspm_true = ("true", VBool True)
        cspm_false = ("false", VBool False)
        cspm_True = ("True", VBool True)
        cspm_False = ("False", VBool False)
        
        constants = [cspm_true, cspm_false, cspm_True, cspm_False]
        
        mkConstant (s, v) = (Name s, v)
    in
        map mkFunc (
            map (\ (n, f) -> (n, VSet . f)) set_funcs
            ++ map (\ (n, f) -> (n, VList . f)) seq_funcs
            ++ map (\ (n, po) -> 
                        (n,\[VProc p]-> VProc $ POperator po p)) proc_operators
            ++ other_funcs)
        ++ map mkProc procs
        ++ map mkConstant constants

injectBuiltInFunctions :: EvaluationMonad a -> EvaluationMonad a
injectBuiltInFunctions = addScopeAndBind builtInFunctions
