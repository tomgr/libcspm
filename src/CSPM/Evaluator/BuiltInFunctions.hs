module CSPM.Evaluator.BuiltInFunctions (
    injectBuiltInFunctions,
    builtInName
) where

import Control.Monad
import qualified Data.Map as M

import qualified CSPM.Compiler.Set as CS
import CSPM.DataStructures.Names
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import CSPM.Prelude
import Util.Exception

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
                branches = [PUnaryOp (PPrefix ev) chaosCall | ev <- CS.toList evSet]
                stopProc = PProcCall (procId (nameForString "STOP") [] Nothing) csp_stop
                p = POp PInternalChoice (stopProc:branches)
        
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
        csp_stop = POp PExternalChoice []
        csp_skip = PUnaryOp (PPrefix Tick) csp_stop
        
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
