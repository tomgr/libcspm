{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module CSPM.Evaluator.DeepSeq () where

import Control.DeepSeq

import CSPM.Evaluator.ProcessValues
import CSPM.Evaluator.ValueSet
import CSPM.Evaluator.Values

-- In general, we don't need to worry about errors in names as we strictly
-- construct them, and strictly insert them everywhere. We also don't need to
-- worry about errors inside expressions since the desugarer is strict.

instance NFData ScopeIdentifier where
    rnf (SFunctionBind n vss s) = n `seq` rnf vss `seq` rnf s
    rnf (SVariableBind vs s) = rnf vs `seq` rnf s

instance NFData FunctionIdentifier where
    rnf (FBuiltInFunction n xs) = n `seq` rnf xs
    rnf (FLambda n p) = n `seq` rnf p
    rnf (FMatchBind n vss p) = n `seq` rnf vss `seq` rnf p

instance NFData Value where
    rnf (VInt x) = rnf x
    rnf (VChar x) = rnf x
    rnf (VBool x) = rnf x
    rnf (VTuple xs) = rnf xs
    rnf (VDot xs) = rnf xs
    rnf (VChannel n) = n `seq` ()
    rnf (VDataType n) = n `seq` ()
    rnf (VList xs) = rnf xs
    rnf (VSet xs) = rnf xs
    rnf (VMap m) = rnf m
    rnf (VFunction fid _) = rnf fid
    rnf (VProc p) = rnf p

instance NFData ValueSet where
    rnf Integers = ()
    rnf Processes = ()
    rnf (ExplicitSet s) = rnf s
    rnf (IntSetFrom x) = rnf x
    rnf (CompositeSet xs) = rnf xs
    rnf (AllSequences xs) = rnf xs
    rnf (CartesianProduct xs _) = rnf xs
    rnf (Powerset xs) = rnf xs

instance NFData UProc where
    rnf (PUnaryOp op p) = rnf op `seq` rnf p
    rnf (PBinaryOp op p1 p2) = rnf op `seq` rnf p1 `seq` rnf p2
    rnf (POp op p) = rnf op `seq` rnf p
    -- We can't do full deep evaluation here, otherwise we would spin. However,
    -- hashing and equality comparison bottom out at a PProcCall anyway.
    rnf (PProcCall pn p) = rnf pn

instance NFData UnCompiledOperator where
    rnf (PAlphaParallel evs) = rnf evs
    rnf (PException evs) = rnf evs
    rnf PExternalChoice = ()
    rnf (PGenParallel evs) = rnf evs
    rnf (PHide evs) = rnf evs
    rnf PInternalChoice = ()
    rnf PInterrupt = ()
    rnf PInterleave = ()
    rnf (PLinkParallel evm) = rnf evm
    rnf (POperator op) = rnf op
    rnf (PPrefix ev) = rnf ev
    rnf (PRename evm) = rnf evm
    rnf PSequentialComp = ()
    rnf PSlidingChoice = ()
    rnf (PSynchronisingExternalChoice evs) = rnf evs
    rnf (PSynchronisingInterrupt evs) = rnf evs

instance NFData UnCompiledProcOperator where
    rnf (Chase b) = rnf b
    rnf DelayBisim = ()
    rnf Determinise = ()
    rnf Diamond = ()
    rnf (Explicate b) = rnf b
    rnf (FailureWatchdog evs ev) = rnf evs `seq` rnf ev
    rnf (Normalize b) = rnf b
    rnf ModelCompress = ()
    rnf (Prioritise b evs) = rnf b `seq` rnf evs
    rnf StrongBisim = ()
    rnf TauLoopFactor = ()
    rnf (TraceWatchdog evs ev) = rnf evs `seq` rnf ev
    rnf WeakBisim = ()

instance NFData ProcName where
    rnf (ProcName s) = rnf s

instance NFData Event where
    rnf Tau = ()
    rnf Tick = ()
    rnf (UserEvent vs) = rnf vs
