{-# LANGUAGE FlexibleContexts, FlexibleInstances, IncoherentInstances,
    MultiParamTypeClasses, TypeSynonymInstances, UndecidableInstances #-}
module CSPM.Evaluator.ValuePrettyPrinter (
    prettyPrintAllRequiredProcesses,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Dot
import CSPM.Evaluator.Monad
import CSPM.Evaluator.ProcessValues
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import CSPM.PrettyPrinter
import CSPM.Prelude
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.List (partition)
import Util.Precedence
import Util.PrettyPrint
import qualified Util.MonadicPrettyPrint as M

instance (Applicative m, Monad m) => M.MonadicPrettyPrintable m (Exp Name) where
    prettyPrint = return . prettyPrint

instance (Applicative m, Monad m) => M.MonadicPrettyPrintable m TCExp where
    prettyPrint = return . prettyPrint

instance PrettyPrintable Event where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable (S.Seq Event) where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable ProcName where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable Value where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable ValueSet where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable UnCompiledProc where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable ProcOperator where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable ScopeIdentifier where
    prettyPrint = runIdentity . M.prettyPrint

instance (Applicative m, Monad m, M.MonadicPrettyPrintable m Value) =>
        M.MonadicPrettyPrintable m Event where
    prettyPrint Tau = M.char 'τ'
    prettyPrint Tick = M.char '✓'
    prettyPrint (UserEvent v) = M.prettyPrint v

    prettyPrintBrief Tau = M.char 'τ'
    prettyPrintBrief Tick = M.char '✓'
    prettyPrintBrief (UserEvent v) = M.prettyPrintBrief v

instance (Applicative m, Monad m) => M.MonadicPrettyPrintable m ProcOperator where
    prettyPrint (Chase True) = M.text "chase"
    prettyPrint (Chase False) = M.text "chase_no_cache"
    prettyPrint Diamond = M.text "diamond"
    prettyPrint Explicate = M.text "explicate"
    prettyPrint Normalize = M.text "normal"
    prettyPrint ModelCompress = M.text "model_compress"
    prettyPrint StrongBisim = M.text "sbisim"
    prettyPrint TauLoopFactor = M.text "tau_loop_factor"
    prettyPrint WeakBisim = M.text "wbisim"

instance (F.Foldable f) => M.MonadicPrettyPrintable EvaluationMonad (f Event) where
    prettyPrint sevs =
        let
            evs = F.toList sevs
            (vevents, oevents) = partition isUserEvent evs
            isUserEvent (UserEvent _) = True
            isUserEvent _ = False
        in case evs of
            [] -> M.braces M.empty
            _ -> do
                mvs <- compressIntoEnumeratedSet (fromList (map (\ (UserEvent v) -> v) vevents))
                case mvs of
                    Just vs -> do
                        ds1 <- mapM M.prettyPrint vs
                        ds2 <- mapM M.prettyPrint oevents
                        M.braces (M.bars (M.list (return $ ds2++ds1)))
                    Nothing -> 
                        -- Compression unsucesful
                        M.braces (M.list (mapM M.prettyPrint evs))

instance (F.Foldable f) => M.MonadicPrettyPrintable Identity (f Event) where
    prettyPrint evs = M.braces (M.list (mapM M.prettyPrint (F.toList evs)))

instance (Applicative m, Monad m, M.MonadicPrettyPrintable m Value) =>
        M.MonadicPrettyPrintable m ProcName where
    prettyPrint (ProcName s) = M.prettyPrint s
    prettyPrintBrief (ProcName s) = M.prettyPrintBrief s

instance (Applicative m, Monad m, M.MonadicPrettyPrintable m Value) =>
        M.MonadicPrettyPrintable m ScopeIdentifier where
    prettyPrint (SFunctionBind n args Nothing) =
        M.prettyPrint n
        M.<> M.hcat (mapM (\as -> M.parens (M.list (mapM M.prettyPrint as))) args)
    prettyPrint (SFunctionBind n args (Just pn)) =
        M.prettyPrint pn M.<> M.text "::" M.<> M.prettyPrint (SFunctionBind n args Nothing)
    prettyPrint (SVariableBind args Nothing) =
        M.text "ANNON" M.<> (M.parens (M.list (mapM M.prettyPrint args)))
    prettyPrint (SVariableBind args (Just pn)) =
        M.prettyPrint pn M.<> M.text "::" M.<> M.prettyPrint (SVariableBind args Nothing)

    prettyPrintBrief (SFunctionBind n args Nothing) =
        M.prettyPrintBrief n M.<> M.hcat (mapM (\as -> M.parens $
            case as of 
                [] -> M.empty
                _ -> M.ellipsis) args)
    prettyPrintBrief (SFunctionBind n args (Just pn)) =
        M.prettyPrintBrief pn M.<> M.text "::"
        M.<> M.prettyPrintBrief (SFunctionBind n args Nothing)
    prettyPrintBrief (SVariableBind args Nothing) =
        M.text "ANNON" M.<> (M.parens (M.list (mapM M.prettyPrintBrief args)))
    prettyPrintBrief (SVariableBind args (Just pn)) =
        M.prettyPrintBrief pn M.<> M.text "::"
        M.<> M.prettyPrintBrief (SVariableBind args Nothing)

instance (Applicative m, F.Foldable seq, Functor seq, Monad m, 
            M.MonadicPrettyPrintable m ev, M.MonadicPrettyPrintable m evs) => 
        M.MonadicPrettyPrintable m (CSPOperator seq ev evs (seq (ev,ev))) where
    prettyPrint (PAlphaParallel as) =
        M.text "||" M.<+> M.list (mapM (\ a -> M.prettyPrint a) (F.toList as))
    prettyPrint (PException a) =
        M.text "[|" M.<> M.prettyPrint a M.<> M.text "|>" 
    prettyPrint PExternalChoice = M.text "[]"
    prettyPrint (PGenParallel a) =
        M.text "||" M.<+> M.brackets (M.prettyPrint a)
    prettyPrint (PHide a) =
        M.char '\\' M.<+> M.prettyPrint a
    prettyPrint PInternalChoice = M.text "|~|"
    prettyPrint PInterleave = M.text "|||"
    prettyPrint PInterrupt = M.text "/\\"
    prettyPrint (PLinkParallel evm) =
        M.text "[" M.<>
            M.list (mapM (\(evLeft, evRight) -> 
                            M.prettyPrint evLeft M.<+> M.text "<-" 
                                M.<+> M.prettyPrint evRight) $ F.toList evm)
        M.<> M.text "]"
    prettyPrint (POperator op) = M.prettyPrint op
    prettyPrint (PPrefix e) =
        M.prettyPrint e M.<+> M.text "->"
    prettyPrint (PRename evm) =
        M.text "[[" 
        M.<> M.list (mapM (\ (evOld, evNew) -> 
                            M.prettyPrint evOld M.<+> M.text "<-" 
                            M.<+> M.prettyPrint evNew) $ F.toList evm) 
        M.<> M.text "]]"
    prettyPrint PSequentialComp = M.text ";"
    prettyPrint PSlidingChoice = M.text "|>"

instance Precedence (Proc seq CSPOperator pn ev evs (seq (ev,ev))) where
    precedence (PUnaryOp (PHide _) _) = 10
    precedence (POp PInterleave _) = 9
    precedence (PBinaryOp (PException _) _ _) = 8
    precedence (POp (PAlphaParallel _) _) = 8
    precedence (POp (PGenParallel _) _) = 8
    precedence (PBinaryOp (PLinkParallel _) _ _) = 8
    precedence (POp PInternalChoice _) = 7
    precedence (POp PExternalChoice _) = 6
    precedence (PBinaryOp PInterrupt _ _) = 5
    precedence (PBinaryOp PSlidingChoice _ _) = 4
    precedence (PBinaryOp PSequentialComp _ _) = 3
    precedence (PUnaryOp (PPrefix _) _) = 2
    precedence (PUnaryOp (PRename _) _) = 1

    precedence (PProcCall _ _) = 0
    precedence (PUnaryOp (POperator _) _) = 0

ppBinaryOp, ppBriefBinaryOp ::
    (F.Foldable seq, Functor seq, M.MonadicPrettyPrintable m pn,
        M.MonadicPrettyPrintable m ev, M.MonadicPrettyPrintable m evs) => 
    Proc seq CSPOperator pn ev evs (seq (ev,ev)) ->
    m Doc ->
    Proc seq CSPOperator pn ev evs (seq (ev,ev)) -> 
    Proc seq CSPOperator pn ev evs (seq (ev,ev)) ->
    m Doc
ppBinaryOp op opd p1 p2 =
    M.sep (sequence [M.prettyPrintPrec (precedence op) p1,
        opd M.<+> M.prettyPrintPrec (precedence op) p2])

ppBriefBinaryOp op opd p1 p2 =
    M.sep (sequence [M.prettyPrintBriefPrec (precedence op) p1,
        opd M.<+> M.prettyPrintBriefPrec (precedence op) p2])

maybeNull :: (Applicative m, F.Foldable s, Monad m) => s a -> m Doc -> m Doc
maybeNull s _ | null (F.toList s) = M.text "STOP"
maybeNull _ d = d

maybeNull' :: (Applicative m, F.Foldable s, Monad m) => s a -> m Doc -> m Doc
maybeNull' s _ | null (F.toList s) = M.text "SKIP"
maybeNull' _ d = d

instance 
    (F.Foldable seq, Functor seq, M.MonadicPrettyPrintable m pn,
        M.MonadicPrettyPrintable m ev, M.MonadicPrettyPrintable m evs) => 
        M.MonadicPrettyPrintable m 
            (Proc seq CSPOperator pn ev evs (seq (ev,ev))) where

    prettyPrint (op@(POp (PAlphaParallel as) ps)) = maybeNull' ps $ 
        case length (F.toList as) of
            2 ->
                let [p1, p2] = F.toList ps
                    [a1, a2] = F.toList as
                in ppBinaryOp op (
                    M.char '[' M.<> M.prettyPrint a1 M.<+>
                    M.text "||" M.<+> M.prettyPrint a2 M.<> M.char ']') p1 p2
            _ ->
                M.text "||" M.<+> M.braces (M.list (zipWithM (\ a p -> 
                        M.parens (M.prettyPrint a M.<> M.comma M.<+> M.prettyPrint p))
                    (F.toList as) (F.toList ps)))
    prettyPrint (op@(PBinaryOp (PException a) p1 p2)) =
        ppBinaryOp op (M.text "[|" M.<+> M.prettyPrint a M.<+> M.text "|>") p1 p2
    prettyPrint (op@(POp PExternalChoice ps)) =
        let flatten (POp PExternalChoice ps) = concatMap flatten (F.toList ps)
            flatten p = [p]
            ps' = flatten (POp PExternalChoice ps)
        in maybeNull ps' $ M.sep (M.punctuateFront (M.text "[] ") $
                    mapM (M.prettyPrintPrec (precedence op)) ps')
    prettyPrint (op@(POp (PGenParallel a) ps)) = maybeNull' ps $ 
        M.sep (M.punctuateFront
                (M.text "[|" M.<+> M.prettyPrint a M.<+> M.text "|] ")
                (mapM (M.prettyPrintPrec (precedence op)) (F.toList ps)))
    prettyPrint (op@(PUnaryOp (PHide a) p)) =
        M.prettyPrintPrec (precedence op) p
        M.<+> M.char '\\' M.<+> M.prettyPrint a
    prettyPrint (op@(POp PInternalChoice ps)) =
        let flatten (POp PInternalChoice ps) = concatMap flatten (F.toList ps)
            flatten p = [p]
            ps' = flatten (POp PInternalChoice ps)
        in M.sep (M.punctuateFront (M.text "|~| ") $
                mapM (M.prettyPrintPrec (precedence op)) ps')
    prettyPrint (op@(POp PInterleave ps)) = maybeNull ps $ 
        M.sep (M.punctuateFront (M.text "||| ") $
            mapM (M.prettyPrintPrec (precedence op)) $ F.toList ps)
    prettyPrint (op@(PBinaryOp PInterrupt p1 p2)) =
        ppBinaryOp op (M.text "/\\") p1 p2
    prettyPrint (op@(PBinaryOp (PLinkParallel evm) p1 p2)) =
        M.prettyPrintPrec (precedence op) p1 M.<+> M.text "[" M.<>
            M.list (mapM (\(evLeft, evRight) -> 
                            M.prettyPrint evLeft M.<+> M.text "<->" 
                                M.<+> M.prettyPrint evRight) $ F.toList evm)
        M.<> M.text "]" M.<+> M.prettyPrintPrec (precedence op) p2
    prettyPrint (op@(PUnaryOp (POperator cop) p)) = 
        M.prettyPrint cop M.<> M.parens (M.prettyPrintPrec (precedence op) p)
    prettyPrint (op@(PUnaryOp (PPrefix e) p)) =
        M.prettyPrint e M.<+> M.text "->"
        M.<+> M.prettyPrintPrec (precedence op) p
    prettyPrint (op@(PUnaryOp (PRename evm) p)) =
        M.prettyPrintPrec (precedence op) p M.<> M.text "[[" 
        M.<> M.list (mapM (\ (evOld, evNew) -> 
                            M.prettyPrint evOld M.<+> M.text "<-" 
                            M.<+> M.prettyPrint evNew) $ F.toList evm) 
        M.<> M.text "]]"
    prettyPrint (op@(PBinaryOp PSequentialComp p1 p2)) =
        ppBinaryOp op (M.char ';') p1 p2
    prettyPrint (op@(PBinaryOp PSlidingChoice p1 p2)) =
        ppBinaryOp op (M.text "[>") p1 p2
    prettyPrint (PProcCall n _) = M.prettyPrint n

    prettyPrintBrief (op@(POp (PAlphaParallel as) ps)) = maybeNull' ps $ 
        M.sep (M.punctuateFront (M.text "[…||…] ")
            (mapM (M.prettyPrintBriefPrec (precedence op)) (F.toList ps)))
    prettyPrintBrief (op@(PBinaryOp (PException a) p1 p2)) =
        ppBriefBinaryOp op (M.text "[|…|>") p1 p2
    prettyPrintBrief (op@(POp PExternalChoice ps)) =
        let flatten (POp PExternalChoice ps) = concatMap flatten (F.toList ps)
            flatten p = [p]
            ps' = flatten (POp PExternalChoice ps)
        in maybeNull ps' $ M.sep (M.punctuateFront (M.text "[] ") $
                mapM (M.prettyPrintBriefPrec (precedence op)) ps')
    prettyPrintBrief (op@(POp (PGenParallel a) ps)) = maybeNull' ps $ 
        M.sep (M.punctuateFront (M.text "[|…|] ")
            (mapM (M.prettyPrintBriefPrec (precedence op)) (F.toList ps)))
    prettyPrintBrief (op@(PUnaryOp (PHide a) p)) =
        M.prettyPrintBriefPrec (precedence op) p
        M.<+> M.char '\\' M.<+> M.braces M.ellipsis
    prettyPrintBrief (op@(POp PInternalChoice ps)) =
        let flatten (POp PInternalChoice ps) = concatMap flatten (F.toList ps)
            flatten p = [p]
            ps' = flatten (POp PInternalChoice ps)
        in M.sep (M.punctuateFront (M.text "|~| ") $
                mapM (M.prettyPrintBriefPrec (precedence op)) ps')
    prettyPrintBrief (op@(POp PInterleave ps)) = maybeNull ps $ 
        M.sep (M.punctuateFront (M.text "||| ") $
            mapM (M.prettyPrintBriefPrec (precedence op)) $ F.toList ps)
    prettyPrintBrief (op@(PBinaryOp PInterrupt p1 p2)) =
        ppBriefBinaryOp op (M.text "/\\") p1 p2
    prettyPrintBrief (op@(PBinaryOp (PLinkParallel evm) p1 p2)) =
        ppBriefBinaryOp op (M.text "[…<->…]") p1 p2
    prettyPrintBrief (op@(PUnaryOp (POperator cop) p)) = 
        M.prettyPrintBrief cop M.<>
            M.parens (M.prettyPrintBriefPrec (precedence op) p)
    prettyPrintBrief (op@(PUnaryOp (PPrefix e) p)) =
        M.prettyPrintBrief e M.<+> M.text "->" M.<+> M.ellipsis
        --M.<+> M.prettyPrintBriefPrec (precedence op) p
    prettyPrintBrief (op@(PUnaryOp (PRename evm) p)) =
        M.prettyPrintBriefPrec (precedence op) p M.<> M.text "[[…]]"
    prettyPrintBrief (op@(PBinaryOp PSequentialComp p1 p2)) =
        ppBriefBinaryOp op (M.char ';') p1 p2
    prettyPrintBrief (op@(PBinaryOp PSlidingChoice p1 p2)) =
        ppBriefBinaryOp op (M.text "[>") p1 p2
    prettyPrintBrief (PProcCall n _) = M.prettyPrintBrief n

instance (Applicative m, Monad m,
        M.MonadicPrettyPrintable m TCExp,
        M.MonadicPrettyPrintable m UProc,
        M.MonadicPrettyPrintable m ValueSet) =>
        M.MonadicPrettyPrintable m Value where
    prettyPrint (VInt i) = M.int i
    prettyPrint (VChar c) = M.char c
    prettyPrint (VBool True) = M.text "true"
    prettyPrint (VBool False) = M.text "false"
    prettyPrint (VTuple vs) = M.parens (M.list $ mapM M.prettyPrint (elems vs))
    prettyPrint (VDot vs) = M.dotSep (mapM M.prettyPrint vs)
    prettyPrint (VChannel n) = M.prettyPrint n
    prettyPrint (VDataType n) = M.prettyPrint n
    prettyPrint (VList (vs@((VChar _) : _))) =
        M.text (map (\ (VChar c) -> c) vs)
    prettyPrint (VList vs) = M.angles (M.list $ mapM M.prettyPrint vs)
    prettyPrint (VSet s) = M.prettyPrint s
    prettyPrint (VFunction (FBuiltInFunction n args) _) =
        M.prettyPrint n M.<> case args of
                            [] -> M.empty
                            _ -> M.parens (M.list (mapM M.prettyPrint args))
    prettyPrint (VFunction (FLambda e Nothing) _) = M.prettyPrint e
    prettyPrint (VFunction (FLambda e (Just p)) _) =
        M.prettyPrint p M.<> M.text "::" M.<> M.parens (M.prettyPrint e)
    prettyPrint (VFunction (FMatchBind n args parent) _) =
        (case parent of
            Just pid -> M.prettyPrint pid M.<> M.text "::"
            Nothing -> M.empty
        ) M.<> M.prettyPrint n M.<>
        case args of
            [] -> M.empty
            _ -> M.hcat (mapM (\ as -> M.parens (M.list (mapM M.prettyPrint as))) args)
    prettyPrint (VProc p) = M.prettyPrint p
    prettyPrint (VThunk th) = M.text "<thunk>"

    prettyPrintBrief (VSet s) = M.braces M.ellipsis
    prettyPrintBrief (VList s) = M.angles M.ellipsis
    prettyPrintBrief (VTuple vs) =
        M.parens (M.list $ mapM M.prettyPrintBrief (elems vs))
    prettyPrintBrief (VDot vs) = M.dotSep (mapM M.prettyPrintBrief vs)
    prettyPrintBrief (VChannel n) = M.prettyPrint n
    prettyPrintBrief (VDataType n) = M.prettyPrint n
    prettyPrintBrief (VFunction _ _) = M.text "<function>"
    prettyPrintBrief (VProc p) = M.prettyPrintBrief p
    prettyPrintBrief v = M.prettyPrint v 

instance M.MonadicPrettyPrintable EvaluationMonad ValueSet where
    prettyPrint Integers = M.text "Integers"
    prettyPrint Processes = M.text "Proc"
    prettyPrint (IntSetFrom lb) = M.braces (M.int lb M.<> M.text "...")
    prettyPrint (AllSequences vs) = M.text "Seq" M.<> M.parens (M.prettyPrint vs)
    prettyPrint (CartesianProduct vss CartTuple) =
        M.parens (M.list (mapM M.prettyPrint vss))
    prettyPrint (CompositeSet ss) =
        M.text "Union" M.<> M.parens (M.braces (M.list (mapM M.prettyPrint (F.toList ss))))
    prettyPrint s = do
        -- Try and compress
        mvs <- compressIntoEnumeratedSet s
        case mvs of
            Just vs ->
                -- Compression succesful
                M.text "{|" M.<+> M.list (mapM M.prettyPrint vs) M.<+> M.text "|}"
            Nothing -> 
                case s of
                    CartesianProduct vss CartDot ->
                        M.hcat (M.punctuate (M.text ".") (mapM M.prettyPrint vss))
                    ExplicitSet _ ->
                        M.braces (M.list (mapM M.prettyPrint (toList s)))

instance M.MonadicPrettyPrintable Identity ValueSet where
    prettyPrint Integers = M.text "Integers"
    prettyPrint Processes = M.text "Proc"
    prettyPrint (IntSetFrom lb) = M.braces (M.int lb M.<> M.text "...")
    prettyPrint (AllSequences vs) = M.text "Seq" M.<> M.parens (M.prettyPrint vs)
    prettyPrint (CartesianProduct vss CartTuple) =
        M.parens (M.list (mapM M.prettyPrint vss))
    prettyPrint (CompositeSet ss) =
        M.text "Union" M.<> M.parens (M.braces (M.list (mapM M.prettyPrint (F.toList ss))))
    prettyPrint (CartesianProduct vss CartDot) =
        M.hcat (M.punctuate (M.text ".") (mapM M.prettyPrint vss))
    prettyPrint (s@(ExplicitSet _)) =
        M.braces (M.list (mapM M.prettyPrint (toList s)))

-- | Pretty prints the given process and all processes that it depends upon.
prettyPrintAllRequiredProcesses ::
    (F.Foldable seq, PrettyPrintable (Proc seq op ProcName ev evs evm)) => 
    Proc seq op ProcName ev evs evm -> Doc
prettyPrintAllRequiredProcesses p =
    let
        (pInit, namedPs') = splitProcIntoComponents p
        stopName = head [name b | b <- builtins False, stringName b == "STOP"]
        namedPs =
            filter (\ (ProcName s, _) -> scopeFunctionName s /= stopName) namedPs'
        ppNamedProc (n,p) =
            hang (prettyPrint n <+> char '=') tabWidth (prettyPrint p)
    in vcat (punctuate (char '\n') ((map ppNamedProc namedPs)++[prettyPrint pInit]))
