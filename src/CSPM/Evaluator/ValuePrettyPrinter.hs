{-# LANGUAGE FlexibleContexts, FlexibleInstances, IncoherentInstances,
    MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances,
    UndecidableInstances #-}
module CSPM.Evaluator.ValuePrettyPrinter (
    prettyPrintAllRequiredProcesses,
) where

import Control.Monad
import Control.Monad.Identity
import CSPM.Syntax.Literals
import CSPM.Syntax.Names
import CSPM.Syntax.AST
import CSPM.Evaluator.AnalyserMonad
import CSPM.Evaluator.Dot
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import CSPM.PrettyPrinter
import CSPM.Prelude
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import qualified Data.Map as Mp
import Data.List (partition)
import Util.Annotated
import Util.Precedence
import Util.PrettyPrint
import qualified Util.MonadicPrettyPrint as M

instance (Applicative m, Monad m) => M.MonadicPrettyPrintable m (Exp Name) where
    prettyPrint = return . prettyPrint

instance (Applicative m, Monad m) => M.MonadicPrettyPrintable m TCExp where
    prettyPrint = return . prettyPrint

instance PrettyPrintable Event where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable EventSet where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable ProcName where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable Value where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable ValueSet where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable Proc where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable ProcOperator where
    prettyPrint = runIdentity . M.prettyPrint

instance PrettyPrintable InstantiatedFrame where
    prettyPrint = runIdentity . M.prettyPrint

instance (Applicative m, Monad m, M.MonadicPrettyPrintable m Value) =>
        M.MonadicPrettyPrintable m Event where
    prettyPrint Tau = M.char 'τ'
    prettyPrint Tick = M.char '✓'
    prettyPrint (UserEvent v) = M.prettyPrint v

    prettyPrintBrief Tau = M.char 'τ'
    prettyPrintBrief Tick = M.char '✓'
    prettyPrintBrief (UserEvent v) = M.prettyPrintBrief v

instance (Applicative m, Monad m,
             M.MonadicPrettyPrintable m EventSet,
            M.MonadicPrettyPrintable m Value) =>
        M.MonadicPrettyPrintable m ProcOperator where
    prettyPrint (Chase True) = M.text "chase"
    prettyPrint (Chase False) = M.text "chase_no_cache"
    prettyPrint DelayBisim = M.text "dbisim"
    prettyPrint Determinise = M.text "deter"
    prettyPrint Diamond = M.text "diamond"
    prettyPrint (Explicate False) = M.text "explicate"
    prettyPrint (Explicate True) = M.text "lazyenumerate"
    prettyPrint (FailureWatchdog evs ev) = M.text "failure_watchdog"
    prettyPrint ModelCompress = M.text "model_compress"
    prettyPrint (Normalize False) = M.text "normal"
    prettyPrint (Normalize True) = M.text "lazynorm"
    prettyPrint (Prioritise cache as) =
        (if cache then M.text "prioritise" else M.text "prioritise_nocache")
        M.<> M.parens (M.angles (M.list (mapM M.prettyPrint (F.toList as))))
    prettyPrint (PartialOrderPrioritise as) =
        M.text "prioritisepo"
        M.<> M.parens (M.braces (M.list (mapM (\ (ev1, ev2) ->
            M.parens (M.prettyPrint ev1 M.<> M.comma M.<+> M.prettyPrint ev2))
            (F.toList as))))
    prettyPrint (TraceWatchdog evs ev) = M.text "trace_watchdog"
    prettyPrint StrongBisim = M.text "sbisim"
    prettyPrint TauLoopFactor = M.text "tau_loop_factor"
    prettyPrint WeakBisim = M.text "wbisim"

    prettyPrintBrief (FailureWatchdog _ _) = M.text "failure_watchdog"
    prettyPrintBrief (Prioritise True as) = M.text "prioritise"
    prettyPrintBrief (Prioritise False as) = M.text "prioritise_nocache"
    prettyPrintBrief (PartialOrderPrioritise _) = M.text "prioritisepo"
    prettyPrintBrief (TraceWatchdog _ _) = M.text "trace_watchdog"
    prettyPrintBrief op = M.prettyPrint op

ppOperatorWithArg (FailureWatchdog evs ev) proc =
    M.text "failure_watchdog" M.<> M.parens (proc M.<> M.comma M.<+>
        M.prettyPrint evs M.<> M.comma M.<+> M.prettyPrint ev)
ppOperatorWithArg (Prioritise cache as) proc = do
    (if cache then M.text "prioritise" else M.text "prioritise_nocache")
    M.<> M.parens (proc M.<> M.comma M.<+>
        M.angles (M.list (mapM M.prettyPrint (F.toList as))))
ppOperatorWithArg (PartialOrderPrioritise as) proc = do
    M.text "prioritisepo" M.<> M.parens (proc M.<> M.comma M.<+>
        M.braces (M.list (mapM (\ (ev1, ev2) ->
            M.parens (M.prettyPrint ev1 M.<> M.comma M.<+> M.prettyPrint ev2))
            (F.toList as))))
ppOperatorWithArg (TraceWatchdog evs ev) proc =
    M.text "trace_watchdog" M.<> M.parens (proc M.<> M.comma M.<+>
        M.prettyPrint evs M.<> M.comma M.<+> M.prettyPrint ev)
ppOperatorWithArg op proc = M.prettyPrint op M.<> M.parens proc

ppBriefOperatorWithArg (FailureWatchdog evs ev) proc =
    M.text "failure_watchdog" M.<> M.parens (
        proc M.<> M.comma M.<+> M.ellipsis M.<> M.comma M.<+> M.ellipsis)
ppBriefOperatorWithArg (Prioritise cache as) proc = do
    (if cache then M.text "prioritise" else M.text "prioritise_nocache")
    M.<> M.parens (proc M.<> M.comma M.<+> M.ellipsis)
ppBriefOperatorWithArg (PartialOrderPrioritise as) proc = do
    M.text "prioritisepo"
    M.<> M.parens (proc M.<> M.comma M.<+> M.ellipsis)
ppBriefOperatorWithArg (TraceWatchdog evs ev) proc =
    M.text "trace_watchdog" M.<> M.parens (
        proc M.<> M.comma M.<+> M.ellipsis M.<> M.comma M.<+> M.ellipsis)
ppBriefOperatorWithArg op proc =
    M.prettyPrint op M.<> M.parens proc
    
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

instance (Applicative m, Monad m,
            M.MonadicPrettyPrintable m EventSet,
            M.MonadicPrettyPrintable m TCExp,
            M.MonadicPrettyPrintable m Proc,
            M.MonadicPrettyPrintable m ProcOperator,
            M.MonadicPrettyPrintable m ValueSet,
            M.MonadicPrettyPrintable m Value) =>
        M.MonadicPrettyPrintable m ProcName where
    prettyPrint (ProcName s) = M.prettyPrint s
    prettyPrintBrief (ProcName s) = M.prettyPrintBrief s

instance (Applicative m, Monad m,
        M.MonadicPrettyPrintable m EventSet,
        M.MonadicPrettyPrintable m TCExp,
        M.MonadicPrettyPrintable m Proc,
        M.MonadicPrettyPrintable m ProcOperator,
        M.MonadicPrettyPrintable m ValueSet,
        M.MonadicPrettyPrintable m Value) =>
        M.MonadicPrettyPrintable m InstantiatedFrame where
    prettyPrint (InstantiatedFrame _ frame freeVarValues args) =
        let
            freeVarNames = frameFreeVars frame

            varMap :: Mp.Map Name Value
            varMap = Mp.fromList (zip freeVarNames freeVarValues)

            ppPat :: TCPat -> m Doc
            ppPat (An _ _ (PCompList xs me _)) =
                M.angles (M.list $ mapM ppPat xs)
                M.<> case me of
                        Just (middle, ends) ->
                            M.char '^'
                            M.<> ppPat middle
                            M.<> case ends of
                                    [] -> M.empty
                                    _ -> M.char '^' M.<>
                                            M.angles (M.list $ mapM ppPat ends)
                        Nothing -> M.empty
            ppPat (An _ _ (PCompDot xs _)) = M.dotSep $! mapM ppPat xs
            ppPat (An _ _ (PDoublePattern p1 p2)) =
                -- TODO: select whichever binds more
                ppPat p1
            ppPat (An _ _ (PLit x)) = return $ prettyPrint x
            ppPat (An _ _ (PParen p)) = ppPat p
            ppPat (An _ _ (PTuple xs)) = M.parens (M.list $ mapM ppPat xs)
            ppPat (An _ _ (PSet xs)) = M.braces (M.list $ mapM ppPat xs)
            ppPat (An _ _ (PVar x)) | isNameDataConstructor x = M.prettyPrint x
            ppPat (An _ _ (PVar x)) =
                case Mp.lookup x varMap of
                    Just v -> M.prettyPrint v
                    Nothing -> M.char '_'
            ppPat (An _ _ PWildCard) = M.char '_'

            ppFrame :: FrameInformation -> m Doc
            ppFrame frame@(BuiltinFunctionFrame {}) =
                ppParentFrame (frameParent frame)
                M.<> M.prettyPrint (builtinFunctionFrameFunctionName frame)
            ppFrame frame@(FunctionFrame {}) =
                ppParentFrame (frameParent frame)
                M.<> M.prettyPrint (functionFrameFunctionName frame)
                M.<> case functionFramePatterns frame of
                        [] -> M.empty
                        args -> M.hcat (mapM (\ as -> M.parens (M.list (mapM ppPat as))) args)
            ppFrame frame@(LambdaFrame {}) =
                ppParentFrame (frameParent frame)
                M.<> M.parens (
                    M.char '\\'
                    M.<> M.list (mapM ppPat (lambdaFramePatterns frame))
                    M.<+> M.char '@'
                    M.<+> return (prettyPrint (lambdaFrameExpression frame))
                )
            ppFrame frame@(PartiallyAppliedFunctionFrame {}) =
                ppParentFrame (frameParent frame)
                M.<> (return $ prettyPrint
                        (partiallyAppliedFunctionFrameFunctionName frame))
            ppFrame frame@(VariableFrame {}) =
                ppParentFrame (frameParent frame)
                M.<> M.text "ANNON"
                M.<> M.parens (M.list (mapM ppPat (variableFramePatterns frame)))

            ppParentFrame Nothing = M.empty
            ppParentFrame (Just frame) = ppFrame frame M.<> M.text "::"

            ppTopMostFrame frame@(LambdaFrame {}) =
                ppParentFrame (frameParent frame)
                M.<> M.parens (return $ prettyPrint
                        (Lambda (lambdaFramePatterns frame)
                            (lambdaFrameExpression frame)))
            ppTopMostFrame frame = ppFrame frame
        in
            ppTopMostFrame frame
            M.<> case args of
                    [] -> M.empty
                    _ -> M.hcat (mapM (\ as ->
                            M.parens (M.list (mapM M.prettyPrint as))) args)

    prettyPrintBrief (InstantiatedFrame _ frame freeVarValues args) =
        let
            freeVarNames = frameFreeVars frame

            varMap :: Mp.Map Name Value
            varMap = Mp.fromList (zip freeVarNames freeVarValues)

            ppPat :: TCPat -> m Doc
            ppPat (An _ _ (PCompList xs me _)) =
                M.angles (M.list $ mapM ppPat xs)
                M.<> case me of
                        Just (middle, ends) ->
                            ppPat middle
                            M.<> case ends of
                                    [] -> M.empty
                                    _ -> M.angles (M.list $ mapM ppPat ends)
                        Nothing -> M.empty
            ppPat (An _ _ (PCompDot xs _)) = M.dotSep $! mapM ppPat xs
            ppPat (An _ _ (PDoublePattern p1 p2)) =
                -- TODO: select whichever binds more
                ppPat p1
            ppPat (An _ _ (PLit x)) = return $ prettyPrint x
            ppPat (An _ _ (PParen p)) = ppPat p
            ppPat (An _ _ (PTuple xs)) = M.parens (M.list $ mapM ppPat xs)
            ppPat (An _ _ (PSet xs)) = M.braces (M.list $ mapM ppPat xs)
            ppPat (An _ _ (PVar x)) | isNameDataConstructor x = M.prettyPrint x
            ppPat (An _ _ (PVar x)) =
                case Mp.lookup x varMap of
                    Just v -> M.prettyPrintBrief v
                    Nothing -> M.char '_'
            ppPat (An _ _ PWildCard) = M.char '_'

            spaceThreashold = 15

            spaceCost :: Value -> Int
            spaceCost (VInt _) = 1
            spaceCost (VChar _) = 1
            spaceCost (VBool _) = 1
            spaceCost (VTuple arr) = 2 + length vs + sum (map spaceCost vs)
                where vs = F.toList arr 
            spaceCost (VDot vs) = length vs + sum (map spaceCost vs)
            spaceCost (VChannel n) = length (show (prettyPrint n))
            spaceCost (VDataType n) = length (show (prettyPrint n))
            spaceCost (VList vs) = 2 + length vs + sum (map spaceCost vs)
            spaceCost (VSet s) = 2 + length vs + sum (map spaceCost vs)
                where vs = toList s
            spaceCost (VMap m) =
                    2 + sum (map spaceCost (map fst vs ++ map snd vs))
                where vs = Mp.toList m
            spaceCost (VFunction _ _) = spaceThreashold
            spaceCost (VProc _) = spaceThreashold
            spaceCost (VThunk _) = spaceThreashold

            patSpaceCost :: TCPat -> Int
            patSpaceCost (An _ _ (PCompList xs me _)) =
                2+sum (map patSpaceCost xs)
                + case me of
                        Just (middle, ends) ->
                            1 + patSpaceCost middle + 1 + 2 + 
                            sum (map patSpaceCost ends)
                        Nothing -> 0
            patSpaceCost (An _ _ (PCompDot xs _)) = sum (map patSpaceCost xs)
            patSpaceCost (An _ _ (PDoublePattern p1 p2)) = patSpaceCost p1
            patSpaceCost (An _ _ (PLit (Int _))) = 1
            patSpaceCost (An _ _ (PLit (Bool _))) = 1
            patSpaceCost (An _ _ (PLit (Char _))) = 1
            patSpaceCost (An _ _ (PLit (Loc _))) = 1
            patSpaceCost (An _ _ (PLit (String s))) = B.length s
            patSpaceCost (An _ _ (PParen p)) = patSpaceCost p
            patSpaceCost (An _ _ (PTuple xs)) =
                2 + length xs + sum (map patSpaceCost xs)
            patSpaceCost (An _ _ (PSet xs)) =
                2 + length xs + sum (map patSpaceCost xs)
            patSpaceCost (An _ _ (PVar x)) | isNameDataConstructor x =
                length (show x)
            patSpaceCost (An _ _ (PVar x)) =
                case Mp.lookup x varMap of
                    Just v -> spaceCost v
                    Nothing -> 1
            patSpaceCost (An _ _ PWildCard) = 1

            ppSmallValue v | spaceCost v > spaceThreashold = M.ellipsis
            ppSmallValue v = M.prettyPrint v
            ppSmallPat p | patSpaceCost p > spaceThreashold = M.ellipsis
            ppSmallPat p = ppPat p

            ppFrame :: FrameInformation -> m Doc
            ppFrame frame@(BuiltinFunctionFrame {}) =
                ppParentFrame (frameParent frame)
                M.<> M.prettyPrintBrief (builtinFunctionFrameFunctionName frame)
            ppFrame frame@(FunctionFrame {}) =
                ppParentFrame (frameParent frame)
                M.<> M.prettyPrintBrief (functionFrameFunctionName frame)
                M.<> case functionFramePatterns frame of
                        [] -> M.empty
                        args -> M.hcat (mapM (\ as ->
                            if length as > spaceThreashold `div` 2 then M.ellipsis
                            else M.parens (M.list (mapM ppSmallPat as))) args)
            ppFrame frame@(LambdaFrame {}) =
                ppParentFrame (frameParent frame)
                M.<> M.parens (
                    M.char '\\'
                    M.<> M.list (mapM ppPat (lambdaFramePatterns frame))
                    M.<+> M.char '@'
                    M.<+> M.ellipsis
                )
            ppFrame frame@(PartiallyAppliedFunctionFrame {}) =
                ppParentFrame (frameParent frame)
                M.<> (M.prettyPrintBrief
                        (partiallyAppliedFunctionFrameFunctionName frame))
            ppFrame frame@(VariableFrame {}) =
                ppParentFrame (frameParent frame)
                M.<> M.text "ANNON"
                M.<> M.parens (M.list (mapM ppPat (variableFramePatterns frame)))

            ppParentFrame Nothing = M.empty
            ppParentFrame (Just frame) = ppFrame frame M.<> M.text "::"
        in
            ppFrame frame
            M.<> case args of
                    [] -> M.empty
                    _ -> M.hcat (mapM (\ as ->
                            if length as > spaceThreashold `div` 2 then M.ellipsis
                            else M.parens (M.list (mapM ppSmallValue as))) args)

bufferFunction :: Monad m => BufferFullMode -> m Doc
bufferFunction WhenFullRefuseInputs = M.text "BUFFER"
bufferFunction (WhenFullExplode _) = M.text "WEAK_BUFFER"

instance (Applicative m, Monad m,
        M.MonadicPrettyPrintable m EventSet,
        M.MonadicPrettyPrintable m ValueSet) => 
        M.MonadicPrettyPrintable m CSPOperator where
    prettyPrintBrief (PAlphaParallel _) = M.text "[ || ]"
    prettyPrintBrief (PBuffer bm _ _) = bufferFunction bm
    prettyPrintBrief (PChaos _) = M.text "CHAOS"
    prettyPrintBrief (PException _) = M.text "[| |>"
    prettyPrintBrief PExternalChoice = M.text "[]"
    prettyPrintBrief (PGenParallel _) = M.text "[| |]"
    prettyPrintBrief (PHide _) = M.text "\\"
    prettyPrintBrief PInternalChoice = M.text "|~|"
    prettyPrintBrief PInterrupt = M.text "/\\"
    prettyPrintBrief PInterleave = M.text "|||"
    prettyPrintBrief (PLinkParallel _) = M.text "[ <-> ]"
    prettyPrintBrief (POperator op) = M.prettyPrintBrief op
    prettyPrintBrief (PPrefix _) = M.text "->"
    prettyPrintBrief (PPrefixEventSet _) = M.text "->"
    prettyPrintBrief (PProject _) = M.text "|\\"
    prettyPrintBrief (PRename _) = M.text "[[ ]]"
    prettyPrintBrief (PRun _) = M.text "RUN"
    prettyPrintBrief PSequentialComp = M.text ";"
    prettyPrintBrief PSlidingChoice = M.text "[>"
    prettyPrintBrief (PSynchronisingExternalChoice _) = M.text "[+ +]"
    prettyPrintBrief (PSynchronisingInterrupt _) = M.text "/+ +\\"

    prettyPrint (PAlphaParallel as) =
        M.text "Alphabetised parallel with process alphabets:"
        M.$$ (M.tabIndent $ M.vcat $
            mapM (\ (cid, a) -> M.int cid M.<> M.colon M.<+> M.prettyPrint a)
                (zip [1..] (F.toList as)))
    prettyPrint (PBuffer bm capacity em) =
        M.text "Buffer with capacity" M.<+> M.int capacity
            M.<+> case bm of
                WhenFullExplode ev -> M.text "and when full the buffer performs" M.<+> M.prettyPrint ev
                WhenFullRefuseInputs -> M.text "and when full the buffer refuses new inputs"
        M.$$ M.text "and event pairs:"
        M.$$ (M.tabIndent $ M.vcat $
            mapM (\(ev1, ev2) -> M.parens (M.prettyPrint ev1 M.<> M.comma M.<+> M.prettyPrint ev2))
                (F.toList em))
    prettyPrint (PChaos a) = 
        M.text "CHAOS over:"
        M.$$ M.tabIndent (M.prettyPrint a)
    prettyPrint (PException a) =
        M.text "Exception with event set:"
        M.$$ M.tabIndent (M.prettyPrint a)
    prettyPrint PExternalChoice = M.text "External Choice"
    prettyPrint (PGenParallel a) =
        M.text "Generalised Parallel synchronising:"
        M.$$ M.tabIndent (M.prettyPrint a)
    prettyPrint (PHide a) =
        M.text "Hiding event set:"
        M.$$ M.tabIndent (M.prettyPrint a)
    prettyPrint PInternalChoice = M.text "Internal Choice"
    prettyPrint PInterrupt = M.text "Interrupt"
    prettyPrint PInterleave = M.text "Interleave"
    prettyPrint (PLinkParallel em) =
        M.text "Link Parallel synchronising:"
        M.$$ (M.tabIndent $ M.vcat $
            mapM (\(ev1, ev2) ->
                    M.prettyPrint ev1 M.<+> M.text "<->" M.<+> M.prettyPrint ev2)
                (F.toList em))
    prettyPrint (POperator op) = 
        M.text "Compression using" M.<+> M.prettyPrint op
    prettyPrint (PPrefix ev) = M.text "Prefix" M.<+> M.prettyPrint ev
    prettyPrint (PPrefixEventSet a) =
        M.text "Prefix events:"
        M.$$ M.tabIndent (M.prettyPrint a)
    prettyPrint (PProject a) =
        M.text "Projecting event set:"
        M.$$ M.tabIndent (M.prettyPrint a)
    prettyPrint (PRename em) =
        M.text "Renaming using map:"
        M.$$ (M.tabIndent $ M.vcat $
            mapM (\(ev1, ev2) ->
                    M.prettyPrint ev1 M.<+> M.text "->" M.<+> M.prettyPrint ev2)
                (F.toList em))
    prettyPrint (PRun a) = 
        M.text "RUN over:"
        M.$$ M.tabIndent (M.prettyPrint a)
    prettyPrint PSequentialComp = M.text "Sequential Composition"
    prettyPrint PSlidingChoice = M.text "Sliding Choice"
    prettyPrint (PSynchronisingExternalChoice evs) =
        M.text "Synchronising external choice, synchronising on:"
        M.$$ M.tabIndent (M.prettyPrint evs)
    prettyPrint (PSynchronisingInterrupt evs) =
        M.text "Synchronising interrupt, synchronising on:"
        M.$$ M.tabIndent (M.prettyPrint evs)

instance Precedence Proc where
    precedence (PUnaryOp (PHide _) _) = 10
    precedence (PUnaryOp (PProject _) _) = 10
    precedence (POp PInterleave _) = 9
    precedence (PBinaryOp (PException _) _ _) = 8
    precedence (POp (PAlphaParallel _) _) = 8
    precedence (POp (PGenParallel _) _) = 8
    precedence (PBinaryOp (PLinkParallel _) _ _) = 8
    precedence (POp PInternalChoice _) = 7
    precedence (POp PExternalChoice _) = 6
    precedence (POp (PSynchronisingExternalChoice _) _) = 6
    precedence (PBinaryOp PInterrupt _ _) = 5
    precedence (PBinaryOp (PSynchronisingInterrupt _) _ _) = 5
    precedence (PBinaryOp PSlidingChoice _ _) = 4
    precedence (PBinaryOp PSequentialComp _ _) = 3
    precedence (PUnaryOp (PPrefix _) _) = 2
    precedence (PUnaryOp (PPrefixEventSet _) _) = 2
    precedence (PUnaryOp (PRename _) _) = 1

    precedence (PProcCall _ _) = 0
    precedence (PUnaryOp (POperator _) _) = 0
    precedence (POp (PChaos _) _) = 0
    precedence (POp (PRun _) _) = 0
    precedence (POp (PBuffer {}) _) = 0

ppBinaryOp, ppBriefBinaryOp :: (M.MonadicPrettyPrintable m Proc) => 
    Proc -> m Doc -> Proc -> Proc -> m Doc
ppBriefBinaryOp op opd p1 p2 =
    M.sep (sequence [M.prettyPrintBriefPrec (precedence op) p1,
        opd M.<+> M.prettyPrintBriefPrec (precedence op) p2])
ppBinaryOp = M.ppBinaryOp

maybeNull :: (Applicative m, F.Foldable s, Monad m) => s a -> m Doc -> m Doc
maybeNull s _ | null (F.toList s) = M.text "STOP"
maybeNull _ d = d

maybeNull' :: (Applicative m, F.Foldable s, Monad m) => s a -> m Doc -> m Doc
maybeNull' s _ | null (F.toList s) = M.text "SKIP"
maybeNull' _ d = d

instance
    (M.MonadicPrettyPrintable m EventSet,
        M.MonadicPrettyPrintable m ValueSet) =>
        M.MonadicPrettyPrintable m Proc
    where

    prettyPrint (op@(POp (PAlphaParallel as) ps)) = maybeNull' ps $ 
        case length (F.toList as) of
            1 -> 
                let [p1] = F.toList ps
                    [a1] = F.toList as
                in
                    M.prettyPrintBriefPrec (precedence op) p1 M.<+>
                    M.text "[" M.<> M.prettyPrint a1 M.<+> M.text "|| {}] SKIP"
            2 ->
                let [p1, p2] = F.toList ps
                    [a1, a2] = F.toList as
                in ppBinaryOp op (
                    M.char '[' M.<> M.prettyPrint a1 M.<+>
                    M.text "||" M.<+> M.prettyPrint a2 M.<> M.char ']') p1 p2
            _ ->
                M.text "|| (a, p) :" M.<+> M.braces (M.list (
                    zipWithM (\ a p -> M.parens $ M.sep $ sequence [
                            M.prettyPrint a, M.comma M.<+> M.prettyPrint p]
                    ) (F.toList as) (F.toList ps))) M.<+> M.text "@ [a] p"
    prettyPrint (POp (PBuffer mode@WhenFullRefuseInputs capacity em) _) =
        bufferFunction mode M.<> M.parens (M.int capacity M.<> M.comma 
            M.<+> M.braces (M.list (
                mapM (\ (evOld, evNew) -> M.parens (M.prettyPrint evOld M.<> M.comma M.<+> M.prettyPrint evNew))
                    (F.toList em)
            )))
    prettyPrint (POp (PBuffer mode@(WhenFullExplode ev) capacity em) _) =
        bufferFunction mode M.<> M.parens (M.int capacity M.<> M.comma M.<+> M.prettyPrint ev M.<> M.comma
            M.<+> M.braces (M.list (
                mapM (\ (evOld, evNew) -> M.parens (M.prettyPrint evOld M.<> M.comma M.<+> M.prettyPrint evNew))
                    (F.toList em)
            )))
    prettyPrint (op@(POp (PChaos a) _)) =
        M.text "CHAOS" M.<> M.parens (M.prettyPrint a)
    prettyPrint (op@(PBinaryOp (PException a) p1 p2)) =
        ppBinaryOp op (M.text "[|" M.<+> M.prettyPrint a M.<+> M.text "|>") p1 p2
    prettyPrint (op@(POp PExternalChoice ps)) =
        let flatten (POp PExternalChoice ps) = concatMap flatten (F.toList ps)
            flatten p = [p]
            ps' = flatten (POp PExternalChoice ps)
        in maybeNull ps' $ M.sep (M.punctuateFront (M.text "[] ") $
                    mapM (M.prettyPrintPrec op) ps')
    prettyPrint (op@(POp (PGenParallel a) ps)) = maybeNull' ps $ 
        M.sep (M.punctuateFront
                (M.text "[|" M.<+> M.prettyPrint a M.<+> M.text "|] ")
                (mapM (M.prettyPrintPrec op) (F.toList ps)))
    prettyPrint (op@(PUnaryOp (PHide a) p)) =
        M.prettyPrintPrec op p
        M.<+> M.char '\\' M.<+> M.prettyPrint a
    prettyPrint (op@(POp PInternalChoice ps)) =
        let flatten (POp PInternalChoice ps) = concatMap flatten (F.toList ps)
            flatten p = [p]
            ps' = flatten (POp PInternalChoice ps)
        in M.sep (M.punctuateFront (M.text "|~| ") $
                mapM (M.prettyPrintPrec op) ps')
    prettyPrint (op@(POp PInterleave ps)) = maybeNull ps $ 
        M.sep (M.punctuateFront (M.text "||| ") $
            mapM (M.prettyPrintPrec op) $ F.toList ps)
    prettyPrint (op@(PBinaryOp PInterrupt p1 p2)) =
        ppBinaryOp op (M.text "/\\") p1 p2
    prettyPrint (op@(PBinaryOp (PLinkParallel evm) p1 p2)) =
        M.prettyPrintPrec op p1 M.<+> M.text "[" M.<>
            M.list (mapM (\(evLeft, evRight) -> 
                            M.prettyPrint evLeft M.<+> M.text "<->" 
                                M.<+> M.prettyPrint evRight) $ F.toList evm)
        M.<> M.text "]" M.<+> M.prettyPrintPrec op p2
    prettyPrint (op@(PUnaryOp (POperator cop) p)) = 
        ppOperatorWithArg cop (M.prettyPrint p)
    prettyPrint (op@(PUnaryOp (PPrefix e) p)) =
        M.prettyPrint e M.<+> M.text "->"
        M.<+> M.prettyPrintPrec op p
    prettyPrint (op@(PUnaryOp (PPrefixEventSet e) p)) =
        M.prettyPrint e M.<+> M.text "->"
        M.<+> M.prettyPrintPrec op p
    prettyPrint (op@(PUnaryOp (PProject a) p)) =
        M.prettyPrintPrec op p
        M.<+> M.text "|\\" M.<+> M.prettyPrint a
    prettyPrint (op@(PUnaryOp (PRename evm) p)) =
        M.prettyPrintPrec op p M.<> M.text "[[" 
        M.<> M.list (mapM (\ (evOld, evNew) -> 
                            M.prettyPrint evOld M.<+> M.text "<-" 
                            M.<+> M.prettyPrint evNew) $ F.toList evm) 
        M.<> M.text "]]"
    prettyPrint (op@(POp (PRun a) _)) =
        M.text "RUN" M.<> M.parens (M.prettyPrint a)
    prettyPrint (op@(PBinaryOp PSequentialComp p1 p2)) =
        ppBinaryOp op (M.char ';') p1 p2
    prettyPrint (op@(PBinaryOp PSlidingChoice p1 p2)) =
        ppBinaryOp op (M.text "[>") p1 p2
    prettyPrint (PProcCall n _) = M.prettyPrint n
    prettyPrint (op@(POp (PSynchronisingExternalChoice alpha) ps)) =
        let ps' = F.toList ps
        in maybeNull ps' $ M.sep (M.punctuateFront
            (M.text "[+" M.<> M.prettyPrint alpha M.<> M.text "+] ") $
            mapM (M.prettyPrintPrec op) ps')
    prettyPrint (op@(PBinaryOp (PSynchronisingInterrupt es) p1 p2)) =
        ppBinaryOp op (M.text "/+" M.<+> M.prettyPrint es M.<+> M.text "+\\")
            p1 p2

    prettyPrintBrief (op@(POp (PAlphaParallel as) ps)) = maybeNull' ps $ 
        if length (F.toList as) == 1 then
            let [p] = F.toList ps in
            M.prettyPrintBriefPrec (precedence op) p M.<+> M.text "[…||…] SKIP"
        else M.sep (M.punctuateFront (M.text "[…||…] ")
                (mapM (M.prettyPrintBriefPrec (precedence op)) (F.toList ps)))
    prettyPrintBrief (POp (PBuffer m@WhenFullRefuseInputs capacity em) _) =
        bufferFunction m M.<> M.parens (M.int capacity M.<> M.comma M.<+> M.ellipsis)
    prettyPrintBrief (POp (PBuffer m@(WhenFullExplode ev) capacity em) _) =
         bufferFunction m M.<> M.parens (M.int capacity M.<> M.comma M.<+> M.prettyPrintBrief ev
                                            M.<> M.comma M.<+> M.ellipsis)
    prettyPrintBrief (op@(POp (PChaos _) _)) =
        M.text "CHAOS" M.<> M.parens (M.ellipsis)
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
        ppBriefOperatorWithArg cop (M.prettyPrintBriefPrec 100 p)
    prettyPrintBrief (op@(PUnaryOp (PPrefix e) p)) =
        M.prettyPrintBrief e M.<+> M.text "->" M.<+> M.ellipsis
    prettyPrintBrief (op@(PUnaryOp (PPrefixEventSet e) p)) =
        M.braces M.ellipsis M.<+> M.text "->" M.<+> M.ellipsis
        --M.<+> M.prettyPrintBriefPrec (precedence op) p
    prettyPrintBrief (op@(PUnaryOp (PProject a) p)) =
        M.prettyPrintBriefPrec (precedence op) p
        M.<+> M.text "|\\" M.<+> M.braces M.ellipsis
    prettyPrintBrief (op@(PUnaryOp (PRename evm) p)) =
        M.prettyPrintBriefPrec (precedence op) p M.<> M.text "[[…]]"
    prettyPrintBrief (op@(POp (PRun _) _)) =
        M.text "RUN" M.<> M.parens (M.ellipsis)
    prettyPrintBrief (op@(PBinaryOp PSequentialComp p1 p2)) =
        ppBriefBinaryOp op (M.char ';') p1 p2
    prettyPrintBrief (op@(PBinaryOp PSlidingChoice p1 p2)) =
        ppBriefBinaryOp op (M.text "[>") p1 p2
    prettyPrintBrief (op@(POp (PSynchronisingExternalChoice alpha) ps)) =
        let ps' = F.toList ps
        in maybeNull ps' $ M.sep (M.punctuateFront (M.text "[+…+] ") $
                mapM (M.prettyPrintBriefPrec (precedence op)) ps')
    prettyPrintBrief (op@(PBinaryOp (PSynchronisingInterrupt es) p1 p2)) =
        ppBriefBinaryOp op (M.text "/+…+\\") p1 p2
    prettyPrintBrief (PProcCall n _) = M.prettyPrintBrief n

instance (Applicative m, Monad m,
        M.MonadicPrettyPrintable m TCExp,
        M.MonadicPrettyPrintable m Proc,
        M.MonadicPrettyPrintable m ProcOperator,
        M.MonadicPrettyPrintable m ValueSet,
        M.MonadicPrettyPrintable m EventSet) =>
        M.MonadicPrettyPrintable m Value where
    prettyPrint (VInt i) = M.int i
    prettyPrint (VChar c) = M.quotes (M.char c)
    prettyPrint (VBool True) = M.text "true"
    prettyPrint (VBool False) = M.text "false"
    prettyPrint (VTuple vs) = M.parens (M.list $ mapM M.prettyPrint (elems vs))
    prettyPrint (VDot vs) = M.dotSep (mapM M.prettyPrint vs)
    prettyPrint (VChannel n) = M.prettyPrint n
    prettyPrint (VDataType n) = M.prettyPrint n
    prettyPrint (VList (vs@((VChar _) : _))) =
        M.doubleQuotes (M.text (map (\ (VChar c) -> c) vs))
    prettyPrint (VList vs) = M.angles (M.list $ mapM M.prettyPrint vs)
    prettyPrint (VSet s) = M.prettyPrint s
    prettyPrint (VMap m) =
        M.text "(|" M.<+> M.list (mapM
            (\ (k,v) -> M.prettyPrint k M.<+> M.text "=>" M.<+> M.prettyPrint v)
            (Mp.toList m))
        M.<+> M.text "|)"
    prettyPrint (VFunction fid _) = M.prettyPrint fid
    prettyPrint (VProc p) = M.prettyPrint p
    prettyPrint (VThunk th) = M.text "<thunk>"

    prettyPrintBrief (VSet s) = M.braces M.ellipsis
    prettyPrintBrief (VMap m) = M.text "(|" M.<+> M.ellipsis M.<+> M.text "|)"
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
    prettyPrint (Powerset vs) = M.text "Set" M.<> M.parens (M.prettyPrint vs)
    prettyPrint (s@(AllMaps ks vs)) =
        M.braces (M.list (mapM M.prettyPrint (toList s)))
    prettyPrint s = do
        -- Try and compress
        mvs <- compressIntoEnumeratedSet s
        case mvs of
            Just vs ->
                -- Compression succesful
                M.text "{|" M.<> M.list (mapM M.prettyPrint vs) M.<> M.text "|}"
            Nothing -> 
                case s of
                    CartesianProduct vss CartDot ->
                        M.hcat (M.punctuate (M.text ".") (mapM M.prettyPrint vss))
                    _ ->
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
    prettyPrint (Powerset vs) = M.text "Set" M.<> M.parens (M.prettyPrint vs)
    prettyPrint (s@(AllMaps ks vs)) =
        M.braces (M.list (mapM M.prettyPrint (toList s)))

-- | Pretty prints the given process and all processes that it depends upon.
prettyPrintAllRequiredProcesses :: Proc -> Doc
prettyPrintAllRequiredProcesses p =
    let
        (pInit, namedPs') = splitProcIntoComponents p
        stopName = head [name b | b <- builtins False, stringName b == "STOP"]
        isStop (InstantiatedFrame _ frame@(BuiltinFunctionFrame {}) _ _) =
            builtinFunctionFrameFunctionName frame == stopName
        isStop _ = False
        namedPs = filter (\ (ProcName s, _) -> not (isStop s)) namedPs'
        ppNamedProc (n,p) =
            hang (prettyPrint n <+> char '=') tabWidth (prettyPrint p)
    in vcat (punctuate (char '\n') ((map ppNamedProc namedPs)++[prettyPrint pInit]))
