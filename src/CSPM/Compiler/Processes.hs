{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings #-}
-- | This module provides the input data structure to the compiler.
module CSPM.Compiler.Processes (
    Proc(..), UnCompiledProc,
    CSPOperator(..),
    ProcOperator(..), 
    ProcName(..),
    operator, components,
    prettyPrintAllRequiredProcesses,
) where

import CSPM.Compiler.Events
import CSPM.DataStructures.Names
import {-# SOURCE #-} CSPM.Evaluator.Values
import qualified Data.Foldable as F
import qualified Data.Functor as F
import qualified Data.Sequence as S
import Data.Hashable
import Util.PrettyPrint
import qualified Util.TextPrettyPrint as T

-- | ProcNames uniquely identify processes.
data ProcName =
    ProcName {
        -- | The name of this process (recal Name s are unique).
        name :: Name,
        -- | The arguments applied to this process, in case it was a function
        -- call.
        arguments :: [[Value]],
        -- | The parent of this proc name. This is used in let expressions.
        parent :: Maybe ProcName
    }
    -- | A proccess that has no name, but needs disambgiuation. These are
    -- used in prefixing to avoid problems with things like:
    -- P = c?x -> (let Q = ... within Q) as the ... can depend on x.
    | AnnonymousProcName {
        arguments :: [[Value]],
        parent :: Maybe ProcName
    }
    deriving Eq

instance Hashable ProcName where
    hash (ProcName n vss p) = combine 1 (combine (hash n) (combine (hash vss) (hash p)))
    hash (AnnonymousProcName as ps) = combine 2 (combine (hash as) (hash ps))
instance PrettyPrintable ProcName where
    prettyPrint (ProcName n args Nothing) =
        prettyPrint n
        <> hcat (map (\as -> parens (list (map prettyPrint as))) args)
    prettyPrint (ProcName n args (Just pn)) =
        prettyPrint pn <> colon<>colon <> prettyPrint (ProcName n args Nothing)
    prettyPrint (AnnonymousProcName args Nothing) =
        text "ANNON"
        <> hcat (map (\as -> parens (list (map prettyPrint as))) args)
    prettyPrint (AnnonymousProcName args (Just pn)) =
        prettyPrint pn <> colon<>colon <> prettyPrint (AnnonymousProcName args Nothing)
instance Show ProcName where
    show pn = show (prettyPrint pn)

instance T.FastPrettyPrintable ProcName where
    toBuilder (ProcName n args Nothing) =
        T.toBuilder n
        T.<> T.hcat (map (\as -> T.parens (T.list (map T.toBuilder as))) args)
    toBuilder (ProcName n args (Just pn)) =
        T.toBuilder n T.<> T.stext "::" T.<> T.toBuilder (ProcName n args Nothing)
    toBuilder (AnnonymousProcName args Nothing) =
        T.stext "ANNON"
        T.<> T.hcat (map (\as -> T.parens (T.list (map T.toBuilder as))) args)
    toBuilder (AnnonymousProcName args (Just pn)) =
        T.toBuilder pn T.<> T.stext "::" T.<> T.toBuilder (AnnonymousProcName args Nothing)

-- | An operator that can be applied to processes.
data ProcOperator =
    Chase 
    | Diamond 
    | Explicate 
    | Normalize 
    | ModelCompress
    | StrongBisim 
    | TauLoopFactor 
    | WeakBisim
    deriving (Eq)

instance PrettyPrintable ProcOperator where
    prettyPrint Chase = text "chase"
    prettyPrint Diamond = text "diamond"
    prettyPrint Explicate = text "explicate"
    prettyPrint Normalize = text "normal"
    prettyPrint ModelCompress = text "model_compress"
    prettyPrint StrongBisim = text "sbisim"
    prettyPrint TauLoopFactor = text "tau_loop_factor"
    prettyPrint WeakBisim = text "wbisim"

instance Show ProcOperator where
    show p = show (prettyPrint p)

type UnCompiledProc = 
    Proc S.Seq CSPOperator ProcName Event (S.Seq Event) (S.Seq (Event, Event))

data CSPOperator seq ev evs evm =
    PAlphaParallel (seq evs)
    | PException evs
    | PExternalChoice
    | PGenParallel evs
    | PHide evs
    | PInternalChoice
    | PInterrupt
    | PInterleave
    -- Map from event of left process, to event of right that it synchronises
    -- with. (Left being p1, Right being p2 ps ps).
    | PLinkParallel evm
    | POperator ProcOperator
    | PPrefix ev
    -- Map from Old -> New event
    | PRename evm
    | PSequentialComp
    | PSlidingChoice

-- | A compiled process. Note this is an infinite data structure (due to
-- PProcCall) as this makes compilation easy (we can easily chase
-- dependencies).
data Proc seq op pn ev evs evm = 
    PUnaryOp (op seq ev evs evm) (Proc seq op pn ev evs evm)
    | PBinaryOp (op seq ev evs evm) (Proc seq op pn ev evs evm) (Proc seq op pn ev evs evm)
    | POp (op seq ev evs evm) (seq (Proc seq op pn ev evs evm))
    -- | Labels the process this contains. This allows infinite loops to be
    -- spotted.
    | PProcCall pn (Proc seq op pn ev evs evm)

-- | Gives the operator of a process. If the process is a ProcCall an error is
-- thrown.
operator :: Proc seq op pn ev evs evm -> op seq ev evs evm
operator (PUnaryOp op _) = op
operator (PBinaryOp op _ _)=  op
operator (POp op _) = op

-- | Returns the components of a given process.
components :: Proc S.Seq op pn ev evs evm -> S.Seq (Proc S.Seq op pn ev evs evm)
components (PBinaryOp _ p1 p2) = p1 S.<| p2 S.<| S.empty
components (POp _ ps) = ps
components (PUnaryOp _ p1) = S.singleton p1

slist :: S.Seq Doc -> Doc
slist s = list (F.toList s)

instance PrettyPrintable UnCompiledProc where
    prettyPrint (POp (PAlphaParallel as) ps) =
        text "||" <+> braces (slist (S.zipWith (\ a p -> 
            parens (prettyPrint a <> char ',' <+> prettyPrint p)) as ps))
    prettyPrint (PBinaryOp (PException a) p1 p2) =
        prettyPrint p1 <+> text "[|" <> prettyPrint a <> text "|>" 
            <+> prettyPrint p2
    prettyPrint (POp PExternalChoice ps) =
        let flatten (POp PExternalChoice ps) = F.msum (F.fmap flatten ps)
            flatten p = S.singleton p
            ps' = flatten (POp PExternalChoice ps)
        in sep (punctuateFront (text "[] ") (map prettyPrint $ F.toList ps'))
    prettyPrint (POp (PGenParallel a) ps) =
        text "||" <+> brackets (prettyPrint a) 
                <+> braces (list (map prettyPrint $ F.toList ps))
    prettyPrint (PUnaryOp (PHide a) p) =
        prettyPrint p <+> char '\\' <+> prettyPrint a
    prettyPrint (POp PInternalChoice ps) =
        let flatten (POp PInternalChoice ps) = F.msum (F.fmap flatten ps)
            flatten p = S.singleton p
            ps' = flatten (POp PInternalChoice ps)
        in sep (punctuateFront (text "|~| ") (map prettyPrint $ F.toList ps'))
    prettyPrint (POp PInterleave ps) =
        sep (punctuateFront (text "||| ") (map prettyPrint $ F.toList ps))
    prettyPrint (PBinaryOp (PLinkParallel evm) p1 p2) =
        prettyPrint p1 <+> text "[" <>
            list (map (\(evLeft, evRight) -> prettyPrint evLeft <+> text "<-" 
                                        <+> prettyPrint evRight) $ F.toList evm)
        <> text "]" <+> prettyPrint p2
    prettyPrint (PUnaryOp (POperator op) p) = 
        prettyPrint op <> parens (prettyPrint p)
    prettyPrint (PUnaryOp (PPrefix e) p) =
        prettyPrint e <+> text "->" <+> prettyPrint p
    prettyPrint (PUnaryOp (PRename evm) p) =
        prettyPrint p <> text "[[" 
        <> list (map (\ (evOld, evNew) -> 
                            prettyPrint evOld <+> text "<-" 
                            <+> prettyPrint evNew) $ F.toList evm) 
        <> text "]]"
    prettyPrint (PBinaryOp PSequentialComp p1 p2) =
        prettyPrint p1 <+> text "->" <+> prettyPrint p2
    prettyPrint (PBinaryOp PSlidingChoice p1 p2) =
        prettyPrint p1 <+> text "|>" <+> prettyPrint p2
    prettyPrint (PProcCall n _) = prettyPrint n

instance Show UnCompiledProc where
    show p = show (prettyPrint p)

-- | Given a process, returns the initial process and all processes that it
-- calls.
splitProcIntoComponents :: UnCompiledProc -> (UnCompiledProc, [(ProcName, UnCompiledProc)])
splitProcIntoComponents p =
    let
        explored pns n = n `elem` (map fst pns)

        exploreAll :: [(ProcName, UnCompiledProc)] -> [UnCompiledProc] -> [(ProcName, UnCompiledProc)]
        exploreAll pns [] = pns
        exploreAll pns (p:ps) = exploreAll (explore pns p) ps

        explore :: [(ProcName, UnCompiledProc)] -> UnCompiledProc -> [(ProcName, UnCompiledProc)]
        explore pns (PUnaryOp _ p) = explore pns p
        explore pns (PBinaryOp _ p1 p2) = exploreAll pns [p1,p2]
        explore pns (POp _ ps) = exploreAll pns (F.toList ps)
        explore pns (PProcCall n p) =
            if explored pns n then pns
            else explore ((n, p):pns) p
    in (p, explore [] p)

-- | Pretty prints the given process and all processes that it depends upon.
prettyPrintAllRequiredProcesses :: UnCompiledProc -> Doc
prettyPrintAllRequiredProcesses p =
    let
        (pInit, namedPs) = splitProcIntoComponents p
        ppNamedProc (n,p) =
            hang (prettyPrint n <+> char '=') tabWidth (prettyPrint p)
    in 
        vcat (punctuate (char '\n') ((map ppNamedProc namedPs)++[prettyPrint pInit]))
