{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | This module provides the input data structure to the compiler.
module CSPM.Compiler.Processes (
    Proc(..), UnCompiledProc,
    ProcOperator(..), 
    ProcName(..),
    prettyPrintAllRequiredProcesses,
) where

import qualified CSPM.Compiler.Map as M
import qualified CSPM.Compiler.Set as S
import CSPM.Compiler.Events
import CSPM.DataStructures.Names
import {-# SOURCE #-} CSPM.Evaluator.Values
import Data.Hashable
import Util.PrettyPrint

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

type UnCompiledProc = Proc ProcName Event [Event] [(Event, Event)]

-- | A compiled process. Note this is an infinite data structure (due to
-- PProcCall) as this makes compilation easy (we can easily chase
-- dependencies).
data Proc pn ev evs evm =
    PAlphaParallel [evs] [Proc pn ev evs evm]
    | PException (Proc pn ev evs evm) evs (Proc pn ev evs evm)
    | PExternalChoice [Proc pn ev evs evm]
    | PGenParallel (evs) [Proc pn ev evs evm]
    | PHide (Proc pn ev evs evm) (evs)
    | PInternalChoice [Proc pn ev evs evm]
    | PInterrupt (Proc pn ev evs evm) (Proc pn ev evs evm)
    | PInterleave [Proc pn ev evs evm]
    -- Map from event of left process, to event of right that it synchronises
    -- with. (Left being p1, Right being p2 ps ps).
    | PLinkParallel (Proc pn ev evs evm) evm (Proc pn ev evs evm)
    | POperator ProcOperator (Proc pn ev evs evm)
    | PPrefix ev (Proc pn ev evs evm)
    -- Map from Old -> New event
    | PRename evm (Proc pn ev evs evm)
    | PSequentialComp (Proc pn ev evs evm) (Proc pn ev evs evm)
    | PSlidingChoice (Proc pn ev evs evm) (Proc pn ev evs evm)
    -- | Labels the process this contains. This allows infinite loops to be
    -- spotted.
    | PProcCall pn (Proc pn ev evs evm)

instance PrettyPrintable UnCompiledProc where
    prettyPrint (PAlphaParallel as ps) =
        text "||" <+> braces (list (zipWith (\ a p -> 
            parens (prettyPrint a <> char ',' <+> prettyPrint p)) as ps))
    prettyPrint (PException p1 a p2) =
        prettyPrint p1 <+> text "[|" <> prettyPrint a <> text "|>" 
            <+> prettyPrint p2
    prettyPrint (PExternalChoice ps) =
        sep (punctuateFront (text "[] ") (map prettyPrint ps))
    prettyPrint (PGenParallel a ps) =
        text "||" <+> brackets (prettyPrint a) 
                <+> braces (list (map prettyPrint ps))
    prettyPrint (PHide p a) =
        prettyPrint p <+> char '\\' <+> prettyPrint a
    prettyPrint (PInternalChoice ps) =
        sep (punctuateFront (text "|~| ") (map prettyPrint ps))
    prettyPrint (PInterleave ps) =
        sep (punctuateFront (text "||| ") (map prettyPrint ps))
    prettyPrint (PLinkParallel p1 evm p2) =
        prettyPrint p1 <+> text "[" <>
            list (map (\(evLeft, evRight) -> prettyPrint evLeft <+> text "<-" 
                                        <+> prettyPrint evRight) evm)
        <> text "]" <+> prettyPrint p2
    prettyPrint (POperator op p) = 
        prettyPrint op <> parens (prettyPrint p)
    prettyPrint (PPrefix e p) =
        prettyPrint e <+> text "->" <+> prettyPrint p
    prettyPrint (PRename evm p) =
        prettyPrint p <> text "[[" 
        <> list (map (\ (evOld, evNew) -> 
                            prettyPrint evOld <+> text "<-" 
                            <+> prettyPrint evNew) evm) 
        <> text "]]"
    prettyPrint (PSequentialComp p1 p2) =
        prettyPrint p1 <+> text "->" <+> prettyPrint p2
    prettyPrint (PSlidingChoice p1 p2) =
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
        explore pns (PAlphaParallel as ps) = exploreAll pns ps
        explore pns (PException p1 _ p2) = exploreAll pns [p1, p2]
        explore pns (PExternalChoice ps) = exploreAll pns ps
        explore pns (PGenParallel _ ps) = exploreAll pns ps
        explore pns (PHide p _) = explore pns p
        explore pns (PInternalChoice ps) = exploreAll pns ps
        explore pns (PInterrupt p1 p2) = exploreAll pns [p1, p2]
        explore pns (PInterleave ps) = exploreAll pns ps
        explore pns (PLinkParallel p1 _ p2) = exploreAll pns [p1, p2]
        explore pns (POperator _ p) = explore pns p
        explore pns (PPrefix _ p) = explore pns p
        explore pns (PRename _ p) = explore pns p
        explore pns (PSequentialComp p1 p2) = exploreAll pns [p1, p2]
        explore pns (PSlidingChoice p1 p2) = exploreAll pns [p1, p2]
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
