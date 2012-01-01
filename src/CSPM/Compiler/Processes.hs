-- | This module provides the input data structure to the compiler.
module CSPM.Compiler.Processes (
    Proc(..), 
    ProcOperator(..), 
    ProcName(..),
    prettyPrintAllRequiredProcesses,
) where

import qualified CSPM.Compiler.Map as M
import qualified CSPM.Compiler.Set as S
import CSPM.Compiler.Events
import CSPM.DataStructures.Names
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.PrettyPrint

-- | ProcNames uniquely identify processes.
data ProcName = ProcName {
        -- | The name of this process (recal Name s are unique).
        name :: Name,
        -- | The arguments applied to this process, in case it was a function
        -- call.
        arguments :: [[Value]]
    }

instance Eq ProcName where
    pn1 == pn2 = name pn1 == name pn2 && arguments pn1 == arguments pn2
instance PrettyPrintable ProcName where
    prettyPrint (ProcName n args) =
        prettyPrint n
        <> hcat (map (\as -> parens (list (map prettyPrint as))) args)
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

-- | A compiled process. Note this is an infinite data structure (due to
-- PProcCall) as this makes compilation easy (we can easily chase
-- dependencies).
data Proc =
    PAlphaParallel [(S.Set Event, Proc)]
    | PException Proc (S.Set Event) Proc
    | PExternalChoice [Proc]
    | PGenParallel (S.Set Event) [Proc]
    | PHide Proc (S.Set Event)
    | PInternalChoice [Proc]
    | PInterrupt Proc Proc
    | PInterleave [Proc]
    -- Map from event of left process, to event of right that it synchronises
    -- with. (Left being p1, Right being p2 ps ps).
    | PLinkParallel Proc (M.Map Event Event) Proc
    | POperator ProcOperator Proc
    | PPrefix Event Proc
    -- Map from Old -> New event
    | PRename (M.Relation Event Event) Proc
    | PSequentialComp Proc Proc
    | PSlidingChoice Proc Proc
    -- | Labels the process this contains. This allows infinite loops to be
    -- spotted.
    | PProcCall ProcName Proc

instance PrettyPrintable Proc where
    prettyPrint (PAlphaParallel aps) =
        text "||" <+> braces (list (map (\ (a,p) -> 
            parens (prettyPrint a <> char ',' <+> prettyPrint p)) aps))
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

instance Show Proc where
    show p = show (prettyPrint p)

-- | Given a process, returns the initial process and all processes that it
-- calls.
splitProcIntoComponents :: Proc -> (Proc, [(ProcName, Proc)])
splitProcIntoComponents p =
    let
        explored pns n = n `elem` (map fst pns)

        exploreAll :: [(ProcName, Proc)] -> [Proc] -> [(ProcName, Proc)]
        exploreAll pns [] = pns
        exploreAll pns (p:ps) = exploreAll (explore pns p) ps

        explore :: [(ProcName, Proc)] -> Proc -> [(ProcName, Proc)]
        explore pns (PAlphaParallel aps) = exploreAll pns ps
            where ps = map snd aps
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
prettyPrintAllRequiredProcesses :: Proc -> Doc
prettyPrintAllRequiredProcesses p =
    let
        (pInit, namedPs) = splitProcIntoComponents p
        ppNamedProc (n,p) =
            hang (prettyPrint n <+> char '=') tabWidth (prettyPrint p)
    in 
        vcat (punctuate (char '\n') ((map ppNamedProc namedPs)++[prettyPrint pInit]))
