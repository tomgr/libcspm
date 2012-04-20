{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | This module provides the input data structure to the compiler.
module CSPM.Compiler.Processes (
    Proc(..), UnCompiledProc,
    CSPOperator(..),
    ProcOperator(..), 
    ProcName(..),
    --prettyPrintAllRequiredProcesses,
) where

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

type UnCompiledProc = 
    Proc [] CSPOperator ProcName Event [Event] [(Event, Event)]

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
    | POp (op seq ev evs evm) [Proc seq op pn ev evs evm]
    -- | Labels the process this contains. This allows infinite loops to be
    -- spotted.
    | PProcCall pn (Proc seq op pn ev evs evm)

instance PrettyPrintable UnCompiledProc where
    prettyPrint (POp (PAlphaParallel as) ps) =
        text "||" <+> braces (list (zipWith (\ a p -> 
            parens (prettyPrint a <> char ',' <+> prettyPrint p)) as ps))
    prettyPrint (PBinaryOp (PException a) p1 p2) =
        prettyPrint p1 <+> text "[|" <> prettyPrint a <> text "|>" 
            <+> prettyPrint p2
    prettyPrint (POp PExternalChoice ps) =
        sep (punctuateFront (text "[] ") (map prettyPrint ps))
    prettyPrint (POp (PGenParallel a) ps) =
        text "||" <+> brackets (prettyPrint a) 
                <+> braces (list (map prettyPrint ps))
    prettyPrint (PUnaryOp (PHide a) p) =
        prettyPrint p <+> char '\\' <+> prettyPrint a
    prettyPrint (POp PInternalChoice ps) =
        sep (punctuateFront (text "|~| ") (map prettyPrint ps))
    prettyPrint (POp PInterleave ps) =
        sep (punctuateFront (text "||| ") (map prettyPrint ps))
    prettyPrint (PBinaryOp (PLinkParallel evm) p1 p2) =
        prettyPrint p1 <+> text "[" <>
            list (map (\(evLeft, evRight) -> prettyPrint evLeft <+> text "<-" 
                                        <+> prettyPrint evRight) evm)
        <> text "]" <+> prettyPrint p2
    prettyPrint (PUnaryOp (POperator op) p) = 
        prettyPrint op <> parens (prettyPrint p)
    prettyPrint (PUnaryOp (PPrefix e) p) =
        prettyPrint e <+> text "->" <+> prettyPrint p
    prettyPrint (PUnaryOp (PRename evm) p) =
        prettyPrint p <> text "[[" 
        <> list (map (\ (evOld, evNew) -> 
                            prettyPrint evOld <+> text "<-" 
                            <+> prettyPrint evNew) evm) 
        <> text "]]"
    prettyPrint (PBinaryOp PSequentialComp p1 p2) =
        prettyPrint p1 <+> text "->" <+> prettyPrint p2
    prettyPrint (PBinaryOp PSlidingChoice p1 p2) =
        prettyPrint p1 <+> text "|>" <+> prettyPrint p2
    prettyPrint (PProcCall n _) = prettyPrint n

instance Show UnCompiledProc where
    show p = show (prettyPrint p)

---- | Given a process, returns the initial process and all processes that it
---- calls.
--splitProcIntoComponents :: UnCompiledProc -> (UnCompiledProc, [(ProcName, UnCompiledProc)])
--splitProcIntoComponents p =
--    let
--        explored pns n = n `elem` (map fst pns)

--        exploreAll :: [(ProcName, UnCompiledProc)] -> [UnCompiledProc] -> [(ProcName, UnCompiledProc)]
--        exploreAll pns [] = pns
--        exploreAll pns (p:ps) = exploreAll (explore pns p) ps

--        explore :: [(ProcName, UnCompiledProc)] -> UnCompiledProc -> [(ProcName, UnCompiledProc)]
--        explore pns (PAlphaParallel as ps) = exploreAll pns ps
--        explore pns (PException p1 _ p2) = exploreAll pns [p1, p2]
--        explore pns (PExternalChoice ps) = exploreAll pns ps
--        explore pns (PGenParallel _ ps) = exploreAll pns ps
--        explore pns (PHide p _) = explore pns p
--        explore pns (PInternalChoice ps) = exploreAll pns ps
--        explore pns (PInterrupt p1 p2) = exploreAll pns [p1, p2]
--        explore pns (PInterleave ps) = exploreAll pns ps
--        explore pns (PLinkParallel p1 _ p2) = exploreAll pns [p1, p2]
--        explore pns (POperator _ p) = explore pns p
--        explore pns (PPrefix _ p) = explore pns p
--        explore pns (PRename _ p) = explore pns p
--        explore pns (PSequentialComp p1 p2) = exploreAll pns [p1, p2]
--        explore pns (PSlidingChoice p1 p2) = exploreAll pns [p1, p2]
--        explore pns (PProcCall n p) =
--            if explored pns n then pns
--            else explore ((n, p):pns) p
--    in (p, explore [] p)

---- | Pretty prints the given process and all processes that it depends upon.
--prettyPrintAllRequiredProcesses :: UnCompiledProc -> Doc
--prettyPrintAllRequiredProcesses p =
--    let
--        (pInit, namedPs) = splitProcIntoComponents p
--        ppNamedProc (n,p) =
--            hang (prettyPrint n <+> char '=') tabWidth (prettyPrint p)
--    in 
--        vcat (punctuate (char '\n') ((map ppNamedProc namedPs)++[prettyPrint pInit]))
