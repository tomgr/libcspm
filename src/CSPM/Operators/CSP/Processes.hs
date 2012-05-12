{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
module CSPM.Operators.CSP.Processes (
    CSPOperator(..),
    UnCompiledCSPProc, UnCompiledCSPOp(..),
) where

import CSPM.Compiler.Events
import CSPM.Compiler.Processes
import CSPM.Evaluator.Values
import qualified Data.Foldable as F
import qualified Data.Functor as F
import qualified Data.Sequence as S
import Util.PrettyPrint

type UnCompiledCSPProc = Proc S.Seq UnCompiledCSPOp (ProcName UnCompiledCSPOp)
newtype UnCompiledCSPOp = UCSPOp {
        uncompiledCSPOp :: CSPOperator S.Seq 
                            (Event UnCompiledCSPOp) 
                            (S.Seq (Event UnCompiledCSPOp)) 
                            (S.Seq (Event UnCompiledCSPOp, Event UnCompiledCSPOp))
    }

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

slist :: S.Seq Doc -> Doc
slist s = list (F.toList s)

instance PrettyPrintable UnCompiledCSPProc where
    prettyPrint (POp (UCSPOp (PAlphaParallel as)) ps) =
        text "||" <+> braces (slist (S.zipWith (\ a p -> 
            parens (prettyPrint a <> char ',' <+> prettyPrint p)) as ps))
    prettyPrint (PBinaryOp (UCSPOp (PException a)) p1 p2) =
        prettyPrint p1 <+> text "[|" <> prettyPrint a <> text "|>" 
            <+> prettyPrint p2
    prettyPrint (POp (UCSPOp PExternalChoice) ps) =
        let flatten (POp (UCSPOp PExternalChoice) ps) = F.msum (F.fmap flatten ps)
            flatten p = S.singleton p
            ps' = flatten (POp (UCSPOp PExternalChoice) ps)
        in sep (punctuateFront (text "[] ") (map prettyPrint $ F.toList ps'))
    prettyPrint (POp (UCSPOp (PGenParallel a)) ps) =
        text "||" <+> brackets (prettyPrint a) 
                <+> braces (list (map prettyPrint $ F.toList ps))
    prettyPrint (PUnaryOp (UCSPOp (PHide a)) p) =
        prettyPrint p <+> char '\\' <+> prettyPrint a
    prettyPrint (POp (UCSPOp PInternalChoice) ps) =
        let flatten (POp (UCSPOp PInternalChoice) ps) = F.msum (F.fmap flatten ps)
            flatten p = S.singleton p
            ps' = flatten (POp (UCSPOp PInternalChoice) ps)
        in sep (punctuateFront (text "|~| ") (map prettyPrint $ F.toList ps'))
    prettyPrint (POp (UCSPOp PInterleave) ps) =
        sep (punctuateFront (text "||| ") (map prettyPrint $ F.toList ps))
    prettyPrint (PBinaryOp (UCSPOp (PLinkParallel evm)) p1 p2) =
        prettyPrint p1 <+> text "[" <>
            list (map (\(evLeft, evRight) -> prettyPrint evLeft <+> text "<-" 
                                        <+> prettyPrint evRight) $ F.toList evm)
        <> text "]" <+> prettyPrint p2
    prettyPrint (PUnaryOp (UCSPOp (POperator op)) p) = 
        prettyPrint op <> parens (prettyPrint p)
    prettyPrint (PUnaryOp (UCSPOp (PPrefix e)) p) =
        prettyPrint e <+> text "->" <+> prettyPrint p
    prettyPrint (PUnaryOp (UCSPOp (PRename evm)) p) =
        prettyPrint p <> text "[[" 
        <> list (map (\ (evOld, evNew) -> 
                            prettyPrint evOld <+> text "<-" 
                            <+> prettyPrint evNew) $ F.toList evm) 
        <> text "]]"
    prettyPrint (PBinaryOp (UCSPOp PSequentialComp) p1 p2) =
        prettyPrint p1 <+> text "->" <+> prettyPrint p2
    prettyPrint (PBinaryOp (UCSPOp PSlidingChoice) p1 p2) =
        prettyPrint p1 <+> text "|>" <+> prettyPrint p2
    prettyPrint (PProcCall n _) = prettyPrint n
