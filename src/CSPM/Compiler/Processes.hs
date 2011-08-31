module CSPM.Compiler.Processes (
    Proc(..), ProcName
) where

import CSPM.Compiler.Events
import Util.PrettyPrint

type ProcName = String

data Proc =
    PAlphaParallel [(EventSet, Proc)]
    | PException Proc EventSet Proc
    | PExternalChoice [Proc]
    | PGenParallel EventSet [Proc]
    | PHide Proc EventSet
    | PInternalChoice [Proc]
    | PInterrupt Proc Proc
    | PInterleave [Proc]
    | PPrefix Event Proc
    | PSequentialComp Proc Proc
    | PSlidingChoice Proc Proc
    -- TODO:
    -- | PLinkParallel EventMap [Proc]
    -- | PRename EventMap Proc
    -- | POperator ProcOperator Proc
    -- where:
    -- data ProcOperator = Normalise | Explicate | StrongBisim | TauLoopFactor | Diamond | ModelCompress
    | PProcCall ProcName (Maybe Proc)

instance PrettyPrintable Proc where
    prettyPrint (PAlphaParallel aps) =
        text "||" <+> braces (list (map (\ (a,p) -> 
            parens (prettyPrint a <> char ',' <+> prettyPrint p)) aps))
    prettyPrint (PException p1 a p2) =
        prettyPrint p1 <+> text "[|" <> prettyPrint a <> text "|>" 
            <+> prettyPrint p2
    prettyPrint (PExternalChoice ps) =
        sep (punctuate (text " []") (map prettyPrint ps))
    prettyPrint (PGenParallel a ps) =
        text "||" <+> brackets (prettyPrint a) 
                <+> braces (list (map prettyPrint ps))
    prettyPrint (PHide p a) =
        prettyPrint p <+> char '\\' <+> prettyPrint a
    prettyPrint (PInternalChoice ps) =
        sep (punctuate (text " |~|") (map prettyPrint ps))
    prettyPrint (PInterleave ps) =
        sep (punctuate (text " |||") (map prettyPrint ps))
    prettyPrint (PPrefix e p) =
        prettyPrint e <+> text "->" <+> prettyPrint p
    prettyPrint (PSequentialComp p1 p2) =
        prettyPrint p1 <+> text "->" <+> prettyPrint p2
    prettyPrint (PSlidingChoice p1 p2) =
        prettyPrint p1 <+> text "|>" <+> prettyPrint p2
    
    prettyPrint (PProcCall s _) = text s
instance Show Proc where
    show p = show (prettyPrint p)
