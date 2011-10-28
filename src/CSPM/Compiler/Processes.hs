-- | This module provides the input data structure to the compiler.
module CSPM.Compiler.Processes (
    Proc(..), 
    ProcOperator(..), 
    ProcName
) where

import qualified Data.Set.ListSet as S
import CSPM.Compiler.Events
import Util.PrettyPrint

type ProcName = String

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

instance PrettyPrintable ProcOperator where
    prettyPrint Chase = text "chase"
    prettyPrint Diamond = text "diamond"
    prettyPrint Explicate = text "explicate"
    prettyPrint Normalize = text "normal"
    prettyPrint ModelCompress = text "model_compress"
    prettyPrint StrongBisim = text "sbisim"
    prettyPrint TauLoopFactor = text "tau_loop_factor"
    prettyPrint WeakBisim = text "wbisim"

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
    -- TODO | PLinkParallel EventMap [Proc]
    | POperator ProcOperator Proc
    | PPrefix Event Proc
    -- TODO | PRename EventMap Proc
    | PSequentialComp Proc Proc
    | PSlidingChoice Proc Proc
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
    -- TODO | PLinkParallel EventMap [Proc]
    -- TODO | POperator ProcOperator Proc
    prettyPrint (PPrefix e p) =
        prettyPrint e <+> text "->" <+> prettyPrint p
    -- TODO | PRename EventMap Proc
    prettyPrint (PSequentialComp p1 p2) =
        prettyPrint p1 <+> text "->" <+> prettyPrint p2
    prettyPrint (PSlidingChoice p1 p2) =
        prettyPrint p1 <+> text "|>" <+> prettyPrint p2
    
    prettyPrint (PProcCall s _) = text s
instance Show Proc where
    show p = show (prettyPrint p)
