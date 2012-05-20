module CSPM.Operators.Custom.OpSemPrettyPrinter where
import Text.PrettyPrint.HughesPJ

import CSPM.DataStructures.Names
import CSPM.Operators.Custom.OpSemDataStructures
import Util.PrettyPrint

-- *********************************************************************
-- Basic instances
-- *********************************************************************
instance PrettyPrintable Int where
    prettyPrint n = int n
instance (PrettyPrintable a, PrettyPrintable b) =>
        PrettyPrintable (a,b) where
    prettyPrint (a,b) = parens (list [prettyPrint a, prettyPrint b])

instance PrettyPrintable id => PrettyPrintable (Event id) where
    prettyPrint Tau = text "tau"
    prettyPrint (Event s) = prettyPrint s
    prettyPrint (ChanEvent n ns) = 
        hcat $ punctuate (char '.') (prettyPrint n:map prettyPrint ns)

instance PrettyPrintable id => PrettyPrintable (Exp id) where
    prettyPrint (OperatorApp n es) = 
        prettyPrint n <> parens (list (map prettyPrint es))
    prettyPrint (Tuple es) = 
        (parens . hsep . punctuate comma . map prettyPrint) es
    prettyPrint (Var n) = prettyPrint n
    
    prettyPrint (SetComprehension es scs) =
        braces (list (map prettyPrint es) 
                <+> text "|" <+> list (map prettyPrint scs))
    
    prettyPrint (SigmaPrime) = text "SigmaPrime"
    prettyPrint (Sigma) = text "Sigma"
    prettyPrint (InductiveCase) = text "InductiveCase"
    prettyPrint (Powerset e) = text "Set" <> parens (prettyPrint e)
    prettyPrint (SetMinus s1 s2) = 
        text "diff" <> parens (prettyPrint s1 <> comma <+> prettyPrint s2)
    prettyPrint (Union s1 s2) = 
        text "union" <> parens (prettyPrint s1 <> comma <+> prettyPrint s2)
    prettyPrint (Intersection s1 s2) = 
        text "inter" <> parens (prettyPrint s1 <> comma <+> prettyPrint s2)
    prettyPrint (ReplicatedUnion e) =
        text "Union" <> parens (prettyPrint e)
    prettyPrint (Set es) =
        braces (list (map prettyPrint es))
instance PrettyPrintable id => PrettyPrintable (ProcessRelation id) where
    prettyPrint (Performs e1 ev e2) = 
        prettyPrint e1 <+> text "="<> prettyPrint ev <> text"=>" <+> prettyPrint e2

instance PrettyPrintable id => PrettyPrintable (Pat id) where
    prettyPrint (PVar n) = prettyPrint n
    prettyPrint (PTuple ps) = 
        (parens . hsep . punctuate comma . map prettyPrint) ps
    prettyPrint (PSet ps) =
        (braces . hsep . punctuate comma . map prettyPrint) ps

instance PrettyPrintable id => PrettyPrintable (SideCondition id) where
    prettyPrint (SCGenerator pat exp) = 
        prettyPrint pat <+> text "<-" <+> prettyPrint exp
    prettyPrint (Formula f) = prettyPrint f

instance PrettyPrintable id => PrettyPrintable (Formula id) where
    prettyPrint (Member pat exp) =
        text "member" <> parens (prettyPrint pat <> comma <+> prettyPrint exp)
    prettyPrint (Equals e1 e2) = prettyPrint e1 <+> text "==" <+> prettyPrint e2
    prettyPrint (Not f) = text "not" <+> prettyPrint f
    prettyPrint (And f1 f2) = 
        prettyPrint f1 <+> text "and" <+> prettyPrint f2
    prettyPrint (Or f1 f2) =
        prettyPrint f1 <+> text "or" <+> prettyPrint f2
    prettyPrint (PFalse) = text "false"
    prettyPrint (PTrue) = text "true"

instance PrettyPrintable id => PrettyPrintable (InductiveRule id) where
    prettyPrint (InductiveRule pres post scs) =
        let
            presString = show (vcat (map prettyPrint pres))
            postString = show (prettyPrint post)
            dashes 0 = empty
            dashes n = char '-' <> dashes (n-1)
        in
            text presString
            $+$ (dashes (max (length presString) (length postString))
                <+> list (map prettyPrint scs))
            $+$ text postString
