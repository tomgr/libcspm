module OpSemPrettyPrinter where
import Text.PrettyPrint.HughesPJ

import OpSemDataStructures

-- *********************************************************************
-- Pretty Printer Extensions
-- *********************************************************************
angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

list :: [Doc] -> Doc
list docs =	
	fsep (punctuate (text ",") docs)

tabHang :: Doc -> Doc -> Doc
tabHang d1 d2 = hang d1 4 d2


-- *********************************************************************
-- Basic instances
-- *********************************************************************
class PrettyPrintable a where
	prettyPrint :: a -> Doc

instance PrettyPrintable Int where
	prettyPrint n = int n
instance (PrettyPrintable a, PrettyPrintable b) =>
		PrettyPrintable (a,b) where
	prettyPrint (a,b) = parens (list [prettyPrint a, prettyPrint b])
instance PrettyPrintable a => PrettyPrintable [a] where
	prettyPrint xs = angles (list (map prettyPrint xs))
	
instance PrettyPrintable Name where
	prettyPrint (Name s)	= text s

instance PrettyPrintable Event where
	prettyPrint Tau = text "tau"
	prettyPrint (Event s) = prettyPrint s
	prettyPrint (ChanEvent n ns) = 
		hcat $ punctuate (char '.') (prettyPrint n:map prettyPrint ns)

instance PrettyPrintable Exp where
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
	prettyPrint (ProcArgs) = text "ProcArgs"
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
instance PrettyPrintable ProcessRelation where
	prettyPrint (Performs e1 ev e2) = 
		prettyPrint e1 <+> text "="<> prettyPrint ev <> text"=>" <+> prettyPrint e2

instance PrettyPrintable Pattern where
	prettyPrint (PVar n) = prettyPrint n
	prettyPrint (PTuple ps) = 
		(parens . hsep . punctuate comma . map prettyPrint) ps
	prettyPrint (PSet ps) =
		(braces . hsep . punctuate comma . map prettyPrint) ps

instance PrettyPrintable SideCondition where
	prettyPrint (SCGenerator pat exp) = 
		prettyPrint pat <+> text "<-" <+> prettyPrint exp
	prettyPrint (Formula f) = prettyPrint f

instance PrettyPrintable PropositionalFormula where
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

instance PrettyPrintable InductiveRule where
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
