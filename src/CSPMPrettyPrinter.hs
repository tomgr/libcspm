{-# LANGUAGE FlexibleInstances #-}
module CSPMPrettyPrinter where

import Util.Annotated

import CSPMDataStructures.Syntax
import Text.PrettyPrint.HughesPJ

class PrettyPrintable a where
	prettyPrint :: a -> Doc

-- *************************************************************************
-- Extensions
-- *************************************************************************
angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

bars :: Doc -> Doc
bars d = char '|' <> d <> char '|'

list :: [Doc] -> Doc
list docs =	
	fsep (punctuate (text ",") docs)

tabWidth = 4
tabindent = nest tabWidth

instance (PrettyPrintable b) => PrettyPrintable (Annotated a b) where
	prettyPrint (An loc typ inner) = prettyPrint inner
	
-- *************************************************************************
-- Names
-- *************************************************************************
instance PrettyPrintable Name where
	prettyPrint (Name s) = text s
instance PrettyPrintable QualifiedName where
	prettyPrint (UnQual name) = prettyPrint name

-- *************************************************************************
-- Modules
-- *************************************************************************
instance PrettyPrintable [Module] where
	prettyPrint = vcat . map prettyPrint

instance PrettyPrintable Module where
	prettyPrint (GlobalModule decls) = 
		vcat (punctuate (char '\n') (map prettyPrint decls))

-- *************************************************************************
-- Declarations
-- *************************************************************************
instance PrettyPrintable Decl where
	prettyPrint (FunBind name matches) =
			vcat (map (\ (An _ _ m) -> ppMatch m) matches)
		where
			ppGroup ps = parens (list (map prettyPrint ps))
			ppMatch (Match groups exp) = 
				hang ((prettyPrint name <> hcat (map ppGroup groups))
					 <+> equals)
					tabWidth (prettyPrint exp)			
	prettyPrint (PatBind pat exp) =
		hang (prettyPrint pat <+> equals)
			tabWidth (prettyPrint exp)
	prettyPrint (Channel ns es) =
		text "channel" <+> list (map prettyPrint ns)
		<+> 
		if length es == 0 then empty
		else text ":" <+> fsep (punctuate (text ".") (map prettyPrint es))
		
	prettyPrint (External ns) =
		text "external" <+> list (map prettyPrint ns)
	prettyPrint (Transparent ns) =
		text "transparent" <+> list (map prettyPrint ns)
	prettyPrint (DataType n dtcs) =
		text "datatype" <+> prettyPrint n <+> text "=" 
			<+> fsep (punctuate (text "|") (map prettyPrint dtcs))
instance PrettyPrintable Model where
	prettyPrint (Traces) = text "[T="
	prettyPrint (Failures) = text "[F="
	prettyPrint (FailuresDivergences) = text "[FD="

instance PrettyPrintable DataTypeClause where
	prettyPrint (DataTypeClause n es) =
		hcat (punctuate (text ".") (prettyPrint n:(map prettyPrint es)))

instance PrettyPrintable Pat where
	prettyPrint (PConcat p1 p2) =
		prettyPrint p1 <+> text "^" <+> prettyPrint p2
	prettyPrint (PDotApp p1 p2) =
		prettyPrint p1 <> text "." <> prettyPrint p2
	prettyPrint (PDoublePattern p1 p2) =
		prettyPrint p1 <+> text "@@" <+> prettyPrint p2
	prettyPrint (PList patterns) = 
		angles (list (map prettyPrint patterns))
	prettyPrint (PLit lit) = prettyPrint lit
	prettyPrint (PSet patterns) = 
		braces (list (map prettyPrint patterns))
	prettyPrint (PParen pattern) =
		parens (prettyPrint pattern)
	prettyPrint (PTuple patterns) = 
		parens (list (map prettyPrint patterns))
	prettyPrint (PVar name) = prettyPrint name
	prettyPrint (PWildCard) = char '_'

-- *************************************************************************
-- Expressions
-- *************************************************************************
instance PrettyPrintable BinaryBooleanOp where
	prettyPrint And = text "and"
	prettyPrint Or = text "or"
	prettyPrint Equals = text "=="
	prettyPrint NotEquals = text "!="
	prettyPrint GreaterThan = text ">"
	prettyPrint LessThan = text "<"
	prettyPrint LessThanEq = text "<="
	prettyPrint GreaterThanEq = text ">="

instance PrettyPrintable UnaryBooleanOp where
	prettyPrint Not = text "not"

instance PrettyPrintable BinaryMathsOp where
	prettyPrint Divide = text "/"
	prettyPrint Minus = text "-"
	prettyPrint Mod = text "%"
	prettyPrint Plus = text "+"
	prettyPrint Times = text "*"
instance PrettyPrintable UnaryMathsOp where
	prettyPrint Negate = text "-"

instance PrettyPrintable Exp where	
	prettyPrint (App e1 args) = 
		prettyPrint e1 <> parens (list (map prettyPrint args))
	prettyPrint (BooleanBinaryOp op e1 e2) =
		prettyPrint e1 <+> prettyPrint op <+> prettyPrint e2
	prettyPrint (BooleanUnaryOp op e1) =
		prettyPrint op <+> prettyPrint e1
	prettyPrint (Concat e1 e2) =
		prettyPrint e1 <> text "^" <> prettyPrint e2
	prettyPrint (DotApp e1 e2) =
		prettyPrint e1 <> text "." <> prettyPrint e2
	prettyPrint (If e1 e2 e3) = 
		hang (text "if" <+> prettyPrint e1 <+> text "then") 
			tabWidth (prettyPrint e2)
		$$
		hang (text "else")
			tabWidth (prettyPrint e3)
	prettyPrint (Let decls exp) = 
		text "let" $+$
		tabindent (vcat ((map prettyPrint decls))) $+$
		text "within"  $+$
		tabindent (prettyPrint exp)
	prettyPrint (ListLength exp) =
		char '#' <> prettyPrint exp
	prettyPrint (List exps) = 
		angles (list (map prettyPrint exps))
	prettyPrint (ListComp exps stmts) =
		angles (
			list (map prettyPrint exps)
			<+> char '|' 
			<+> list (map prettyPrint stmts))
	prettyPrint (ListEnumFrom lb) = 
		angles (prettyPrint lb <> text "...")
	prettyPrint (ListEnumFromTo lb ub) = 
		angles (prettyPrint lb <> text "..." <> prettyPrint ub)
	prettyPrint (Lit lit) = prettyPrint lit
	prettyPrint (MathsBinaryOp op e1 e2) =
		prettyPrint e1 <+> prettyPrint op <+> prettyPrint e2
	prettyPrint (Paren e) = parens (prettyPrint e)
	prettyPrint (Set exps) = 
		braces (list (map prettyPrint exps))
	prettyPrint (SetComp exps stmts) = 
		braces (
			list (map prettyPrint exps)
			<+> char '|' 
			<+> list (map prettyPrint stmts))
	prettyPrint (SetEnum es) =
		braces . bars . list . map prettyPrint $ es
	prettyPrint (SetEnumComp es stmts) =
		braces (bars (
			list (map prettyPrint es)
			<+> char '|' 
			<+> list (map prettyPrint stmts)))
	prettyPrint (SetEnumFrom lb) =
		braces (prettyPrint lb <> text "..")
	prettyPrint (SetEnumFromTo lb ub) =
		braces (prettyPrint lb <> text ".." <> prettyPrint ub)
	prettyPrint (Tuple exps) = parens (list (map prettyPrint exps))
	prettyPrint (Var qname) = prettyPrint qname

	-- Processes
	prettyPrint(AlphaParallel p1 a1 p2 a2) =
		fsep [prettyPrint p1,
			lbrack <+> prettyPrint a1 <+> text "||" <+> prettyPrint a2 <+> rbrack,
			prettyPrint p2]
	prettyPrint (Exception p1 a p2) =
		prettyPrint p1 <+> text "[|" <+> prettyPrint a <+> text "|>" <+> prettyPrint p2
	prettyPrint (ExternalChoice p1 p2) = sep [prettyPrint p1, text "[]" <+> prettyPrint p2]
{-
	prettyPrint (GenParallel p1 a p2) =
	prettyPrint (GuardedExp p1 p2) =
	prettyPrint (Hiding p1 p2) =
	prettyPrint (InternalChoice p1 p2) =
	prettyPrint (Interrupt p1 p2) =
	prettyPrint (Interleave p1 p2) =
	prettyPrint (LinkParallel p1 ties stmts p2) = 
-}
	prettyPrint (Prefix ev fs p1) =
		prettyPrint ev <> hcat (map prettyPrint fs) <+> text "->" <+> prettyPrint p1
{-
	prettyPrint (Rename p1 becomes stmts) =
	prettyPrint (SequentialComp p1 p2) =
	prettyPrint (SlidingChoice p1 p2) =
-}	

instance PrettyPrintable Field where
	prettyPrint (Output exp) = 
		text "!" <> prettyPrint exp
	prettyPrint (Input pat Nothing) =
		text "?" <> prettyPrint pat
	prettyPrint (Input pat (Just exp)) =
		text "?" <> prettyPrint pat <+> text ":" <+> prettyPrint exp

instance PrettyPrintable Stmt where
	prettyPrint (Generator pat exp) = 
		sep [prettyPrint pat, text "<-" <+> prettyPrint exp]
	prettyPrint (Qualifier exp) = 
		prettyPrint exp

instance PrettyPrintable Literal where
	prettyPrint (Int n) = integer n
	prettyPrint (Bool True) = text "true"
	prettyPrint (Bool False) = text "false"
