{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module CSPM.PrettyPrinter (
    prettyPrint, prettyPrintMatch,
    ppBinOp, ppBinOp', ppComp, ppComp',
)
where

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

instance (PrettyPrintable id, PrettyPrintable (p id)) =>
        PrettyPrintable [Module id p] where
    prettyPrint = vcat . map prettyPrint

instance (PrettyPrintable id, PrettyPrintable (p id)) =>
        PrettyPrintable (Module id p) where
    prettyPrint (GlobalModule decls) = 
        vcat (punctuate (char '\n') (map prettyPrint decls))

instance (PrettyPrintable id, PrettyPrintable (p id)) => 
        PrettyPrintable (InteractiveStmt id p) where
    prettyPrint (Evaluate e) = prettyPrint e
    prettyPrint (Bind decl) = 
        text "let" <+> prettyPrint decl
    
prettyPrintMatch :: (PrettyPrintable id, PrettyPrintable (p id)) => 
    id -> AnMatch id p -> Doc
prettyPrintMatch n (An _ _ (Match groups exp)) = 
        hang ((prettyPrint n <> hcat (map ppGroup groups))
             <+> equals)
            tabWidth (prettyPrint exp)
    where
        ppGroup ps = parens (list (map prettyPrint ps))

instance (PrettyPrintable id, PrettyPrintable (p id)) => 
        PrettyPrintable (Decl id p) where
    prettyPrint (FunBind n ms) = vcat (map (prettyPrintMatch n) ms)
    prettyPrint (PatBind pat exp) =
        hang (prettyPrint pat <+> equals)
            tabWidth (prettyPrint exp)
    prettyPrint (Channel ns Nothing) =
        text "channel" <+> list (map prettyPrint ns)
    prettyPrint (Channel ns (Just e)) =
        text "channel" <+> list (map prettyPrint ns)
        <+> text ":" <+> prettyPrint e
    prettyPrint (External ns) =
        text "external" <+> list (map prettyPrint ns)
    prettyPrint (Transparent ns) =
        text "transparent" <+> list (map prettyPrint ns)
    prettyPrint (DataType n dtcs) =
        text "datatype" <+> prettyPrint n <+> text "=" 
            <+> fsep (punctuate (text "|") (map prettyPrint dtcs))
    prettyPrint (Assert a) =
        text "assert" <+> prettyPrint a
        
instance (PrettyPrintable id, PrettyPrintable (p id)) => 
        PrettyPrintable (Assertion id p) where
    prettyPrint (Refinement e1 m e2 opts) =
        hang (hang (prettyPrint e1) tabWidth
                (char '[' <> prettyPrint m <> char '=' <+> prettyPrint e2))
            tabWidth (fcat (map prettyPrint opts))
    prettyPrint (PropertyCheck e1 prop Nothing) =
        hang (prettyPrint e1) tabWidth
            (text ":[" <> prettyPrint prop <> text "]")
    prettyPrint (PropertyCheck e1 prop (Just m)) =
        hang (prettyPrint e1) tabWidth
            (colon <> brackets (prettyPrint prop <+> brackets (prettyPrint m)))

instance PrettyPrintable Model where
    prettyPrint Traces = text "T"
    prettyPrint Failures = text "F"
    prettyPrint FailuresDivergences = text "FD"
    prettyPrint Refusals = text "R"
    prettyPrint RefusalsDivergences = text "RD"
    prettyPrint Revivals = text "V"
    prettyPrint RevivalsDivergences = text "VD"
    
instance (PrettyPrintable id, PrettyPrintable (p id)) => 
        PrettyPrintable (ModelOption id p) where
    prettyPrint (TauPriority e) = 
        text ":[tau priority over]:" <+> prettyPrint e

instance PrettyPrintable SemanticProperty where
    prettyPrint DeadlockFreedom = text "deadlock free"
    prettyPrint Deterministic = text "deterministic"
    prettyPrint LivelockFreedom = text "divergence free"

instance (PrettyPrintable id, PrettyPrintable (p id)) =>
        PrettyPrintable (DataTypeClause id p) where
    prettyPrint (DataTypeClause n Nothing) = prettyPrint n
    prettyPrint (DataTypeClause n (Just e)) = 
        prettyPrint n <> text "." <> prettyPrint e

instance PrettyPrintable id => PrettyPrintable (Pat id) where
    prettyPrint (PConcat e1 e2) =
        prettyPrint e1 <+> text "^" <+> prettyPrint e2
    prettyPrint (PDotApp e1 e2) =
        prettyPrint e1 <> text "." <> prettyPrint e2
    prettyPrint (PDoublePattern e1 e2) =
        prettyPrint e1 <+> text "@@" <+> prettyPrint e2
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
    
    prettyPrint (PCompList _ _ p) = prettyPrint p
    prettyPrint (PCompDot _ p) = prettyPrint p
    
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
    -- We include an extra space here to avoid { -1} being pretty printed as
    -- {-1} which would start a block comment
    prettyPrint Negate = text " -"

instance (PrettyPrintable id, PrettyPrintable (p id)) => 
        PrettyPrintable (Exp id p) where  
    prettyPrint (App e1 args) = 
        prettyPrint e1 <> parens (list (map prettyPrint args))
    prettyPrint (BooleanBinaryOp op e1 e2) = 
        ppBinOp (prettyPrint e1) (prettyPrint op) (prettyPrint e2)
    prettyPrint (BooleanUnaryOp op e1) = prettyPrint op <+> prettyPrint e1
    prettyPrint (Concat e1 e2) =
        ppBinOp' (prettyPrint e1) (char '^') (prettyPrint e2)
    prettyPrint (DotApp e1 e2) =
        ppBinOp' (prettyPrint e1) (char '.') (prettyPrint e2)
    prettyPrint (If e1 e2 e3) = 
        sep [hang (text "if" <+> prettyPrint e1 <+> text "then") 
                tabWidth (prettyPrint e2),
            text "else" <+> prettyPrint e3]
    prettyPrint (Lambda pat exp) = 
        text "\\" <> prettyPrint pat
        <+> text "@"
        <+> prettyPrint exp
    prettyPrint (Let decls exp) = 
        sep [hang (text "let") tabWidth (vcat (map prettyPrint decls)),
            hang (text "within") tabWidth (prettyPrint exp)]
    prettyPrint (ListLength exp) = char '#' <> prettyPrint exp
    prettyPrint (List exps) = angles (list (map prettyPrint exps))
    prettyPrint (ListComp es stmts) = angles (ppComp es stmts)
    prettyPrint (ListEnumFrom lb) = angles (prettyPrint lb <> text "..")
    prettyPrint (ListEnumFromTo lb ub) = 
        angles (prettyPrint lb <> text ".." <> prettyPrint ub)
    prettyPrint (Lit lit) = prettyPrint lit
    prettyPrint (MathsUnaryOp op e1) = prettyPrint op <> prettyPrint e1
    prettyPrint (MathsBinaryOp op e1 e2) =
        ppBinOp' (prettyPrint e1) (prettyPrint op) (prettyPrint e2)
    prettyPrint (Paren e) = parens (prettyPrint e)
    prettyPrint (Process p) = prettyPrint p
    prettyPrint (Set exps) = braces (list (map prettyPrint exps))
    prettyPrint (SetComp es stmts) = braces (ppComp es stmts)
    prettyPrint (SetEnum es) = (braces . bars . list . map prettyPrint) es
    prettyPrint (SetEnumComp es stmts) = braces (bars (ppComp es stmts))
    prettyPrint (SetEnumFrom lb) = braces (prettyPrint lb <> text "..")
    prettyPrint (SetEnumFromTo lb ub) =
        braces (prettyPrint lb <> text ".." <> prettyPrint ub)
    prettyPrint (Tuple exps) = parens (list (map prettyPrint exps))
    prettyPrint (Var qname) = prettyPrint qname
    -- Patterns - this is only used when emitting parser errors about invalid
    -- expressions.
    prettyPrint (ExpPatWildCard) = char '_'
    prettyPrint (ExpPatDoublePattern e1 e2) = 
        prettyPrint e1 <+> text "@@" <+> prettyPrint e2

instance (PrettyPrintable id, PrettyPrintable (p id)) => 
        PrettyPrintable (Stmt id p) where
    prettyPrint (Generator pat exp) = 
        sep [prettyPrint pat, text "<-" <+> prettyPrint exp]
    prettyPrint (Qualifier exp) = 
        prettyPrint exp

ppComp :: (PrettyPrintable id, PrettyPrintable (p id)) =>
    [AnExp id p] -> [AnStmt id p] -> Doc
ppComp es stmts = ppComp' (map prettyPrint es) stmts 
        
ppComp' :: (PrettyPrintable id, PrettyPrintable (p id)) => 
    [Doc] -> [AnStmt id p] -> Doc
ppComp' es stmts = 
    hang (list es) tabWidth
        (if length stmts > 0 then char '|' <+> list (map prettyPrint stmts)
        else empty)

ppBinOp :: Doc -> Doc -> Doc -> Doc
ppBinOp arg1 op arg2 = sep [arg1, op <+> arg2]

ppBinOp' :: Doc -> Doc -> Doc -> Doc
ppBinOp' arg1 op arg2 = cat [arg1, op <> arg2]
