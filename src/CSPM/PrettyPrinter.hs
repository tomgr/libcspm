{-# LANGUAGE FlexibleInstances #-}
module CSPM.PrettyPrinter (
    prettyPrint, prettyPrintMatch,
)
where

import CSPM.DataStructures.Syntax
import Util.Annotated
import Util.PrettyPrint

instance PrettyPrintable id => PrettyPrintable [Module id] where
    prettyPrint = vcat . map prettyPrint

instance PrettyPrintable id => PrettyPrintable (Module id) where
    prettyPrint (GlobalModule decls) = 
        vcat (punctuate (char '\n') (map prettyPrint decls))

instance PrettyPrintable id => PrettyPrintable (InteractiveStmt id) where
    prettyPrint (Evaluate e) = prettyPrint e
    prettyPrint (Bind decl) = 
        text "let" <+> prettyPrint decl
    
prettyPrintMatch :: PrettyPrintable id => id -> AnMatch id -> Doc
prettyPrintMatch n (An _ _ (Match groups exp)) = 
        hang ((prettyPrint n <> hcat (map ppGroup groups))
             <+> equals)
            tabWidth (prettyPrint exp)
    where
        ppGroup ps = parens (list (map prettyPrint ps))

instance PrettyPrintable id => PrettyPrintable (Decl id) where
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
    prettyPrint (NameType n e) =
        text "nametype" <+> prettyPrint n <+> text "=" <+> prettyPrint e
    prettyPrint (Assert a) =
        text "assert" <+> prettyPrint a
        
instance PrettyPrintable id => PrettyPrintable (Assertion id) where
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
    prettyPrint (ASNot a) = text "not" <+> prettyPrint a

instance PrettyPrintable Model where
    prettyPrint Traces = text "T"
    prettyPrint Failures = text "F"
    prettyPrint FailuresDivergences = text "FD"
    prettyPrint Refusals = text "R"
    prettyPrint RefusalsDivergences = text "RD"
    prettyPrint Revivals = text "V"
    prettyPrint RevivalsDivergences = text "VD"
    
instance PrettyPrintable id => PrettyPrintable (ModelOption id) where
    prettyPrint (TauPriority e) = 
        text ":[tau priority over]:" <+> prettyPrint e

instance PrettyPrintable SemanticProperty where
    prettyPrint DeadlockFreedom = text "deadlock free"
    prettyPrint Deterministic = text "deterministic"
    prettyPrint LivelockFreedom = text "divergence free"

instance PrettyPrintable id => PrettyPrintable (DataTypeClause id) where
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

instance PrettyPrintable id => PrettyPrintable (Exp id) where  
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
    prettyPrint (Set exps) = braces (list (map prettyPrint exps))
    prettyPrint (SetComp es stmts) = braces (ppComp es stmts)
    prettyPrint (SetEnum es) = (braces . bars . list . map prettyPrint) es
    prettyPrint (SetEnumComp es stmts) = braces (bars (ppComp es stmts))
    prettyPrint (SetEnumFrom lb) = braces (prettyPrint lb <> text "..")
    prettyPrint (SetEnumFromTo lb ub) =
        braces (prettyPrint lb <> text ".." <> prettyPrint ub)
    prettyPrint (Tuple exps) = parens (list (map prettyPrint exps))
    prettyPrint (Var qname) = prettyPrint qname
        
    -- Processes
    prettyPrint(AlphaParallel e1 a1 a2 e2) =
        ppBinOp (prettyPrint e1) (brackets (prettyPrint a1 <> text "||"
                                    <> prettyPrint a2)) (prettyPrint e2)
    prettyPrint (Exception e1 a e2) =
        ppBinOp (prettyPrint e1) (text "[|" <+> prettyPrint a <+> text "|>") 
                (prettyPrint e2)
    prettyPrint (ExternalChoice e1 e2) = 
        ppBinOp (prettyPrint e1) (text "[]") (prettyPrint e2)
    prettyPrint (GenParallel e1 alpha e2) = 
        ppBinOp (prettyPrint e1) (brackets (bars (prettyPrint alpha))) 
                (prettyPrint e2)
    prettyPrint (GuardedExp e1 e2) = 
        ppBinOp (prettyPrint e1) (char '&') (prettyPrint e2)
    prettyPrint (Hiding e1 e2) = 
        ppBinOp (prettyPrint e1) (char '\\') (prettyPrint e2)
    prettyPrint (InternalChoice e1 e2) =
        ppBinOp (prettyPrint e1) (text "|~|") (prettyPrint e2)
    prettyPrint (Interrupt e1 e2) =
        ppBinOp (prettyPrint e1) (text "/\\") (prettyPrint e2)
    prettyPrint (Interleave e1 e2) =
        ppBinOp (prettyPrint e1) (text "|||") (prettyPrint e2)
    prettyPrint (LinkParallel e1 ties stmts e2) =
        ppBinOp (prettyPrint e1)
                (ppComp' (map ppTie ties) stmts)
                (prettyPrint e2)
    prettyPrint (Prefix ev fs e) =
        ppBinOp (prettyPrint ev <> hcat (map prettyPrint fs)) (text "->")
                (prettyPrint e)
    prettyPrint (Rename e ties stmts) =
        prettyPrint e <+> brackets (brackets (
                ppComp' (map ppRename ties) stmts
            ))
    prettyPrint (SequentialComp e1 e2) =
        ppBinOp (prettyPrint e1) (char ';') (prettyPrint e2)
    prettyPrint (SlidingChoice e1 e2) =
        ppBinOp (prettyPrint e1) (text "[>") (prettyPrint e2)

    prettyPrint (ReplicatedAlphaParallel stmts alpha e) = 
        ppRepOp (text "||") stmts 
                (brackets (prettyPrint alpha) <+> prettyPrint e)
    prettyPrint (ReplicatedExternalChoice stmts e) = 
        ppRepOp (text "[]") stmts (prettyPrint e)
    prettyPrint (ReplicatedInterleave stmts e) = 
        ppRepOp (text "|||") stmts (prettyPrint e)
    prettyPrint (ReplicatedInternalChoice stmts e) = 
        ppRepOp (text "|~|") stmts (prettyPrint e)
    prettyPrint (ReplicatedLinkParallel ties tiesStmts stmts e) =
        ppRepOp (brackets (ppComp' (map ppTie ties) tiesStmts)) 
                stmts (prettyPrint e)
    prettyPrint (ReplicatedParallel alpha stmts e) =
        ppRepOp (brackets (bars (prettyPrint alpha))) stmts (prettyPrint e)
    
    -- Patterns - this is only used when emitting parser errors about invalid
    -- expressions.
    prettyPrint (ExpPatWildCard) = char '_'
    prettyPrint (ExpPatDoublePattern e1 e2) = 
        prettyPrint e1 <+> text "@@" <+> prettyPrint e2

instance PrettyPrintable id => PrettyPrintable (Field id) where
    prettyPrint (Output exp) = 
        char '!' <> prettyPrint exp
    prettyPrint (Input pat Nothing) =
        char '?' <> prettyPrint pat
    prettyPrint (Input pat (Just exp)) =
        char '?' <> prettyPrint pat <+> colon <+> prettyPrint exp
    prettyPrint (NonDetInput pat Nothing) = 
        char '$' <> prettyPrint pat
    prettyPrint (NonDetInput pat (Just exp)) =
        char '$' <> prettyPrint pat <+> colon <+> prettyPrint exp

instance PrettyPrintable id => PrettyPrintable (Stmt id) where
    prettyPrint (Generator pat exp) = 
        sep [prettyPrint pat, text "<-" <+> prettyPrint exp]
    prettyPrint (Qualifier exp) = 
        prettyPrint exp

ppTie :: PrettyPrintable id => (AnExp id, AnExp id) -> Doc
ppTie (l, r) = prettyPrint l <+> text "<->" <+> prettyPrint r

ppRename :: PrettyPrintable id => (AnExp id, AnExp id) -> Doc
ppRename (l, r) = prettyPrint l <+> text "<-" <+> prettyPrint r

ppRepOp :: PrettyPrintable id => Doc -> [AnStmt id] -> Doc -> Doc
ppRepOp op stmts exp =
    hang op tabWidth (
        hang (list (map prettyPrint stmts) <+> char '@')
            tabWidth exp)

ppComp :: PrettyPrintable id => [AnExp id] -> [AnStmt id] -> Doc
ppComp es stmts = ppComp' (map prettyPrint es) stmts 
        
ppComp' :: PrettyPrintable id => [Doc] -> [AnStmt id] -> Doc
ppComp' es stmts = 
    hang (list es) tabWidth
        (if length stmts > 0 then char '|' <+> list (map prettyPrint stmts)
        else empty)
        
ppBinOp :: Doc -> Doc -> Doc -> Doc
ppBinOp arg1 op arg2 = sep [arg1, op <+> arg2]

ppBinOp' :: Doc -> Doc -> Doc -> Doc
ppBinOp' arg1 op arg2 = cat [arg1, op <> arg2]
