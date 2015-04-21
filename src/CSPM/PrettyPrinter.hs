{-# LANGUAGE FlexibleInstances #-}
module CSPM.PrettyPrinter (
    prettyPrint, prettyPrintMatch,
)
where

import CSPM.Syntax.AST
import Util.Annotated
import Util.PrettyPrint

instance PrettyPrintable id => PrettyPrintable (CSPMFile id) where
    prettyPrint (CSPMFile decls) = 
        vcat (punctuate (char '\n') (map prettyPrint decls))

instance PrettyPrintable id => PrettyPrintable (InteractiveStmt id) where
    prettyPrint (Evaluate e) = prettyPrint e
    prettyPrint (Bind (decl:_)) = text "let" <+> prettyPrint decl
    prettyPrint (RunAssertion a) = text "assert" <+> prettyPrint a
    
prettyPrintMatch :: PrettyPrintable id => id -> AnMatch id -> Doc
prettyPrintMatch n (An _ _ (Match groups exp)) = 
        hang ((prettyPrint n <> hcat (map ppGroup groups))
             <+> equals)
            tabWidth (prettyPrint exp)
    where
        ppGroup ps = parens (list (map prettyPrint ps))

instance PrettyPrintable id => PrettyPrintable (STypeScheme id) where
    prettyPrint (STypeScheme _ [] t) = prettyPrint t
    prettyPrint (STypeScheme _ cs t) =
        hang (case cs of
                [c] -> prettyPrint c
                _ -> parens (list (map prettyPrint cs)))
            tabWidth
            (text "=>" <+> prettyPrint t)

instance PrettyPrintable id => PrettyPrintable (STypeConstraint id) where
    prettyPrint (STypeConstraint c n) = prettyPrint c <+> prettyPrint n

instance PrettyPrintable id => PrettyPrintable (SType id) where
    prettyPrint (STVar n) = prettyPrint n
    prettyPrint STProc = text "Proc"
    prettyPrint STInt = text "Int"
    prettyPrint STBool = text "Bool"
    prettyPrint STChar = text "Char"
    prettyPrint STEvent = text "Event"
    prettyPrint (STExtendable t n) =
        ppBinOp (prettyPrint n) (text "=>*") (prettyPrint t)
    prettyPrint (STSet t) = braces (prettyPrint t)
    prettyPrint (STSeq t) = angles (prettyPrint t)
    prettyPrint (STDot t1 t2) = prettyPrint t1 <> char '.' <> prettyPrint t2
    prettyPrint (STMap t1 t2) =
        text "(|" <+> prettyPrint t1 <+> text "=>" <+> prettyPrint t2
            <+> text "|)"
    prettyPrint (STTuple ts) = parens (list (map prettyPrint ts))
    prettyPrint (STFunction args rt) =
        ppBinOp (parens (list (map prettyPrint args))) (text "->")
            (prettyPrint rt)
    prettyPrint (STDotable t1 t2) =
        ppBinOp (prettyPrint t1) (text "=>") (prettyPrint t2)
    prettyPrint (STDatatype n) = prettyPrint n
    prettyPrint (STParen t) = parens (prettyPrint t)

prettyPrintTypeAnnotation :: PrettyPrintable id => Doc -> Maybe (AnSTypeScheme id) -> Doc
prettyPrintTypeAnnotation _ Nothing = empty
prettyPrintTypeAnnotation ns (Just ta) = 
    ns <+> text "::" <+> prettyPrint ta

instance PrettyPrintable id => PrettyPrintable (Decl id) where
    prettyPrint (FunBind n ms ts) =
        prettyPrintTypeAnnotation (prettyPrint n) ts
        $$ vcat (map (prettyPrintMatch n) ms)
    prettyPrint (PatBind pat exp ts) =
        prettyPrintTypeAnnotation (prettyPrint pat) ts
        $$ hang (prettyPrint pat <+> equals) tabWidth (prettyPrint exp)
    prettyPrint (Channel ns me ts) =
        prettyPrintTypeAnnotation (list (map prettyPrint ns)) ts
        $$ text "channel" <+> list (map prettyPrint ns)
        <+> case me of
                Just c -> text ":" <+> prettyPrint c
                Nothing -> empty
    prettyPrint (External ns) =
        text "external" <+> list (map prettyPrint ns)
    prettyPrint (Transparent ns) =
        text "transparent" <+> list (map prettyPrint ns)
    prettyPrint (DataType n dtcs) =
        text "datatype" <+> prettyPrint n <+> text "=" 
            <+> fsep (punctuate (text "|") (map prettyPrint dtcs))
    prettyPrint (SubType n dtcs) =
        text "subtype" <+> prettyPrint n <+> text "=" 
            <+> fsep (punctuate (text "|") (map prettyPrint dtcs))
    prettyPrint (NameType n e mta) =
        (case mta of
            Nothing -> empty
            Just ta -> prettyPrint n <+> text "::" <+> prettyPrint ta)
        $$ text "nametype" <+> prettyPrint n <+> text "=" <+> prettyPrint e
    prettyPrint (Assert a) =
        text "assert" <+> prettyPrint a
    prettyPrint (Module n args private exported) =
        text "module" <+> prettyPrint n <> parens (list (map prettyPrint args))
        $$ tabIndent (vcat (punctuate (char '\n') (map prettyPrint private)))
        $$ text "exports"
        $$ tabIndent (vcat (punctuate (char '\n') (map prettyPrint exported)))
        $$ text "endmodule"
    prettyPrint (TimedSection _ f ds) =
        text "Timed"
        <+> case f of
                Just f -> parens (prettyPrint f)
                Nothing -> empty
        <+> char '{'
        $$ tabIndent (vcat (punctuate (char '\n') (map prettyPrint ds)))
        $$ char '}'
    prettyPrint (ModuleInstance n nm args mp _) =
        text "instance" <+> prettyPrint n <+> char '=' <+> prettyPrint nm <>
            parens (list (map prettyPrint args))
    prettyPrint (PrintStatement s) = text "print" <+> bytestring s

instance PrettyPrintable id => PrettyPrintable (Assertion id) where
    prettyPrint (Refinement e1 m e2 opts) =
        hang (hang (prettyPrint e1) tabWidth
                (char '[' <> prettyPrint m <> char '=' <+> prettyPrint e2))
            tabWidth (fcat (map prettyPrint opts))
    prettyPrint (PropertyCheck e1 (HasTrace evs) Nothing opts) =
        hang (hang (prettyPrint e1) tabWidth
            (text ":[" <> text "has trace" <> text "]:"
                <+> angles (list (map prettyPrint evs))))
            tabWidth (fcat (map prettyPrint opts))
    prettyPrint (PropertyCheck e1 (HasTrace evs) (Just m) opts) =
        hang (hang (prettyPrint e1) tabWidth
            (colon <> brackets (text "has trace" <+> brackets (prettyPrint m))
                <+> angles (list (map prettyPrint evs))))
            tabWidth (fcat (map prettyPrint opts))
    prettyPrint (PropertyCheck e1 prop Nothing opts) =
        hang (hang (prettyPrint e1) tabWidth
            (text ":[" <> prettyPrint prop <> text "]"))
            tabWidth (fcat (map prettyPrint opts))
    prettyPrint (PropertyCheck e1 prop (Just m) opts) =
        hang (hang (prettyPrint e1) tabWidth
            (colon <> brackets (prettyPrint prop <+> brackets (prettyPrint m))))
            tabWidth (fcat (map prettyPrint opts))
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
    prettyPrint (PartialOrderReduce Nothing) = text ":[partial order reduce]"
    prettyPrint (PartialOrderReduce (Just m)) =
        text ":[partial order reduce" <+> bytestring m <+> text "]"
    prettyPrint (AnalyseStatically Nothing) = text ":[static]"
    prettyPrint (AnalyseStatically (Just m)) =
        text ":[static" <+> bytestring m <+> text "]"

instance PrettyPrintable id => PrettyPrintable (SemanticProperty id) where
    prettyPrint DeadlockFreedom = text "deadlock free"
    prettyPrint Deterministic = text "deterministic"
    prettyPrint LivelockFreedom = text "divergence free"
    prettyPrint (HasTrace _) = text "has trace"

instance PrettyPrintable id => PrettyPrintable (DataTypeClause id) where
    prettyPrint (DataTypeClause n me _) = prettyPrint n
        <> case me of
            Nothing -> empty
            Just e -> text "." <> prettyPrint e

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
    prettyPrint (Lambda ps exp) = 
        text "\\" <> list (map prettyPrint ps)
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
    prettyPrint (ListEnumFromComp lb stmts) = 
        angles (ppComp' [prettyPrint lb <> text ".."] stmts)
    prettyPrint (ListEnumFromToComp lb ub stmts) = 
        angles (ppComp' [prettyPrint lb <> text ".." <> prettyPrint ub] stmts)
    prettyPrint (Lit lit) = prettyPrint lit
    prettyPrint (Map kvs) = text "(|" <+>
        list (map (\ (k, v) -> prettyPrint k <+> text "=>" <+> prettyPrint v) kvs)
        <+> text "|)"
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
    prettyPrint (SetEnumFromComp lb stmts) = 
        braces (ppComp' [prettyPrint lb <> text ".."] stmts)
    prettyPrint (SetEnumFromToComp lb ub stmts) = 
        braces (ppComp' [prettyPrint lb <> text ".." <> prettyPrint ub] stmts)
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
    prettyPrint (Project e1 e2) = 
        ppBinOp (prettyPrint e1) (text "|\\") (prettyPrint e2)
    prettyPrint (Rename e ties stmts) =
        prettyPrint e <+> brackets (brackets (
                ppComp' (map ppRename ties) stmts
            ))
    prettyPrint (SequentialComp e1 e2) =
        ppBinOp (prettyPrint e1) (char ';') (prettyPrint e2)
    prettyPrint (SlidingChoice e1 e2) =
        ppBinOp (prettyPrint e1) (text "[>") (prettyPrint e2)
    prettyPrint (SynchronisingExternalChoice e1 a e2) =
        ppBinOp (prettyPrint e1) (text "[+" <> prettyPrint a <> text "+]")
            (prettyPrint e2)
    prettyPrint (SynchronisingInterrupt e1 a e2) =
        ppBinOp (prettyPrint e1) (text "/+" <> prettyPrint a <> text "+\\")
            (prettyPrint e2)

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
    prettyPrint (ReplicatedSequentialComp stmts e) =
        ppRepOp (text ";") stmts (prettyPrint e)
    prettyPrint (ReplicatedSynchronisingExternalChoice e1 stmts e2) =
        ppRepOp (text "[+" <> prettyPrint e1 <> text "+]")
            stmts (prettyPrint e2)
        
    -- Patterns - this is only used when emitting parser errors about invalid
    -- expressions.
    prettyPrint (ExpPatWildCard) = char '_'
    prettyPrint (ExpPatDoublePattern e1 e2) = 
        prettyPrint e1 <+> text "@@" <+> prettyPrint e2

    prettyPrint (TimedPrefix _ p) = prettyPrint p
    prettyPrint (LocatedApp (An _ _ (Var n)) _) = prettyPrint n

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
