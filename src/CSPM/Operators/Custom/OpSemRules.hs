module OpSemRules (
    compileOperators, rulesFunctionToCSP, 
    discardableArgsFunctionToCSP, operatorShortcutsToCSP,
    operatorDatatypeToCSP,
    replicatedOperatorsToCSP,
    channelsToCSP) where
import Data.List
import Text.PrettyPrint.HughesPJ

import OpSemDataStructures
import OpSemTypeChecker
import Util

-- *************************************************************************
-- Transformer
-- *************************************************************************
compileOperators :: OpSemDefinition -> [CompiledOp]
compileOperators opSemDefn = 
    let
        ops = operators opSemDefn
        transformOperator (op @ (Operator name args rules _)) =
            let
                Name cname = name
                cargs = args
                crules = map (transformInductiveRule ops op) rules
            in
                CompiledOp cname cargs crules (discardableArguments crules)
    in
        map transformOperator [op | op @ (Operator _ _ _ _) <- ops]

transformInductiveRule :: [Operator] -> Operator -> InductiveRule -> CompiledRule
transformInductiveRule ops op (InductiveRule pre post conditions) =
    let     
        (Performs (OperatorApp name currentOperatorArgs) resultingEvent 
                        (OperatorApp newOperator newArgs)) = post
        -- All arguments of the operator of type proc
        procArgs = [pr | Var pr <- currentOperatorArgs, 
                            typeIsProc (getArgType op pr)]
        -- Map from proc |-> proc; if resultingProcs p' = p then it means
        -- p -> p' by some rule
        resultingProcs = 
            [(pr', pr) | Performs (Var pr) _ (Var pr') <- pre]
        -- Map from proc to proc; resulting proc is an argument of the current
        -- operator (and thus has an index)
        procMap = resultingProcs 
            ++ (identityFunction (procArgs \\ functionImage resultingProcs))
        
        currentOnProcs = map (getId op) (onProcs op)
        currentOffProcs = map (getId op) (offProcs op)
        
        resultingOperator @ (Operator _ resultingOperatorArgs _ _) = 
            getOp ops newOperator
        resultingOperatorName = (name,
                [exp | (exp, (_, t)) <- zip newArgs resultingOperatorArgs, 
                        not (typeIsProc t)])
            where
                Name name = newOperator
        resultingOperatorProcessArgs = 
            zip [0..] [arg | (Var arg, (_, TOnProcess _)) 
                                <- zip newArgs resultingOperatorArgs]
            ++ zip [(-length (offProcs resultingOperator))..(-1)] 
                            [arg | (Var arg, (_, TOffProcess _)) 
                                <- zip newArgs resultingOperatorArgs]
        -- PartialFunction from arg id of resulting op to argument value
        resultingOnProcs = 
            map (\ pid -> (pid, apply resultingOperatorProcessArgs pid))
                (map (getId resultingOperator) (onProcs resultingOperator))
        -- PartialFunction from arg id of resulting op to argument value
        resultingOffProcs =
            map (\ pid -> (pid, apply resultingOperatorProcessArgs pid))
                (map (getId resultingOperator) (offProcs resultingOperator))
            
        -- If procMap pid = pid' then it means the pid of the new operator
        -- is pid' of the old process
        onProcMap =
            [(pid, getId op (apply procMap pr')) | (pid,pr') <- resultingOnProcs]
        -- Map from pid to [0..]; if newProcMap pid = n it means that the 
        -- nth turned on argument of the resulting operator is the pid off
        -- argument of the current operator
        newProcMap = 
            zip [pid' | (pid, pid') <- onProcMap, elem pid' currentOffProcs] [0..]
        
        phi = [(getId op n, ev) | Performs (Var n) ev (Var n') <- pre]
        psi = [(pid, if pid' < 0 then apply newProcMap pid' else pid')
                | (pid, pid') <- onProcMap]
        f = invert newProcMap
        chi = 
            [(pid, getId op (apply procMap pr')) | (pid,pr') <- resultingOffProcs]
        discards = 
            currentOnProcs
            \\ (map (getId op) (mapPF procMap (functionImage resultingOnProcs)))
        generators = map transformSideCondition conditions
        boundVars = varsBound conditions
    in
{-      error (
            show resultingOperator ++"\n\n"
            ++ show resultingOperatorProcessArgs ++"\n\n"
            ++ show resultingOnProcs ++"\n\n"
            ++ show resultingOffProcs ++"\n\n"
            ++ show procMap++"\n\n"
            ++ show phi ++"\n\n"
            ++ show psi ++"\n\n"
            
            ++show resultingOperatorName++"\n\n"
            ++show resultingEvent++"\n\n"           
            ++show f++"\n\n"
            ++show chi++"\n\n"
            ++show discards++"\n\n"
            ++show generators++"\n\n"
        )
-}
        CompiledRule phi resultingEvent resultingOperatorName f psi 
            chi discards boundVars generators
            
transformSideCondition :: SideCondition -> Stmt
transformSideCondition (SCGenerator p e) = Generator p e
transformSideCondition (Formula f) = PropFormula f

getOp :: [Operator] -> Name -> Operator
getOp ops n =
    head [op | (op @ (Operator n' _ _ _)) <- ops, n==n']
    
getArgType :: Operator -> Name -> Type
getArgType (Operator _ args _ _) n = 
    head [t | (n', t) <- args, n==n']

-- A process is off iff there exists no rule that allows it to perform any
-- visible event
offProcs :: Operator -> [Name]
offProcs (op @ (Operator n args rules _)) = 
    [Name pr | (Name pr, TOffProcess _) <- args]

onProcs :: Operator -> [Name]
onProcs (op @ (Operator _ args rules _)) = 
    [Name pr | (Name pr, TOnProcess _) <- args]

getId :: Operator -> Name -> ProcId
getId op n =
    head ([id | (n', id) <- zip (onProcs op) [0..], n==n']
            ++[id | (n', id) <- zip (offProcs op) [-(length (offProcs op))..(-1)], 
                    n==n']
    )

-- *********************************************************************
-- Discardable Arguments
-- *********************************************************************
discardableArguments :: [CompiledRule] -> [ProcId]
discardableArguments rules = 
    (nub . concat) 
        [discards | CompiledRule _ _ _ _ _ _ discards _ _ <- rules]



-- *********************************************************************
-- Pretty Printer
-- *********************************************************************
opNameToCSP s args =
    hcat (punctuate (char '.') ((text "Op_" <> text s):map toCSP args))

channelsToCSP :: OpSemDefinition -> Doc
channelsToCSP opSemDefn =
    let
        chans = channels opSemDefn
        ppChan (Channel n exps) = 
            text "channel" <+> toCSP n <+>
                if length exps == 0 then empty
                else text ":" <+> hcat (punctuate (char '.') (map setExpr exps))
        systemEvents = 
            text "SystemEvents = union(UserEvents, {|" 
                <+> list (map (\ (Channel n _) -> toCSP n) chans) <+> text "|})"
        setExpr e = toCSP e
    in
        vcat (systemEvents:map ppChan chans)

rulesFunctionToCSP :: [CompiledOp] -> Doc
rulesFunctionToCSP ops =
    let
        ppOp (CompiledOp name args rules discards) = 
            tabHang (text "Rules" <> parens (opNameToCSP name nonProcArgs) 
                        <+> equals)
                    (text "concat" <> (parens . angles . list . map toCSP) rules)
            where
                nonProcArgs = [n | (n, t) <- args, not (typeIsProc t)]
    in
        vcat (map ppOp ops)

discardableArgsFunctionToCSP :: [CompiledOp] -> Doc
discardableArgsFunctionToCSP ops =
    let
        ppOp (CompiledOp name args rules discards) = 
            tabHang (text "DiscardableArgs" <> parens (opNameToCSP name nonProcArgs) 
                    <+> equals)
                    (toCSP discards)
            where
                nonProcArgs = [n | (n, t) <- args, not (typeIsProc t)]
    in
        vcat (map ppOp ops)

operatorShortcutsToCSP :: [CompiledOp] -> Doc
operatorShortcutsToCSP ops =
    let 
        ppOp(CompiledOp name args rules discards) =
            tabHang (text (name++"'") <> 
                    (if length args == 0 then empty
                    else parens (list (map toCSP [n | (n,t) <- args])))
                    <+> equals)
                    (text "Operator_M::Operator"
                    <> parens (opName <> comma <+>
                                toCSP [n | (n, TOnProcess _) <- args] <> comma <+>
                                toCSP [n | (n, TOffProcess _) <- args]))
            where
                opName = hcat (punctuate (char '.') 
                    ((text "Op_" <> text name):map (toCSP . fst) nonProcArgs))
                nonProcArgs = [(n,t) | (n, t) <- args, not (typeIsProc t)]
    in
        vcat (map ppOp ops)

operatorDatatypeToCSP :: [CompiledOp] -> Doc
operatorDatatypeToCSP ops =
    let
        ppOp (CompiledOp name args rules discards) = 
                hcat (punctuate (char '.') 
                    ((text "Op_" <> text name):map constructor nonProcTypes))
            where
                nonProcTypes = [t | (n, t) <- args, not (typeIsProc t)]
                constructor TEvent = text "UserEvents"
                constructor (TSet a) = text "Set" <> parens (constructor a)
                constructor (TTuple ts) =
                    let
                        names = zip ts ['a'..'z']
                        generator (t, n) = 
                            char n <+> text "<-" <+> constructor t
                    in
                        braces (
                            (parens (list (map (char . snd) names))
                            <+> char '|' <+>
                            (sep . punctuate comma . map generator) names
                            ))
                constructor' t = error (show t)
    in
        text "datatype Operators = "
        $$ nest 4 (vcat ((\(x:xs) -> x:(map ((<+>) (char '|')) xs)) (map ppOp ops)))

replicatedOperatorsToCSP :: OpSemDefinition -> Doc
replicatedOperatorsToCSP opSemDefn =
    vcat (map replicatedOperatorToCSP 
            [repOp | repOp @ (ReplicatedOperator _ _ _ _ _) <- operators opSemDefn])

replicatedOperatorToCSP :: Operator -> Doc
replicatedOperatorToCSP 
    (ReplicatedOperator (Name n) args (basePats, baseExp)
                        (inductiveVars, inductiveCase) _) =
    
    tabHang (
        text (n++"'")
        <> (parens . hsep . punctuate comma $ 
            map basePat basePats)
        <+> char '=')
        (setExpr baseExp)
    $$
    tabHang (
        text (n++"'")
        <> (parens . hsep . punctuate comma $ 
            zipWith (\ current inductive -> 
                        angles (toCSP current) <> char '^' <> toCSP inductive)
                    (map fst args) inductiveVars)
        <+> char '=')
        (setExpr inductiveCase)
    
    where
        basePat (PVar (Name s)) = text s
        basePat (PSet ps) = angles (list $ map basePat ps)

        setExpr (OperatorApp (Name n) es) = 
            text (n++"'") <> 
                if length es == 0 then empty else parens (list (map setExpr es))
        setExpr (Tuple es) = 
            (parens . hsep . punctuate comma . map setExpr) es
        setExpr (Var n) = 
            -- TODO: sort out typing
            if n `elem` inductiveVars then text "set" <> parens (toCSP n)
            else toCSP n
        setExpr SigmaPrime = text "SystemEvents"
        setExpr Sigma = text "UserEvents"
        setExpr ProcArgs = text "ProcArgs"
        setExpr (Powerset e) = text "Set" <> parens (setExpr e)
        setExpr (SetMinus s1 s2) = 
            text "diff" <> parens (setExpr s1 <> comma <+> setExpr s2)
        setExpr (Union s1 s2) = 
            text "union" <> parens (setExpr s1 <> comma <+> setExpr s2)
        setExpr (Intersection s1 s2) = 
            text "inter" <> parens (setExpr s1 <> comma <+> setExpr s2)
        setExpr (SetComprehension exps stmts) =
            -- TODO: sort out typing
            braces ((hsep . punctuate comma . map setExpr $ exps) 
                    <+> char '|' <+> (hsep . punctuate comma . map toCSP $ stmts))
        setExpr (InductiveCase) = 
            text (n++"'")
            <> parens (hsep (punctuate comma (map toCSP inductiveVars)))
        setExpr (ReplicatedUnion s) =
            text "Union" <> (parens (setExpr s))
        
        
    
-- *********************************************************************
-- Data Types
-- *********************************************************************
class ToCSP a where
    toCSP :: a -> Doc

-- *********************************************************************
-- Pretty Printer Extensions
-- *********************************************************************
list :: [Doc] -> Doc
list docs = 
    fsep (punctuate (text ",") docs)

tabHang :: Doc -> Doc -> Doc
tabHang d1 d2 = hang d1 4 d2

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

-- *********************************************************************
-- Basic instances
-- *********************************************************************

instance ToCSP Int where
    toCSP n = int n
instance (ToCSP a, ToCSP b) =>
        ToCSP (a,b) where
    toCSP (a,b) = parens (list [toCSP a, toCSP b])
instance ToCSP a => ToCSP [a] where
    toCSP xs = angles (list (map toCSP xs))

instance ToCSP Name where
    toCSP (Name "callProc") = text "callProc"
    toCSP (Name s) = text (s++"0")

instance ToCSP Event where
    toCSP Tau = text "tau"
    toCSP (Event s) = toCSP s
    toCSP (ChanEvent n ns) = 
        hcat (punctuate (char '.') (toCSP n:map toCSP ns))

instance ToCSP Exp where
    toCSP (OperatorApp n es) = toCSP n <> parens (list (map toCSP es))
    toCSP (Tuple es) = 
        (parens . hsep . punctuate comma . map toCSP) es
    toCSP (Var n) = toCSP n
    
    toCSP (Powerset e) = text "Set" <> parens (toCSP e)
    toCSP (ProcArgs) = text "ProcArgs"
    toCSP (Sigma) = text "UserEvents"
    toCSP (SigmaPrime) = text "SystemEvents"
    toCSP (SetMinus s1 s2) = 
        text "diff" <> parens (toCSP s1 <> comma <+> toCSP s2)
    toCSP (Union s1 s2) = 
        text "union" <> parens (toCSP s1 <> comma <+> toCSP s2)
    toCSP (Intersection s1 s2) = 
        text "inter" <> parens (toCSP s1 <> comma <+> toCSP s2)
    toCSP (SetComprehension exps stmts) =
        braces ((hsep . punctuate comma . map toCSP $ exps) 
                -- TODO: below should be sets not seqs
                <+> char '|' <+> (hsep . punctuate comma . map toCSP $ stmts))
    toCSP (Set exps) = braces (hsep . punctuate comma . map toCSP $ exps)
instance ToCSP SideCondition where
    -- SETS
    toCSP (SCGenerator p e) = 
        toCSP p <+> text "<-" <+> toCSP e
instance ToCSP PropositionalFormula where
    toCSP (Member p e) = text "member" <> parens (list [toCSP p, toCSP e])
    toCSP (Subset e1 e2) = toCSP e1 <+> text "<=" <+> toCSP e2
    toCSP (Equals e1 e2) = toCSP e1 <+> text "==" <+> toCSP e2
    toCSP (Not f) = text "not" <+> toCSP f
    toCSP (And f1 f2) = toCSP f1 <+> text "and" <+> toCSP f2
    toCSP (Or f1 f2) = toCSP f1 <+> text "or" <+> toCSP f2
    toCSP (PFalse) = text "false"
    toCSP (PTrue) = text "true"
instance ToCSP Pattern where
    toCSP (PVar n) = toCSP n
    toCSP (PTuple ps) = parens (list (map toCSP ps))
    toCSP (PSet ps) = braces (list (map toCSP ps))

instance ToCSP Stmt where
    toCSP (Generator exp set) = 
        toCSP exp <+> text "<-" <+> toCSP set
    toCSP (PropFormula f) = toCSP f
    
instance ToCSP CompiledRule where
    toCSP (CompiledRule phi resultingEvent (n, args) f psi chi discards 
                        namesBound generators) =
        angles (parens (list [
            toCSP phi, toCSP resultingEvent, 
            opNameToCSP n args, toCSP f, toCSP psi, 
            toCSP chi, toCSP discards
        ]) $$
            if length generators == 0 then empty
            else char '|' <+> 
                parens (list $ map toCSP namesBound) <+> text "<-" 
                        <+> text "seq" <+> parens (braces (
                            parens (list $ map toCSP namesBound) <+> char '|' <+>
                            list (map toCSP generators))))
