{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module CSPM.Desugar (Desugarable(..)) where

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.PrettyPrinter
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

class Desugarable a where
    desugar :: a -> a
    desugarWithType :: a -> Type -> a
    
    desugarWithType a t = desugar a

instance Desugarable a => Desugarable [a] where
    desugar xs = map desugar xs
instance Desugarable a => Desugarable (Maybe a) where
    desugar Nothing = Nothing
    desugar (Just a) = Just (desugar a)
instance Desugarable a => Desugarable (Annotated Type a) where
    desugar (An l t i) = An l t (desugarWithType i t)
instance Desugarable a => Desugarable (Annotated b a) where
    desugar (An l b i) = An l b (desugar i)
instance (Desugarable a, Desugarable b) => Desugarable (a,b) where
    desugar (a,b) = (desugar a, desugar b)

instance Desugarable (p Name) => Desugarable (Module Name p) where
    desugar (GlobalModule ds) = GlobalModule (desugar ds)

instance Desugarable (p Name) => Desugarable (Decl Name p) where
    desugar (FunBind n ms) = FunBind n (desugar ms)
    desugar (PatBind p e) = PatBind (desugar p) (desugar e)
    desugar (Assert a) = Assert (desugar a)
    desugar (External ns) = External ns
    desugar (Transparent ns) = Transparent ns
    desugar (Channel ns me) = Channel ns (desugar me)
    desugar (DataType n cs) = DataType n (desugar cs)
    desugar (NameType n e) = NameType n (desugar e)

instance Desugarable (p Name) => Desugarable (Assertion Name p) where
    desugar (Refinement e1 m e2 opts) = 
        Refinement (desugar e1) m (desugar e2) (desugar opts)
    desugar (PropertyCheck e p m) = 
        PropertyCheck (desugar e) p m

instance Desugarable (p Name) => Desugarable (ModelOption Name p) where
    desugar (TauPriority e) = TauPriority (desugar e)

instance Desugarable (p Name) => Desugarable (DataTypeClause Name p) where
    desugar (DataTypeClause n me) = DataTypeClause n (desugar me)

instance Desugarable (p Name) => Desugarable (Match Name p) where
    desugar (Match pss e) = Match (desugar pss) (desugar e)

instance Desugarable (p Name) => Desugarable (Exp Name p) where
    desugar (App e es) = App (desugar e) (desugar es)
    desugar (BooleanBinaryOp op e1 e2) = 
        BooleanBinaryOp op (desugar e1) (desugar e2)
    desugar (BooleanUnaryOp op e) =
        BooleanUnaryOp op (desugar e)
    desugar (Concat e1 e2) = Concat (desugar e1) (desugar e2)
    desugar (DotApp e1 e2) = DotApp (desugar e1) (desugar e2)
    desugar (If e1 e2 e3) = If (desugar e1) (desugar e2) (desugar e3)
    desugar (Lambda p e) = Lambda (desugar p) (desugar e)
    desugar (Let ds e) = Let (desugar ds) (desugar e)
    desugar (Lit l) = Lit (desugar l)
    desugar (List es) = List (desugar es)
    desugar (ListComp es stmts) = ListComp (desugar es) (desugar stmts)
    desugar (ListEnumFrom e) = ListEnumFrom (desugar e)
    desugar (ListEnumFromTo e1 e2) = ListEnumFromTo (desugar e1) (desugar e2)
    desugar (ListLength e) = ListLength (desugar e)
    desugar (MathsBinaryOp op e1 e2) = 
        MathsBinaryOp op (desugar e1) (desugar e2)
    desugar (MathsUnaryOp op e) = MathsUnaryOp op (desugar e)
    -- We don't remove the Paren as people may pretty print a desugared
    -- expression, which would then not have parenthesis needed to
    -- remove ambiguity
    desugar (Paren e) = unAnnotate (desugar e)
    desugar (Process p) = Process (desugar p)
    desugar (Set es) = Set (desugar es)
    desugar (SetComp es stmts) = SetComp (desugar es) (desugar stmts)
    desugar (SetEnum es) = SetEnum (desugar es)
    desugar (SetEnumComp es stmts) = SetEnumComp (desugar es) (desugar stmts)
    desugar (SetEnumFrom e) = SetEnumFrom (desugar e)
    desugar (SetEnumFromTo e1 e2) = SetEnumFromTo (desugar e1) (desugar e2)
    desugar (Tuple es) = Tuple (desugar es)
    desugar (Var n) = Var n

instance Desugarable (p Name) => Desugarable (Stmt Name p) where
    desugar (Generator p e) = Generator (desugar p) (desugar e)
    desugar (Qualifier e) = Qualifier (desugar e)

instance Desugarable (p Name) => Desugarable (InteractiveStmt Name p) where
    desugar (Bind d) = Bind (desugar d)
    desugar (Evaluate e) = Evaluate (desugar e)
    desugar (RunAssertion a) = RunAssertion (desugar a)

instance Desugarable (Pat Name) where
    desugar (PConcat p1 p2) = 
        let
            combine (as1, Just (p, bs1)) (as2, Nothing) = (as1, Just (p, bs1++as2))
            combine (as1, Nothing) (as2, p) = (as1++as2, p)
            
            extractCompList :: TCPat -> ([TCPat], Maybe (TCPat, [TCPat]))
            extractCompList (An _ _ (PCompList ps mp _)) = (ps, mp)
            extractCompList p = ([], Just (p, []))
            
            (start, end) = 
                combine (extractCompList . desugar $ p1)
                        (extractCompList . desugar $ p2)
        in PCompList start end (PConcat p1 p2)
    desugar (PList ps) = PCompList (map desugar ps) Nothing (PList ps)
    desugar (PDotApp p1 p2) = 
        let
            extractDotList (An _ _ (PCompDot ds _)) = ds
            extractDotList d = [d]
            ds1 = extractDotList (desugar p1)
            ds2 = extractDotList (desugar p2)
        in PCompDot (ds1++ds2) (PDotApp p1 p2)
    desugar (PDoublePattern p1 p2) =
        PDoublePattern (desugar p1) (desugar p2)
    desugar (PLit l) = PLit (desugar l)
    -- We don't remove the Paren as people may pretty print a desugared
    -- expression, which would then not have parenthesis needed to
    -- remove ambiguity
    desugar (PParen p) = unAnnotate (desugar p)
    desugar (PSet []) = PSet []
    desugar (PSet [p]) = PSet [desugar p]
    desugar (PSet ps) = throwSourceError [mkErrorMessage l err]
        where
            -- TODO: get a proper location for the whole
            -- pattern
            l = loc (head ps)
            err = prettyPrint (PSet ps) <+> text "is not a valid set pattern as set patterns may only match at most one element."
    desugar (PTuple ps) = PTuple (map desugar ps)
    desugar (PVar n) = PVar n
    desugar (PWildCard) = PWildCard

instance Desugarable Literal where
    desugar l = l
