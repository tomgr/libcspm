{-# LANGUAGE FlexibleInstances #-}
module CSPM.Desugar (Desugarable(..)) where

import Control.Monad.Trans
import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.PrettyPrinter
import Util.Annotated
import Util.Exception
import Util.Monad
import Util.PrettyPrint hiding (($$))

class Desugarable a where
    desugar :: MonadIO m => a -> m a

instance Desugarable a => Desugarable [a] where
    desugar xs = mapM desugar xs
instance Desugarable a => Desugarable (Maybe a) where
    desugar Nothing = return Nothing
    desugar (Just a) = desugar a >>= return . Just
instance Desugarable a => Desugarable (Annotated b a) where
    desugar (An l b i) = desugar i >>= \ x -> return (An l b x)
instance (Desugarable a, Desugarable b) => Desugarable (a,b) where
    desugar (a,b) = do
        a' <- desugar a
        b' <- desugar b
        return (a', b')

instance Desugarable (Module Name) where
    desugar (GlobalModule ds) = return GlobalModule $$ desugar ds

instance Desugarable (Decl Name) where
    desugar (FunBind n ms) = return (FunBind n) $$ desugar ms
    desugar (PatBind p e) = return PatBind $$ desugar p $$ desugar e
    desugar (Assert a) = return Assert $$ desugar a
    desugar (External ns) = return $ External ns
    desugar (Transparent ns) = return $ Transparent ns
    desugar (Channel ns me) = return (Channel ns) $$ desugar me
    desugar (DataType n cs) = return (DataType n) $$ desugar cs
    desugar (SubType n cs) = return (SubType n) $$ desugar cs
    desugar (NameType n e) = return (NameType n) $$ desugar e

instance Desugarable (Assertion Name) where
    desugar (Refinement e1 m e2 opts) = 
        return Refinement $$ desugar e1 $$ desugar m $$ desugar e2 $$ desugar opts
    desugar (PropertyCheck e p m) = 
        return PropertyCheck $$ desugar e $$ desugar p $$ desugar m
    desugar (ASNot a) = return ASNot $$ desugar a

instance Desugarable SemanticProperty where
    desugar e = return e
instance Desugarable Model where
    desugar m = return m
instance Desugarable (ModelOption Name) where
    desugar (TauPriority e) = return TauPriority $$ desugar e

instance Desugarable (DataTypeClause Name) where
    desugar (DataTypeClause n me) = return (DataTypeClause n) $$ desugar me

instance Desugarable (Match Name) where
    desugar (Match pss e) = return Match $$ desugar pss $$ desugar e

instance Desugarable (Exp Name) where
    desugar (App e es) = return App $$ desugar e $$ desugar es
    desugar (BooleanBinaryOp op e1 e2) = 
        return (BooleanBinaryOp op) $$ desugar e1 $$ desugar e2
    desugar (BooleanUnaryOp op e) =
        return (BooleanUnaryOp op) $$ desugar e
    desugar (Concat e1 e2) = return Concat $$ desugar e1 $$ desugar e2
    desugar (DotApp e1 e2) = return DotApp $$ desugar e1 $$ desugar e2
    desugar (If e1 e2 e3) = return If $$ desugar e1 $$ desugar e2 $$ desugar e3
    desugar (Lambda p e) = return Lambda $$ desugar p $$ desugar e
    desugar (Let ds e) = return Let $$ desugar ds $$ desugar e
    desugar (Lit l) = return Lit $$ desugar l
    desugar (List es) = return List $$ desugar es
    desugar (ListComp es stmts) = return ListComp $$ desugar es $$ desugar stmts
    desugar (ListEnumFrom e) = return ListEnumFrom $$ desugar e
    desugar (ListEnumFromTo e1 e2) = return ListEnumFromTo $$ desugar e1 $$ desugar e2
    desugar (ListLength e) = return ListLength $$ desugar e
    desugar (MathsBinaryOp op e1 e2) = 
        return (MathsBinaryOp op) $$ desugar e1 $$ desugar e2
    desugar (MathsUnaryOp op e) = return (MathsUnaryOp op) $$ desugar e
    desugar (Paren e) = desugar e >>= return . unAnnotate
    desugar (Set es) = return Set $$ desugar es
    desugar (SetComp es stmts) = return SetComp $$ desugar es $$ desugar stmts
    desugar (SetEnum es) = return SetEnum $$ desugar es
    desugar (SetEnumComp es stmts) = return SetEnumComp $$ desugar es $$ desugar stmts
    desugar (SetEnumFrom e) = return SetEnumFrom $$ desugar e
    desugar (SetEnumFromTo e1 e2) = return SetEnumFromTo $$ desugar e1 $$ desugar e2
    desugar (Tuple es) = return Tuple $$ desugar es
    desugar (Var n) = return (Var n)

    desugar (AlphaParallel e1 e2 e3 e4) =
        return AlphaParallel $$ desugar e1 $$ desugar e2 $$ desugar e3 $$ desugar e4
    desugar (Exception e1 e2 e3) =
        return Exception $$ desugar e1 $$ desugar e2 $$ desugar e3
    desugar (ExternalChoice e1 e2) = return ExternalChoice $$ desugar e1 $$ desugar e2
    desugar (GenParallel e1 e2 e3) =
        return GenParallel $$ desugar e1 $$ desugar e2 $$ desugar e3
    desugar (GuardedExp e1 e2) = return GuardedExp $$ desugar e1 $$ desugar e2
    desugar (Hiding e1 e2) = return Hiding $$ desugar e1 $$ desugar e2
    desugar (InternalChoice e1 e2) = return InternalChoice $$ desugar e1 $$ desugar e2
    desugar (Interrupt e1 e2) = return Interrupt $$ desugar e1 $$ desugar e2
    desugar (Interleave e1 e2) = return Interleave $$ desugar e1 $$ desugar e2
    desugar (LinkParallel e1 ties stmts e2) = 
        return LinkParallel $$ desugar e1 $$ desugar ties $$ desugar stmts $$ desugar e2
    desugar (Prefix e1 fs e2) = return Prefix $$ desugar e1 $$ desugar fs $$ desugar e2
    desugar (Rename e1 ties stmts) =
        return Rename $$ desugar e1 $$ desugar ties $$ desugar stmts
    desugar (SequentialComp e1 e2) = return SequentialComp $$ desugar e1 $$ desugar e2
    desugar (SlidingChoice e1 e2) = return SlidingChoice $$ desugar e1 $$ desugar e2
    
    desugar (ReplicatedAlphaParallel stmts e1 e2) =
        return ReplicatedAlphaParallel $$ desugar stmts $$ desugar e1 $$ desugar e2
    desugar (ReplicatedInterleave stmts e) =
        return ReplicatedInterleave $$ desugar stmts $$ desugar e
    desugar (ReplicatedExternalChoice stmts e) =
        return ReplicatedExternalChoice $$ desugar stmts $$ desugar e
    desugar (ReplicatedInternalChoice stmts e) =
        return ReplicatedInternalChoice $$ desugar stmts $$ desugar e
    desugar (ReplicatedParallel stmts e1 e2) =
        return ReplicatedParallel $$ desugar stmts $$ desugar e1 $$ desugar e2
    desugar (ReplicatedLinkParallel ties tiesStmts stmts e) =
        return ReplicatedLinkParallel $$ desugar ties $$ desugar tiesStmts
                                $$ desugar stmts $$ desugar e
    
instance Desugarable (Field Name) where
    desugar (Output e) = return Output $$ desugar e
    desugar (Input p e) = return Input $$ desugar p $$ desugar e
    desugar (NonDetInput p e) = return NonDetInput $$ desugar p $$ desugar e

instance Desugarable (Stmt Name) where
    desugar (Generator p e) = return Generator $$ desugar p $$ desugar e
    desugar (Qualifier e) = return Qualifier $$ desugar e

instance Desugarable (InteractiveStmt Name) where
    desugar (Bind d) = return Bind $$ desugar d
    desugar (Evaluate e) = return Evaluate $$ desugar e
    desugar (RunAssertion a) = return RunAssertion $$ desugar a

instance Desugarable (Pat Name) where
    desugar (PConcat p1 p2) = 
        let
            combine (as1, Just (p, bs1)) (as2, Nothing) = (as1, Just (p, bs1++as2))
            combine (as1, Nothing) (as2, p) = (as1++as2, p)
            
            extractCompList :: TCPat -> ([TCPat], Maybe (TCPat, [TCPat]))
            extractCompList (An _ _ (PCompList ps mp _)) = (ps, mp)
            extractCompList p = ([], Just (p, []))
        in do
            p1' <- desugar p1
            p2' <- desugar p2
            let (start, end) = combine (extractCompList p1') (extractCompList p2')
            return $ PCompList start end (PConcat p1 p2)
    desugar (PList ps) =
        return PCompList $$ desugar ps $$ return Nothing $$ return (PList ps)
    desugar (PDotApp p1 p2) = 
        let
            extractDotList (An _ _ (PCompDot ds _)) = ds
            extractDotList d = [d]
        in do
            p1' <- desugar p1
            p2' <- desugar p2
            return $ PCompDot (extractDotList p1'++extractDotList p2') (PDotApp p1 p2)
    desugar (PDoublePattern p1 p2) =
        return PDoublePattern $$ desugar p1 $$ desugar p2
    desugar (PLit l) = return PLit $$ desugar l
    -- We don't remove the Paren as people may pretty print a desugared
    -- expression, which would then not have parenthesis needed to
    -- remove ambiguity
    desugar (PParen p) = desugar p >>= return . unAnnotate
    desugar (PSet []) = return $ PSet []
    desugar (PSet [p]) = return PSet $$ (desugar p >>= (\x -> return [x]))
    desugar (PSet ps) = throwSourceError [mkErrorMessage l err]
        where
            -- TODO: get a proper location for the whole
            -- pattern
            l = loc (head ps)
            err = prettyPrint (PSet ps) <+> text "is not a valid set pattern as set patterns may only match at most one element."
    desugar (PTuple ps) = return PTuple $$ desugar ps
    desugar (PVar n) = return $ PVar n
    desugar (PWildCard) = return PWildCard

instance Desugarable Literal where
    desugar l = return l
