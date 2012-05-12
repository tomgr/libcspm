{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module CSPM.Operators.CSP.Desugar () where

import CSPM.Desugar
import CSPM.DataStructures.Names
import CSPM.Operators.CSP.Syntax

instance Desugarable (CSPProcess Name) where
    desugar (AlphaParallel e1 e2 e3 e4) =
        AlphaParallel (desugar e1) (desugar e2) (desugar e3) (desugar e4)
    desugar (Exception e1 e2 e3) =
        Exception (desugar e1) (desugar e2) (desugar e3)
    desugar (ExternalChoice e1 e2) = ExternalChoice (desugar e1) (desugar e2)
    desugar (GenParallel e1 e2 e3) =
        GenParallel (desugar e1) (desugar e2) (desugar e3)
    desugar (GuardedExp e1 e2) = GuardedExp (desugar e1) (desugar e2)
    desugar (Hiding e1 e2) = Hiding (desugar e1) (desugar e2)
    desugar (InternalChoice e1 e2) = InternalChoice (desugar e1) (desugar e2)
    desugar (Interrupt e1 e2) = Interrupt (desugar e1) (desugar e2)
    desugar (Interleave e1 e2) = Interleave (desugar e1) (desugar e2)
    desugar (LinkParallel e1 ties stmts e2) = 
        LinkParallel (desugar e1) (desugar ties) (desugar stmts) (desugar e2)
    desugar (Prefix e1 fs e2) = Prefix (desugar e1) (desugar fs) (desugar e2)
    desugar (Rename e1 ties stmts) =
        Rename (desugar e1) (desugar ties) (desugar stmts)
    desugar (SequentialComp e1 e2) = SequentialComp (desugar e1) (desugar e2)
    desugar (SlidingChoice e1 e2) = SlidingChoice (desugar e1) (desugar e2)
    
    desugar (ReplicatedAlphaParallel stmts e1 e2) =
        ReplicatedAlphaParallel (desugar stmts) (desugar e1) (desugar e2)
    desugar (ReplicatedInterleave stmts e) =
        ReplicatedInterleave (desugar stmts) (desugar e)
    desugar (ReplicatedExternalChoice stmts e) =
        ReplicatedExternalChoice (desugar stmts) (desugar e)
    desugar (ReplicatedInternalChoice stmts e) =
        ReplicatedInternalChoice (desugar stmts) (desugar e)
    desugar (ReplicatedParallel stmts e1 e2) =
        ReplicatedParallel (desugar stmts) (desugar e1) (desugar e2)
    desugar (ReplicatedLinkParallel ties tiesStmts stmts e) =
        ReplicatedLinkParallel (desugar ties) (desugar tiesStmts) 
                                (desugar stmts) (desugar e)
    
instance Desugarable (CSPField Name) where
    desugar (Output e) = Output (desugar e)
    desugar (Input p e) = Input (desugar p) (desugar e)
    desugar (NonDetInput p e) = NonDetInput (desugar p) (desugar e)
