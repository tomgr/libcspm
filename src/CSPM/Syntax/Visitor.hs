{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module CSPM.Syntax.Visitor (
    Visitor(..), defaultVisitor,
    Visitable(..),
) where

import CSPM.Syntax.AST
import CSPM.Syntax.Names
import Util.Annotated

-- | A visitor over the abstract syntax tree.
--
-- The visiting is guaranteed to be done in depth-first order. A visitor takes
-- two arguments: the AST node being visited, and a program that will visit the
-- children of the AST node. This allows for properties to be set on the monad
-- during the traversal of the children, and for subtrees to be completely
-- ignored.
data Visitor m = Visitor {
        visitFile :: TCCSPMFile -> m () -> m (),
        visitDecl :: TCDecl -> m () -> m (),
        visitMatch :: TCMatch -> m () -> m (),
        visitPat :: TCPat -> m () -> m (),
        visitExp :: TCExp -> m () -> m (),
        visitField :: TCField -> m () -> m (),
        visitStmt :: TCStmt -> m () -> m (),
        visitDataTypeClause :: TCDataTypeClause -> m () -> m (),
        visitAssertion :: TCAssertion -> m () -> m (),
        visitInteractiveStmt :: TCInteractiveStmt -> m () -> m (),
        visitSTypeScheme :: TCSTypeScheme -> m () -> m (),
        visitSTypeConstraint :: TCSTypeConstraint -> m () -> m (),
        visitSType :: TCSType -> m () -> m (),
        visitModelOption :: TCModelOption -> m () -> m ()
    }

-- | A visit that simply recursively visits each node.
defaultVisitor :: Monad m => Visitor m
defaultVisitor = Visitor {
        visitFile = ignore,
        visitDecl = ignore,
        visitMatch = ignore,
        visitPat = ignore,
        visitExp = ignore,
        visitField = ignore,
        visitStmt = ignore,
        visitDataTypeClause = ignore,
        visitAssertion = ignore,
        visitInteractiveStmt = ignore,
        visitSTypeScheme = ignore,
        visitSTypeConstraint = ignore,
        visitSType = ignore,
        visitModelOption = ignore
    }
    where ignore _ prog = prog

class Monad m => Visitable a m where
    visit :: Visitor m -> a -> m ()

instance Visitable a m => Visitable [a] m where
    visit v xs = mapM_ (visit v) xs
instance (Visitable a m, Visitable b m) => Visitable (a, b) m where
    visit v (a, b) = visit v a >> visit v b
instance Visitable a m => Visitable (Maybe a) m where
    visit _ Nothing = return ()
    visit v (Just a) = visit v a

instance Monad m => Visitable TCCSPMFile m where
    visit v i@(An _ _ a) = visitFile v i (visit v a)
instance Monad m => Visitable TCDecl m where
    visit v i@(An _ _ a) = visitDecl v i (visit v a)
instance Monad m => Visitable TCMatch m where
    visit v i@(An _ _ a) = visitMatch v i (visit v a)
instance Monad m => Visitable TCPat m where
    visit v i@(An _ _ a) = visitPat v i (visit v a)
instance Monad m => Visitable TCExp m where
    visit v i@(An _ _ a) = visitExp v i (visit v a)
instance Monad m => Visitable TCField m where
    visit v i@(An _ _ a) = visitField v i (visit v a)
instance Monad m => Visitable TCStmt m where
    visit v i@(An _ _ a) = visitStmt v i (visit v a)
instance Monad m => Visitable TCDataTypeClause m where
    visit v i@(An _ _ a) = visitDataTypeClause v i (visit v a)
instance Monad m => Visitable TCAssertion m where
    visit v i@(An _ _ a) = visitAssertion v i (visit v a)
instance Monad m => Visitable TCInteractiveStmt m where
    visit v i@(An _ _ a) = visitInteractiveStmt v i (visit v a)
instance Monad m => Visitable TCSTypeScheme m where
    visit v i@(An _ _ a) = visitSTypeScheme v i (visit v a)
instance Monad m => Visitable TCSTypeConstraint m where
    visit v i@(An _ _ a) = visitSTypeConstraint v i (visit v a)
instance Monad m => Visitable TCSType m where
    visit v i@(An _ _ a) = visitSType v i (visit v a)
instance Monad m => Visitable TCModelOption m where
    visit v i@(An _ _ a) = visitModelOption v i (visit v a)

instance Monad m => Visitable (CSPMFile Name) m where
    visit v (CSPMFile ds) = visit v ds

instance Monad m => Visitable (Decl Name) m where
    visit v (FunBind n ms ta) = visit v ms >> visit v ta
    visit v (PatBind p e ta) = visit v p >> visit v e >> visit v ta
    visit v (Channel ns es ta) = visit v es >> visit v ta
    visit v (DataType n cs) = visit v cs
    visit v (SubType n cs) = visit v cs
    visit v (NameType n e ta) = visit v e >> visit v ta
    visit v (External ns) = return ()
    visit v (Transparent ns) = return ()
    visit v (Assert a) = visit v a
    visit v (Module _ ps ds1 ds2) = visit v ps >> visit v ds1 >> visit v ds2
    visit v (TimedSection _ f ds) = visit v f >> visit v ds
    visit v (ModuleInstance _ n args _ _) = visit v args
    visit v (PrintStatement _) = return ()

instance Monad m => Visitable (Exp Name) m where
    visit v (App e es) = visit v (e:es)
    visit v (LocatedApp e es) = visit v (e:es)
    visit v (BooleanBinaryOp _ e1 e2) = visit v [e1, e2]
    visit v (BooleanUnaryOp _ e) = visit v e
    visit v (Concat e1 e2) = visit v [e1, e2]
    visit v (DotApp e1 e2) = visit v [e1, e2]
    visit v (If e1 e2 e3) = visit v [e1, e2, e3]
    visit v (Lambda p e) = visit v e
    visit v (Let ds e) = visit v ds >> visit v e
    visit v (Lit _) =  return ()
    visit v (List es) = visit v es
    visit v (ListComp es stmts) =
        visit v es >> visit v stmts
    visit v (ListEnumFrom e1) = visit v e1
    visit v (ListEnumFromTo e1 e2) = visit v [e1,e2]
    visit v (ListEnumFromComp e1 stmts) =
        visit v e1 >> visit v stmts
    visit v (ListEnumFromToComp e1 e2 stmts) =
        visit v [e1, e2] >> visit v stmts
    visit v (ListLength e) = visit v e
    visit v (Map kvs) = visit v kvs
    visit v (MathsBinaryOp _ e1 e2) = visit v [e1,e2]
    visit v (MathsUnaryOp _ e1) = visit v e1
    visit v (Paren e) = visit v e
    visit v (Set es) = visit v es
    visit v (SetComp es stmts) =
        visit v es >> visit v stmts
    visit v (SetEnumComp es stmts) =
        visit v es >> visit v stmts
    visit v (SetEnumFrom e1) = visit v e1
    visit v (SetEnumFromTo e1 e2) = visit v [e1,e2]
    visit v (SetEnumFromComp e1 stmts) =
        visit v e1 >> visit v stmts
    visit v (SetEnumFromToComp e1 e2 stmts) =
        visit v [e1, e2] >> visit v stmts
    visit v (SetEnum es) = visit v es
    visit v (Tuple es) = visit v es
    visit v (Var _) = return ()
    
    visit v (AlphaParallel e1 e2 e3 e4) = visit v [e1,e2,e3,e4]
    visit v (Exception e1 e2 e3) = visit v [e1,e2,e3]
    visit v (ExternalChoice e1 e2) = visit v [e1,e2]
    visit v (GenParallel e1 e2 e3) = visit v [e1,e2,e3]
    visit v (GuardedExp e1 e2) = visit v [e1,e2]
    visit v (Hiding e1 e2) = visit v [e1,e2]
    visit v (InternalChoice e1 e2) = visit v [e1,e2]
    visit v (Interrupt e1 e2) = visit v [e1,e2]
    visit v (LinkParallel e1 links stmts e2) =
        visit v [e1, e2] >> visit v links >> visit v stmts
    visit v (Interleave e1 e2) = visit v [e1, e2]
    visit v (Prefix e1 fields e2) =
        visit v [e1, e2] >> visit v fields
    visit v (TimedPrefix e1 e2) = visit v e2
    visit v (Project e1 e2) = visit v [e1, e2]
    visit v (Rename e1 renames stmts) =
        visit v e1 >> visit v renames >> visit v stmts
    visit v (SequentialComp e1 e2) = visit v [e1,e2]
    visit v (SlidingChoice e1 e2) = visit v [e1,e2]
    visit v (SynchronisingExternalChoice e1 e2 e3) = visit v [e1,e2,e3]
    visit v (SynchronisingInterrupt e1 e2 e3) = visit v [e1,e2,e3]

    visit v (ReplicatedAlphaParallel stmts e1 e2) = 
        visit v [e1, e2] >> visit v stmts
    visit v (ReplicatedInterleave stmts e1) = 
        visit v e1 >> visit v stmts
    visit v (ReplicatedExternalChoice stmts e1) = 
        visit v e1 >> visit v stmts
    visit v (ReplicatedInternalChoice stmts e1) = 
        visit v e1 >> visit v stmts
    visit v (ReplicatedLinkParallel ties tiesStmts stmts e) =
        visit v ties >> visit v tiesStmts >>
        visit v stmts >> visit v e
    visit v (ReplicatedParallel e1 stmts e2) =
        visit v e1 >> visit v stmts >> visit v e2
    visit v (ReplicatedSequentialComp stmts e1) =
        visit v stmts >> visit v e1
    visit v (ReplicatedSynchronisingExternalChoice e1 stmts e2) = 
        visit v e1 >> visit v stmts >> visit v e2

instance Monad m => Visitable (Stmt Name) m where
    visit v (Generator p e) = visit v p >> visit v e
    visit v (Qualifier e) = visit v e

instance Monad m => Visitable (Field Name) m where
    visit v (Input p e) = visit v p >> visit v e
    visit v (NonDetInput p e) = visit v p >> visit v e
    visit v (Output e) = visit v e

instance Monad m => Visitable (Assertion Name) m where
    visit v (Refinement e1 m e2 opts) =
        visit v [e1, e2] >> visit v opts
    visit v (PropertyCheck e1 p m opts) =
        visit v e1 >> visit v opts
    visit v (SymmetryCheck e1 ns) = visit v e1
    visit v (ASNot a) = visit v a

instance Monad m => Visitable (ModelOption Name) m where
    visit v (TauPriority e) = visit v e
    visit v (PartialOrderReduce _) = return ()
    visit v (SymmetryReduce _) = return ()

instance Monad m => Visitable (InteractiveStmt Name) m where
    visit v (Evaluate e) = visit v e
    visit v (Bind ds) = visit v ds
    visit v (RunAssertion a) = visit v a

instance Monad m => Visitable (Match Name) m where
    visit v (Match pss e) = visit v pss >> visit v e

instance Monad m => Visitable (DataTypeClause Name) m where
    visit v (DataTypeClause _ e _) = visit v e

instance Monad m => Visitable (Pat Name) m where
    visit v (PConcat p1 p2) = visit v [p1,p2]
    visit v (PDotApp p1 p2) = visit v [p1,p2]
    visit v (PList ps) = visit v ps
    visit v (PWildCard) = return ()
    visit v (PTuple ps) = visit v ps
    visit v (PSet ps) = visit v ps
    visit v (PParen p) = visit v p
    visit v (PLit l) = return ()
    visit v (PDoublePattern p1 p2) = visit v p1 >> visit v p2
    visit v (PCompList ps1 ps2 _) = visit v ps1 >> visit v ps2
    visit v (PCompDot ps _) = visit v ps
    visit v (PVar n) = return ()

instance Monad m => Visitable (STypeScheme Name) m where
    visit v (STypeScheme _ cs t) = visit v cs >> visit v t

instance Monad m => Visitable (STypeConstraint Name) m where
    visit v _ = return ()

instance Monad m => Visitable (SType Name) m where
    visit v (STVar n) = return ()
    visit v (STExtendable t n) = visit v t
    visit v (STSet t) = visit v t
    visit v (STSeq t) = visit v t
    visit v (STDot t1 t2) = visit v t1 >> visit v t2
    visit v (STTuple ts) = visit v ts
    visit v (STFunction ts t) = visit v ts >> visit v t
    visit v (STDotable t1 t2) = visit v [t1, t2]
    visit v (STParen t) = visit v t
    visit v (STMap t1 t2) = visit v [t1, t2]
    visit v (STDatatype n) = return ()
    visit v STProc = return ()
    visit v STInt = return ()
    visit v STBool = return ()
    visit v STChar = return ()
    visit v STEvent = return ()
