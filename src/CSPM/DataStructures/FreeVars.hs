{-# LANGUAGE FlexibleInstances #-}
module CSPM.DataStructures.FreeVars (
    BoundNames(..), FreeVars(..)
) where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import Data.List
import Util.Annotated
import Util.Exception

class BoundNames a where
    boundNames :: a -> [Name]

instance BoundNames a => BoundNames (Annotated b a) where
    boundNames = boundNames . unAnnotate
instance BoundNames a => BoundNames [a] where
    boundNames = concatMap boundNames
instance BoundNames (Decl Name) where
    boundNames (FunBind n ms _) = [n]
    boundNames (PatBind p ms _) = boundNames p
    boundNames (Channel ns es _) = ns
    boundNames (SubType n dcs) = [n]
    boundNames (DataType n dcs) = n : boundNames dcs
    boundNames (NameType n e _) = [n]
    boundNames (External ns) = []
    boundNames (Transparent ns) = []
    boundNames (Assert _) = []
    boundNames (Module n args ds1 ds2) = [n]
    boundNames (TimedSection _ _ ds) = boundNames ds
    boundNames (ModuleInstance _ _ _ nm _) = map fst nm
    boundNames (PrintStatement _) = []

instance BoundNames (DataTypeClause Name) where
    boundNames (DataTypeClause n _ _) = [n]

instance BoundNames (Pat Name) where
    boundNames (PVar n) | isNameDataConstructor n = []
    boundNames (PVar n) = [n]
    boundNames (PConcat p1 p2) = boundNames [p1,p2]
    boundNames (PDotApp p1 p2) = boundNames [p1,p2]
    boundNames (PList ps) = boundNames ps
    boundNames (PWildCard) = []
    boundNames (PTuple ps) = boundNames ps
    boundNames (PSet ps) = boundNames ps
    boundNames (PParen p) = boundNames p
    boundNames (PLit l) = []
    boundNames (PDoublePattern p1 p2) = boundNames [p1,p2]
    boundNames (PCompList ps1 Nothing _) = boundNames ps1
    boundNames (PCompList ps1 (Just (p, ps2)) _) =
        boundNames ps1 ++ boundNames p ++ boundNames ps2
    boundNames (PCompDot ps _) = boundNames ps

instance BoundNames (Stmt Name) where
    boundNames (Qualifier e) = []
    boundNames (Generator p e) = boundNames p

instance BoundNames (Field Name) where
    boundNames (Input p e) = boundNames p
    boundNames (NonDetInput p e) = boundNames p
    boundNames (Output e) = []

class FreeVars a where
    freeVars :: a -> [Name]
    freeVars xs = nub (sort (freeVars' xs))
    freeVars' :: a -> [Name]

instance FreeVars a => FreeVars [a] where
    freeVars' xs = concatMap freeVars' xs
instance FreeVars a => FreeVars (Maybe a) where
    freeVars' (Just x) = freeVars' x
    freeVars' Nothing = []
instance FreeVars a => FreeVars (Annotated b a) where
    freeVars' (An _ _ inner) = freeVars' inner

instance FreeVars (Pat Name) where
    -- A variable is free iff it is a data constructor, as in all other cases
    -- it indicates that a new variable should be created, thus by definition it
    -- is not free.
    freeVars' (PVar n) | isNameDataConstructor n = [n]
    freeVars' (PVar n) = []
    freeVars' (PConcat p1 p2) = freeVars' p1 ++ freeVars' p2
    freeVars' (PDotApp p1 p2) = freeVars' [p1,p2]
    freeVars' (PList ps) = freeVars' ps
    freeVars' (PWildCard) = []
    freeVars' (PTuple ps) = freeVars' ps
    freeVars' (PSet ps) = freeVars' ps
    freeVars' (PParen p) = freeVars' p
    freeVars' (PLit l) = []
    freeVars' (PDoublePattern p1 p2) = freeVars' p1 ++ freeVars' p2
    freeVars' (PCompList ps1 Nothing _) = freeVars' ps1
    freeVars' (PCompList ps1 (Just (p, ps2)) _) =
        freeVars' ps1 ++ freeVars' p ++ freeVars' ps2
    freeVars' (PCompDot ps _) = freeVars' ps

instance FreeVars (Exp Name) where
    freeVars' (App e es) = freeVars' (e:es)
    freeVars' (BooleanBinaryOp _ e1 e2) = freeVars' [e1, e2]
    freeVars' (BooleanUnaryOp _ e) = freeVars' e
    freeVars' (Concat e1 e2) = freeVars' [e1, e2]
    freeVars' (DotApp e1 e2) = freeVars' [e1, e2]
    freeVars' (If e1 e2 e3) = freeVars' [e1, e2, e3]
    freeVars' (Lambda p e) =
        let
            fvsp = boundNames p
            depsp = freeVars p
            fvse = freeVars e
        in (fvse \\ fvsp)++depsp
    freeVars' (Let ds e) =
        let
            fvsd = freeVars ds
            newBoundVars = boundNames ds
            fvse = freeVars e
        in nub (fvse++fvsd) \\ newBoundVars
    freeVars' (Lit _) =  []
    freeVars' (List es) = freeVars es
    freeVars' (ListComp es stmts) =
        let
            fvStmts = boundNames stmts
            depsStmts = freeVars stmts
            fvses' = freeVars es
            fvse = nub (fvses'++depsStmts)
        in fvse \\ fvStmts
    freeVars' (ListEnumFrom e1) = freeVars' e1
    freeVars' (ListEnumFromTo e1 e2) = freeVars' [e1,e2]
    freeVars' (ListEnumFromComp e1 stmts) = freeVarsStmts stmts [e1]
    freeVars' (ListEnumFromToComp e1 e2 stmts) = freeVarsStmts stmts [e1, e2]
    freeVars' (ListLength e) = freeVars' e
    freeVars' (Map kvs) = freeVars' (map fst kvs) ++ freeVars' (map snd kvs)
    freeVars' (MathsBinaryOp _ e1 e2) = freeVars' [e1,e2]
    freeVars' (MathsUnaryOp _ e1) = freeVars' e1
    freeVars' (Paren e) = freeVars' e
    freeVars' (Set es) = freeVars es
    freeVars' (SetComp es stmts) =
        let
            fvStmts = boundNames stmts
            depsStmts = freeVars stmts
            fvses' = freeVars es
            fvse = nub (fvses'++depsStmts)
        in fvse \\ fvStmts
    freeVars' (SetEnumComp es stmts) =
        let
            fvStmts = boundNames stmts
            depsStmts = freeVars stmts
            fvses' = freeVars es
            fvse = nub (fvses'++depsStmts)
        in fvse \\ fvStmts
    freeVars' (SetEnumFrom e1) = freeVars' e1
    freeVars' (SetEnumFromTo e1 e2) = freeVars' [e1,e2]
    freeVars' (SetEnumFromComp e1 stmts) = freeVarsStmts stmts [e1]
    freeVars' (SetEnumFromToComp e1 e2 stmts) = freeVarsStmts stmts [e1, e2]
    freeVars' (SetEnum es) = freeVars' es
    freeVars' (Tuple es) = freeVars' es
    freeVars' (Var n) = [n]
    
    -- Processes
    freeVars' (AlphaParallel e1 e2 e3 e4) = freeVars' [e1,e2,e3,e4]
    freeVars' (Exception e1 e2 e3) = freeVars' [e1,e2,e3]
    freeVars' (ExternalChoice e1 e2) = freeVars' [e1,e2]
    freeVars' (GenParallel e1 e2 e3) = freeVars' [e1,e2,e3]
    freeVars' (GuardedExp e1 e2) = freeVars' [e1,e2]
    freeVars' (Hiding e1 e2) = freeVars' [e1,e2]
    freeVars' (InternalChoice e1 e2) = freeVars' [e1,e2]
    freeVars' (Interrupt e1 e2) = freeVars' [e1,e2]
    freeVars' (LinkParallel e1 links stmts e2) =
        let
            ds1 = freeVars [e1,e2]
            ds2 = freeVarsStmts stmts (concatMap (\ (x,y) -> x:y:[]) links)
        in ds1++ds2
    freeVars' (Interleave e1 e2) = freeVars' [e1,e2]
    freeVars' (Prefix e1 fields e2) =
        let
            depse = freeVars' [e1,e2]
            depsfields = freeVars' fields
            fvfields = boundNames fields
            fvse = nub (depsfields++depse)
        in fvse \\ fvfields
    freeVars' (Rename e1 renames stmts) =
        let
            (es, es') = unzip renames
            d1 = freeVars' e1
            d2 = freeVarsStmts stmts (es++es')
        in d1++d2
    freeVars' (SequentialComp e1 e2) = freeVars' [e1,e2]
    freeVars' (SlidingChoice e1 e2) = freeVars' [e1,e2]
    freeVars' (SynchronisingExternalChoice e1 e2 e3) = freeVars' [e1,e2,e3]
    freeVars' (SynchronisingInterrupt e1 e2 e3) = freeVars' [e1,e2,e3]

    freeVars' (ReplicatedAlphaParallel stmts e1 e2) = 
        freeVarsStmts stmts [e1,e2]
    freeVars' (ReplicatedInterleave stmts e1) = 
        freeVarsStmts stmts [e1]
    freeVars' (ReplicatedExternalChoice stmts e1) = 
        freeVarsStmts stmts [e1]
    freeVars' (ReplicatedInternalChoice stmts e1) = 
        freeVarsStmts stmts [e1]
    freeVars' (ReplicatedLinkParallel ties tiesStmts stmts e) =
        let
            (es, es') = unzip ties
            d1 = freeVarsStmts tiesStmts (es++es')
            d2 = freeVarsStmts stmts e
            -- The ties may depend on variables bound by stmts too
            fvsstmts = freeVars stmts
        in (d1 \\ fvsstmts)++d2
    freeVars' (ReplicatedParallel e1 stmts e2) =
        freeVars' e1 ++ freeVarsStmts stmts [e2]
    freeVars' (ReplicatedSequentialComp stmts e1) = freeVarsStmts stmts [e1]
    freeVars' (ReplicatedSynchronisingExternalChoice e1 stmts e2) = 
        freeVars' e1 ++ freeVarsStmts stmts [e2]
    
    freeVars' x = panic ("TCFreeVars.hs: unrecognised exp "++show x)

-- Recall that a later stmt can depend on values that appear in an ealier stmt
-- For example, consider <x | x <- ..., f(x)>. Therefore we do a foldr to correctly
-- consider cases like <x | f(x), x <- ... >
freeVarsStmts :: FreeVars a => [TCStmt] -> a -> [Name]
freeVarsStmts [] e = freeVars e
freeVarsStmts (stmt:stmts) e =
    let
        depse = freeVarsStmts stmts e
        depsstmt = freeVars stmt
        fvstmt = boundNames stmt
        depse' = nub (depsstmt++depse)
    in depse' \\ fvstmt

instance FreeVars (Stmt Name) where
    freeVars' (Generator p e) = freeVars p ++ freeVars e
    freeVars' (Qualifier e) = freeVars e

instance FreeVars (Field Name) where
    freeVars' (Input p e) = freeVars p ++ freeVars e
    freeVars' (NonDetInput p e) = freeVars p ++ freeVars e
    freeVars' (Output e) = freeVars e

instance FreeVars (Decl Name) where
    freeVars' (FunBind n ms ta) = freeVars ms ++ freeVars ta
    freeVars' (PatBind p e ta) = freeVars p ++ freeVars e ++ freeVars ta
    freeVars' (Channel ns es ta) = freeVars es ++ freeVars ta
    freeVars' (DataType n cs) = freeVars [cs]
    freeVars' (SubType n cs) =
        concatMap (\ (DataTypeClause n e ta) -> n : freeVars e ++ freeVars ta)
            (map unAnnotate cs)
    freeVars' (NameType n e ta) = freeVars' e ++ freeVars' ta
    freeVars' (External ns) = []
    freeVars' (Transparent ns) = []
    freeVars' (Assert a) = freeVars a
    freeVars' (Module _ [] ds1 ds2) = boundNames (ds1++ds2)
    freeVars' (Module _ _ ds1 ds2) = freeVars' ds1 ++ freeVars' ds2
    freeVars' (TimedSection (Just n) f ds) =
        n : freeVars' f ++ concatMap freeVars' ds
    freeVars' (ModuleInstance _ n args _ _) = n : freeVars' args
    freeVars' (PrintStatement _) = []

instance FreeVars (Assertion Name) where
    freeVars' (Refinement e1 m e2 opts) = freeVars [e1, e2] ++ freeVars opts
    freeVars' (PropertyCheck e1 p m opts) = freeVars [e1] ++ freeVars opts
    freeVars' (ASNot a) = freeVars a

instance FreeVars (ModelOption Name) where
    freeVars' (TauPriority e) = freeVars' e
    freeVars' (PartialOrderReduce _) = []
    
instance FreeVars (Match Name) where
    freeVars' (Match ps e) =
        let
            fvs1 = boundNames ps
            depsPs = freeVars ps
            fvs2 = freeVars e
        in (fvs2 \\ fvs1) ++ depsPs

instance FreeVars (DataTypeClause Name) where
    freeVars' (DataTypeClause n Nothing _) = []
    freeVars' (DataTypeClause n (Just e) _) = freeVars' e

instance FreeVars (STypeScheme Name) where
    freeVars' (STypeScheme ns _ t) = sort (nub (freeVars' t)) \\ ns

instance FreeVars (SType Name) where
    freeVars' (STVar n) = [n]
    freeVars' (STExtendable t n) = n : freeVars' t
    freeVars' (STSet t) = freeVars' t
    freeVars' (STSeq t) = freeVars' t
    freeVars' (STDot t1 t2) = freeVars' t1 ++ freeVars' t2
    freeVars' (STTuple ts) = freeVars ts
    freeVars' (STFunction ts t) = freeVars t ++ freeVars ts
    freeVars' (STDotable t1 t2) = freeVars' [t1, t2]
    freeVars' (STParen t) = freeVars t
    freeVars' (STMap t1 t2) = freeVars [t1, t2]
    freeVars' (STDatatype n) = [n]
    freeVars' STProc = []
    freeVars' STInt = []
    freeVars' STBool = []
    freeVars' STChar = []
    freeVars' STEvent = []
