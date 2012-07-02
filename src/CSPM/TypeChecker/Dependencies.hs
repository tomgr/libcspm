{-# LANGUAGE FlexibleInstances #-}
module CSPM.TypeChecker.Dependencies (
    Dependencies, dependencies, namesBoundByDecl, namesBoundByDecl',
    FreeVars, freeVars
) where

import Control.Monad
import Data.List (nub, (\\))

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.TypeChecker.Monad
import Util.Annotated
import Util.Monad

-- This file exports two main type classes, Dependencies and FreeVars. The first
-- computes what names a given expression/pattern/field/etc depends on.
-- The second computes the free variables within each statement.
-- Clearly these depend on each other heavily as a name is free iff
-- it is not a dependency.

namesBoundByDecl :: AnDecl Name -> TypeCheckMonad [Name]
namesBoundByDecl =  namesBoundByDecl' . unAnnotate
namesBoundByDecl' (FunBind n ms) = return [n]
namesBoundByDecl' (PatBind p ms) = freeVars p
namesBoundByDecl' (Channel ns es) = return ns
namesBoundByDecl' (SubType n dcs) = return [n]
namesBoundByDecl' (DataType n dcs) = 
    let
        namesBoundByDtClause (DataTypeClause n _) = [n]
    in
        return $ n:concatMap (namesBoundByDtClause . unAnnotate) dcs
namesBoundByDecl' (NameType n e) = return [n]
namesBoundByDecl' (External ns) = return []
namesBoundByDecl' (Transparent ns) = return []
namesBoundByDecl' (Assert _) = return []
namesBoundByDecl' (Module _ _ ds1 ds2) = do
    nss1 <- mapM namesBoundByDecl ds1
    nss2 <- mapM namesBoundByDecl ds2
    return $ concat (nss1++nss2)

class Dependencies a where
    dependencies :: a -> TypeCheckMonad [Name]
    dependencies xs = liftM nub (dependencies' xs)
    dependencies' :: a -> TypeCheckMonad [Name]

instance Dependencies a => Dependencies [a] where
    dependencies' xs = concatMapM dependencies xs
instance Dependencies a => Dependencies (Maybe a) where
    dependencies' (Just x) = dependencies' x
    dependencies' Nothing = return []
instance Dependencies a => Dependencies (Annotated b a) where
    dependencies' (An _ _ inner) = dependencies inner

instance Dependencies (Pat Name) where
    dependencies' (PVar n) | isNameDataConstructor n = return [n]
    dependencies' (PVar n) = return []
    dependencies' (PConcat p1 p2) = do
        fvs1 <- dependencies' p1
        fvs2 <- dependencies' p2
        return $ fvs1++fvs2
    dependencies' (PDotApp p1 p2) = dependencies' [p1,p2]
    dependencies' (PList ps) = dependencies' ps
    dependencies' (PWildCard) = return []
    dependencies' (PTuple ps) = dependencies' ps
    dependencies' (PSet ps) = dependencies' ps
    dependencies' (PParen p) = dependencies' p
    dependencies' (PLit l) = return []
    dependencies' (PDoublePattern p1 p2) = do
        fvs1 <- dependencies' p1
        fvs2 <- dependencies' p2
        return $ fvs1++fvs2
    
instance Dependencies (Exp Name) where
    dependencies' (App e es) = dependencies' (e:es)
    dependencies' (BooleanBinaryOp _ e1 e2) = dependencies' [e1,e2]
    dependencies' (BooleanUnaryOp _ e) = dependencies' e
    dependencies' (Concat e1 e2) = dependencies' [e1,e2]
    dependencies' (DotApp e1 e2) = dependencies' [e1,e2]
    dependencies' (If e1 e2 e3) = dependencies' [e1,e2, e3]
    dependencies' (Lambda p e) = do
        fvsp <- freeVars p
        depsp <- dependencies p
        fvse <- dependencies e
        return $ (fvse \\ fvsp)++depsp
    dependencies' (Let ds e) = do
        fvsd <- dependencies ds
        newBoundVars <- liftM nub (concatMapM namesBoundByDecl ds)
        fvse <- dependencies e
        return $ nub (fvse++fvsd) \\ newBoundVars
    dependencies' (Lit _) = return []
    dependencies' (List es) = dependencies es
    dependencies' (ListComp es stmts) = do
        fvStmts <- freeVars stmts
        depsStmts <- dependencies stmts
        fvses' <- dependencies es
        let fvse = nub (fvses'++depsStmts)
        return $ fvse \\ fvStmts
    dependencies' (ListEnumFrom e1) = dependencies' e1
    dependencies' (ListEnumFromTo e1 e2) = dependencies' [e1,e2]
    dependencies' (ListLength e) = dependencies' e
    dependencies' (MathsBinaryOp _ e1 e2) = dependencies' [e1,e2]
    dependencies' (MathsUnaryOp _ e1) = dependencies' e1
    dependencies' (Paren e) = dependencies' e
    dependencies' (Set es) = dependencies es
    dependencies' (SetComp es stmts) = do
        fvStmts <- freeVars stmts
        depsStmts <- dependencies stmts
        fvses' <- dependencies es
        let fvse = nub (fvses'++depsStmts)
        return $ fvse \\ fvStmts
    dependencies' (SetEnumComp es stmts) = do
        fvStmts <- freeVars stmts
        depsStmts <- dependencies stmts
        fvses' <- dependencies es
        let fvse = nub (fvses'++depsStmts)
        return $ fvse \\ fvStmts
    dependencies' (SetEnumFrom e1) = dependencies' e1
    dependencies' (SetEnumFromTo e1 e2) = dependencies' [e1,e2]
    dependencies' (SetEnum es) = dependencies' es
    dependencies' (Tuple es) = dependencies' es
    dependencies' (Var n) = return [n]
    
    -- Processes
    dependencies' (AlphaParallel e1 e2 e3 e4) = dependencies' [e1,e2,e3,e4]
    dependencies' (Exception e1 e2 e3) = dependencies' [e1,e2,e3]
    dependencies' (ExternalChoice e1 e2) = dependencies' [e1,e2]
    dependencies' (GenParallel e1 e2 e3) = dependencies' [e1,e2,e3]
    dependencies' (GuardedExp e1 e2) = dependencies' [e1,e2]
    dependencies' (Hiding e1 e2) = dependencies' [e1,e2]
    dependencies' (InternalChoice e1 e2) = dependencies' [e1,e2]
    dependencies' (Interrupt e1 e2) = dependencies' [e1,e2]
    dependencies' (LinkParallel e1 links stmts e2) = do
        ds1 <- dependencies [e1,e2]
        ds2 <- dependenciesStmts stmts (concat (map (\ (x,y) -> x:y:[]) links))
        return $ ds1++ds2
    dependencies' (Interleave e1 e2) = dependencies' [e1,e2]
    dependencies' (Prefix e1 fields e2) = do
        depse <- dependencies' [e1,e2]
        depsfields <- dependencies fields
        fvfields <- freeVars fields
        let fvse = nub (fvfields++depsfields++depse)
        return $ fvse \\ fvfields
    dependencies' (Rename e1 renames stmts) = do
        d1 <- dependencies' e1
        d2 <- dependenciesStmts stmts (es++es')
        return $ d1++d2
        where (es, es') = unzip renames
    dependencies' (SequentialComp e1 e2) = dependencies' [e1,e2]
    dependencies' (SlidingChoice e1 e2) = dependencies' [e1,e2]

    dependencies' (ReplicatedAlphaParallel stmts e1 e2) = 
        dependenciesStmts stmts [e1,e2]
    dependencies' (ReplicatedInterleave stmts e1) = 
        dependenciesStmts stmts [e1]
    dependencies' (ReplicatedExternalChoice stmts e1) = 
        dependenciesStmts stmts [e1]
    dependencies' (ReplicatedInternalChoice stmts e1) = 
        dependenciesStmts stmts [e1]
    dependencies' (ReplicatedLinkParallel ties tiesStmts stmts e) = do
        d1 <- dependenciesStmts tiesStmts (es++es')
        d2 <- dependenciesStmts stmts e
        -- The ties may depend on variables bound by stmts too
        fvsstmts <- freeVars stmts
        return $ (d1 \\ fvsstmts)++d2
        where  (es, es') = unzip ties
    dependencies' (ReplicatedParallel e1 stmts e2) = dependenciesStmts stmts [e1,e2]
    
    dependencies' x = panic ("TCDependencies.hs: unrecognised exp "++show x)

-- Recall that a later stmt can depend on values that appear in an ealier stmt
-- For example, consider <x | x <- ..., f(x)>. Therefore we do a foldr to correctly
-- consider cases like <x | f(x), x <- ... >
dependenciesStmts :: Dependencies a => [TCStmt] -> a -> TypeCheckMonad [Name]
dependenciesStmts [] e = dependencies e
dependenciesStmts (stmt:stmts) e = do
    depse <- dependenciesStmts stmts e
    depsstmt <- dependencies stmt
    fvstmt <- freeVars stmt
    let depse' = nub (depsstmt++depse)
    return $ depse' \\ fvstmt

instance Dependencies (Stmt Name) where
    dependencies' (Generator p e) = do
        ds1 <- dependencies p
        ds2 <- dependencies e
        return $ ds1++ds2
    dependencies' (Qualifier e) = dependencies e

instance Dependencies (Field Name) where
    dependencies' (Input p e) = dependencies e
    dependencies' (NonDetInput p e) = dependencies e
    dependencies' (Output e) = dependencies e

instance Dependencies (Decl Name) where
    dependencies' (FunBind n ms) = do
        fvsms <- dependencies ms
        return $ fvsms
    dependencies' (PatBind p e) = do
        depsp <- dependencies p
        fvsp <- freeVars p
        depse <- dependencies e
        return $ depsp++depse
    dependencies' (Channel ns es) = dependencies es
    dependencies' (DataType n cs) = dependencies [cs]
    dependencies' (SubType n cs) = 
        concatMapM (\ (DataTypeClause n e) -> do
            ds <- dependencies e
            return $ n:ds
        ) (map unAnnotate cs)
    dependencies' (NameType n e) = dependencies' e
    dependencies' (External ns) = return []
    dependencies' (Transparent ns) = return []
    dependencies' (Assert a) = dependencies a
    dependencies' (Module _ _ ds1 ds2) = do
        deps1 <- dependencies ds1
        deps2 <- dependencies ds2
        return $ deps1 ++ deps2

instance Dependencies (Assertion Name) where
    dependencies' (Refinement e1 m e2 opts) = do
        d1 <- dependencies [e1, e2]
        d2 <- dependencies opts
        return $ d1++d2
    dependencies' (PropertyCheck e1 p m) = dependencies [e1]
    dependencies' (ASNot a) = dependencies a

instance Dependencies (ModelOption Name) where
    dependencies' (TauPriority e) = dependencies' e
    
instance Dependencies (Match Name) where
    dependencies' (Match ps e) =
        do
            fvs1 <- freeVars ps
            depsPs <- dependencies ps
            fvs2 <- dependencies e
            return $ (fvs2 \\ fvs1) ++ depsPs

instance Dependencies (DataTypeClause Name) where
    dependencies' (DataTypeClause n Nothing) = return []
    dependencies' (DataTypeClause n (Just e)) = dependencies' e


class FreeVars a where
    freeVars :: a -> TypeCheckMonad [Name]

instance FreeVars a => FreeVars [a] where
    freeVars xs = concatMapM freeVars xs
    
instance FreeVars a => FreeVars (Annotated b a) where
    freeVars (An _ _ inner) = freeVars inner

instance FreeVars (Pat Name) where
    freeVars (PVar n) | isNameDataConstructor n = return []
    freeVars (PVar n) = return [n]
    freeVars (PConcat p1 p2) = do
        fvs1 <- freeVars p1
        fvs2 <- freeVars p2
        return $ fvs1++fvs2
    freeVars (PDotApp p1 p2) = freeVars [p1,p2]
    freeVars (PList ps) = freeVars ps
    freeVars (PWildCard) = return []
    freeVars (PTuple ps) = freeVars ps
    freeVars (PSet ps) = freeVars ps
    freeVars (PParen p) = freeVars p
    freeVars (PLit l) = return []
    freeVars (PDoublePattern p1 p2) = do
        fvs1 <- freeVars p1
        fvs2 <- freeVars p2
        return $ fvs1++fvs2

instance FreeVars (Stmt Name) where
    freeVars (Qualifier e) = return []
    freeVars (Generator p e) = freeVars p

instance FreeVars (Field Name) where
    freeVars (Input p e) = freeVars p
    freeVars (NonDetInput p e) = freeVars p
    freeVars (Output e) = return []
