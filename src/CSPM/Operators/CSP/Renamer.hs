{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module CSPM.Operators.CSP.Renamer () where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Operators.CSP.Syntax
import CSPM.Renamer
import Util.Annotated
import Util.Monad

instance FreeVars (CSPField UnRenamedName) where
    freeVars (Input p e) = freeVars p
    freeVars (NonDetInput p e) = freeVars p
    freeVars (Output e) = return []

instance Renamable (CSPProcess UnRenamedName) (CSPProcess Name) where
    rename (AlphaParallel e1 e2 e3 e4) = return
        AlphaParallel $$ rename e1 $$ rename e2 $$ rename e3 $$ rename e4
    rename (Exception e1 e2 e3) = return
        Exception $$ rename e1 $$ rename e2 $$ rename e3
    rename (ExternalChoice e1 e2) = return ExternalChoice $$ rename e1 $$ rename e2
    rename (GenParallel e1 e2 e3) = return
        GenParallel $$ rename e1 $$ rename e2 $$ rename e3
    rename (GuardedExp e1 e2) = return GuardedExp $$ rename e1 $$ rename e2
    rename (Hiding e1 e2) = return Hiding $$ rename e1 $$ rename e2
    rename (InternalChoice e1 e2) = return InternalChoice $$ rename e1 $$ rename e2
    rename (Interrupt e1 e2) = return Interrupt $$ rename e1 $$ rename e2
    rename (Interleave e1 e2) = return Interleave $$ rename e1 $$ rename e2
    rename (LinkParallel e1 ties stmts e2) = do
        e1' <- rename e1
        e2' <- rename e2
        (stmts', ties') <- renameStatements stmts (rename ties)
        return $ LinkParallel e1' ties' stmts' e2'
    rename (Prefix e1 fs e2) = do
        e1' <- rename e1
        (fs', e2') <- renameFields fs (rename e2)
        return $ Prefix e1' fs' e2'
    rename (Rename e1 ties stmts) = do
        e1' <- rename e1
        (stmts', ties') <- renameStatements stmts (rename ties)
        return $ Rename e1' ties' stmts'
    rename (SequentialComp e1 e2) = return SequentialComp $$ rename e1 $$ rename e2
    rename (SlidingChoice e1 e2) = return SlidingChoice $$ rename e1 $$ rename e2
    
    rename (ReplicatedAlphaParallel stmts e1 e2) = do
        (stmts', (e1', e2')) <- renameStatements stmts (do
            e1' <- rename e1
            e2' <- rename e2
            return (e1', e2'))
        return $ ReplicatedAlphaParallel stmts' e1' e2'
    rename (ReplicatedInterleave stmts e) = do
        (stmts', e') <- renameStatements stmts (rename e)
        return $ ReplicatedInterleave stmts' e'
    rename (ReplicatedExternalChoice stmts e) = do
        (stmts', e') <- renameStatements stmts (rename e)
        return $ ReplicatedExternalChoice stmts' e'
    rename (ReplicatedInternalChoice stmts e) = do
        (stmts', e') <- renameStatements stmts (rename e)
        return $ ReplicatedInternalChoice stmts' e'
    rename (ReplicatedParallel e1 stmts e2) = do
        e1' <- rename e1
        (stmts', e2') <- renameStatements stmts (rename e2)
        return $ ReplicatedParallel e1' stmts' e2'
    rename (ReplicatedLinkParallel ties tiesStmts stmts e) = do
        (stmts', (e', ties', tiesStmts')) <- renameStatements stmts (do
            e' <- rename e
            (tiesStmts', ties') <- renameStatements tiesStmts (rename ties)
            return (e', ties', tiesStmts'))
        return $ ReplicatedLinkParallel ties' tiesStmts' stmts' e'


renameFields :: [AnField UnRenamedName] -> RenamerMonad a -> RenamerMonad ([AnField Name], a)
renameFields fs inner = do
    checkDuplicates fs
    -- No duplicates, so we can just add one scope
    addScope (do
        -- We do the fields left to right, to ensure that the scoping is correct

        -- Recall that c?x$y is equiv to |~| y:... @ [] x:... @ ... and hence
        -- the nondet fields bind first.

        -- Firstly, we do the nondet fields.
        fsNonDet <- mapM (\afd -> 
                case unAnnotate afd of
                    Output e -> return Nothing
                    Input p me -> return Nothing
                    NonDetInput p me -> do
                        me' <- rename me
                        p' <- renamePattern internalNameMaker p
                        return $ Just $ reAnnotatePure afd $ NonDetInput p' me') fs

        fsDet <- mapM (\ afd ->
                case unAnnotate afd of
                    Output e -> do
                        e' <- rename e
                        return $ Just $ reAnnotatePure afd $ Output e'
                    Input p me -> do
                        -- Rename me first, as it can't depend on p
                        me' <- rename me
                        p' <- renamePattern internalNameMaker p
                        return $ Just $ reAnnotatePure afd $ Input p' me'
                    NonDetInput p me -> return Nothing) fs
        
        let 
            combineJusts :: [Maybe a] -> [Maybe a] -> [a]
            combineJusts [] [] = []
            combineJusts (Just x:xs) (Nothing:ys) = x:combineJusts xs ys
            combineJusts (Nothing:xs) (Just y:ys) = y:combineJusts xs ys

            fs' :: [AnField Name]
            fs' = combineJusts fsNonDet fsDet

        -- all fields renamed, now rename the inner thing
        a <- inner
        return (fs', a))
