{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module CSPM.Operators.CSP.TypeChecker () where

import Control.Monad
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.Operators.CSP.PrettyPrinter
import CSPM.Operators.CSP.Syntax
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Compressor
import CSPM.TypeChecker.Dependencies
import CSPM.TypeChecker.Expr
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Unification
import Data.List (nub, (\\))
import Util.Annotated
import Util.List
import Util.Monad
import Util.PrettyPrint hiding (($$))

instance TypeCheckable (CSPProcess Name) Type where
    errorContext _ = Nothing
    -- Processes
    typeCheck' (AlphaParallel e1 a1 a2 e2) = do
        ensureIsProc e1
        ensureIsProc e2
        typeCheckExpect a1 (TSet TEvent)
        typeCheckExpect a2 (TSet TEvent)
        return TProc
    typeCheck' (Exception e1 a e2) = do
        ensureIsProc e1
        ensureIsProc e2
        typeCheckExpect a (TSet TEvent)
        return TProc
    typeCheck' (ExternalChoice e1 e2) = do
        ensureIsProc e1
        ensureIsProc e2
        return TProc
    typeCheck' (Hiding e1 e2) = do
        ensureIsProc e1
        typeCheckExpect e2 (TSet TEvent)
        return TProc
    typeCheck' (GenParallel e1 a e2) = do
        ensureIsProc e1
        ensureIsProc e2
        typeCheckExpect a (TSet TEvent)
        return TProc
    typeCheck' (GuardedExp e1 e2) = do
        ensureIsBool e1
        ensureIsProc e2
        return TProc
    typeCheck' (InternalChoice e1 e2) = do
        ensureIsProc e1
        ensureIsProc e2
        return TProc
    typeCheck' (Interrupt e1 e2) = do
        ensureIsProc e1
        ensureIsProc e2
        return TProc
    typeCheck' (Interleave e1 e2) = do
        ensureIsProc e1
        ensureIsProc e2
        return TProc        
    typeCheck' (SequentialComp e1 e2) = do
        ensureIsProc e1
        ensureIsProc e2
        return TProc
    typeCheck' (SlidingChoice e1 e2) = do
        ensureIsProc e1
        ensureIsProc e2
        return TProc
    typeCheck' (Prefix e1 [] e2) = do
        ensureIsEvent e1
        ensureIsProc e2
        return TProc
    typeCheck' (Prefix e1 fields e2) = do
        fvsByField <- mapM (\f -> do
                fvs <- freeVars f
                return (f, fvs)) fields
        let 
            fvs = concatMap snd fvsByField
            namesToLocations = 
                [(n, loc f) | (f, fvs) <- fvsByField, n <- fvs]
        -- Throw an error if a name is defined multiple times
        when (not (noDups fvs)) (panic "Dupes found in prefix after renaming.")

        t1 <- typeCheck e1
        let 
            tcfs [] tsfields = do
                unify TEvent (TDot t1 (foldr1 TDot (reverse tsfields)))
                ensureIsProc e2
            tcfs (f:fs) tsfields =
                typeCheckField f (\ t -> tcfs fs (t:tsfields))
        local fvs (tcfs fields [])

    typeCheck' (LinkParallel e1 ties stmts e2) = do
        ensureIsProc e1
        ensureIsProc e2
        typeCheckReplicatedOp stmts $ do
            let (as, bs) = unzip ties
            ast <- mapM ensureIsChannel as
            zipWithM typeCheckExpect bs ast
            return TProc

    typeCheck' (Rename e1 exps stmts) = do
        ensureIsProc e1
        typeCheckReplicatedOp stmts $ do
            let (as, bs) = unzip exps
            -- Unify the pairs of channels
            ast <- mapM ensureIsChannel as
            zipWithM typeCheckExpect bs ast
            return TProc
            
    -- Replicated Operators
    typeCheck' (ReplicatedAlphaParallel stmts alpha proc) =
        typeCheckReplicatedOp stmts $ do
            t1 <- typeCheck alpha
            unify (TSet TEvent) t1
            ensureIsProc proc
    typeCheck' (ReplicatedParallel alpha stmts proc) =
        typeCheckReplicatedOp stmts $ do
            typeCheckExpect alpha (TSet TEvent)
            ensureIsProc proc
    typeCheck' (ReplicatedLinkParallel ties tiesStmts stmts proc) = do
        typeCheckStmts TSeq stmts $ do
            typeCheckStmts TSet tiesStmts $ do
                let (as, bs) = unzip ties
                ast <- mapM ensureIsChannel as
                zipWithM typeCheckExpect bs ast
                ensureIsProc proc
        return $ TProc
    typeCheck' (ReplicatedInterleave stmts e1) =
        typeCheckReplicatedOp stmts (ensureIsProc e1)
    typeCheck' (ReplicatedExternalChoice stmts e1) =
        typeCheckReplicatedOp stmts (ensureIsProc e1)
    typeCheck' (ReplicatedInternalChoice stmts e1) =
        typeCheckReplicatedOp stmts (ensureIsProc e1)

typeCheckField :: AnField Name -> (Type -> TypeCheckMonad a) -> TypeCheckMonad a
typeCheckField field tc = 
    let
        errCtxt = hang (text "In the field:") tabWidth (prettyPrint field)
        checkInput p e = do
            t <- typeCheck e
            tp <- addErrorContext errCtxt (do
                    -- We don't enforce that tp is Inputable as users are free 
                    -- to do what they wish when they specify the set where 
                    -- the items come from.
                    tp <- typeCheck p
                    unify (TSet tp) t
                    return tp)
            tc tp
        chkInputNoSet p = do
            t <- addErrorContext errCtxt $ do
                    t <- typeCheck p
                    ensureHasConstraint Inputable t
            tc t
        check (NonDetInput p (Just e)) = checkInput p e
        check (Input p (Just e)) = checkInput p e
        check (NonDetInput p Nothing) = chkInputNoSet p
        check (Input p Nothing) = chkInputNoSet p
        check (Output e) = addErrorContext errCtxt (typeCheck e) >>= tc
    in setSrcSpan (loc field) (check (unAnnotate field))

-- | Shortcut for replicated operators
typeCheckReplicatedOp :: 
    (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
            TypeCheckable (p Name) Type)
        => [TCStmt p] -> TypeCheckMonad a -> TypeCheckMonad a
typeCheckReplicatedOp = typeCheckStmts TSet

instance Compressable (CSPField Name) where
    mcompress (Output e) = return Output $$ mcompress e
    mcompress (Input p e) = return Input $$ mcompress p $$ mcompress e
    mcompress (NonDetInput p e) = return NonDetInput $$ mcompress p $$ mcompress e

instance Compressable (CSPProcess Name) where
    mcompress (AlphaParallel e1 e2 e3 e4) = return
        AlphaParallel $$ mcompress e1 $$ mcompress e2 $$ mcompress e3 $$ mcompress e4
    mcompress (Exception e1 e2 e3) = return
        Exception $$ mcompress e1 $$ mcompress e2 $$ mcompress e3
    mcompress (ExternalChoice e1 e2) = return ExternalChoice $$ mcompress e1 $$ mcompress e2
    mcompress (GenParallel e1 e2 e3) = return
        GenParallel $$ mcompress e1 $$ mcompress e2 $$ mcompress e3
    mcompress (GuardedExp e1 e2) = return GuardedExp $$ mcompress e1 $$ mcompress e2
    mcompress (Hiding e1 e2) = return Hiding $$ mcompress e1 $$ mcompress e2
    mcompress (InternalChoice e1 e2) = return InternalChoice $$ mcompress e1 $$ mcompress e2
    mcompress (Interrupt e1 e2) = return Interrupt $$ mcompress e1 $$ mcompress e2
    mcompress (Interleave e1 e2) = return Interleave $$ mcompress e1 $$ mcompress e2
    mcompress (LinkParallel e1 ties stmts e2) = return 
        LinkParallel $$ mcompress e1 $$ mcompress ties $$ mcompress stmts $$ mcompress e2
    mcompress (Prefix e1 fs e2) = return Prefix $$ mcompress e1 $$ mcompress fs $$ mcompress e2
    mcompress (Rename e1 ties stmts) = return
        Rename $$ mcompress e1 $$ mcompress ties $$ mcompress stmts
    mcompress (SequentialComp e1 e2) = return SequentialComp $$ mcompress e1 $$ mcompress e2
    mcompress (SlidingChoice e1 e2) = return SlidingChoice $$ mcompress e1 $$ mcompress e2
    
    mcompress (ReplicatedAlphaParallel stmts e1 e2) =
        return ReplicatedAlphaParallel $$ mcompress stmts $$ mcompress e1 $$ mcompress e2
    mcompress (ReplicatedInterleave stmts e) =
        return ReplicatedInterleave $$ mcompress stmts $$ mcompress e
    mcompress (ReplicatedExternalChoice stmts e) =
        return ReplicatedExternalChoice $$ mcompress stmts $$ mcompress e
    mcompress (ReplicatedInternalChoice stmts e) =
        return ReplicatedInternalChoice $$ mcompress stmts $$ mcompress e
    mcompress (ReplicatedParallel stmts e1 e2) =
        return ReplicatedParallel $$ mcompress stmts $$ mcompress e1 $$ mcompress e2
    mcompress (ReplicatedLinkParallel ties tiesStmts stmts e) =
        return ReplicatedLinkParallel $$ mcompress ties $$ mcompress tiesStmts 
                                        $$ mcompress stmts $$ mcompress e
    

instance Dependencies (CSPProcess Name) where
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

instance Dependencies (CSPField Name) where
    dependencies' (Input p e) = dependencies e
    dependencies' (NonDetInput p e) = dependencies e
    dependencies' (Output e) = dependencies e

instance FreeVars (CSPField Name) where
    freeVars (Input p e) = freeVars p
    freeVars (NonDetInput p e) = freeVars p
    freeVars (Output e) = return []
