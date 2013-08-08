{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module CSPM.TypeChecker.Expr () where

import Control.Monad
import Data.List

import CSPM.DataStructures.FreeVars
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax hiding (getType)
import CSPM.DataStructures.Types
import CSPM.TypeChecker.Common
import {-# SOURCE #-} CSPM.TypeChecker.Decl
import CSPM.TypeChecker.Exceptions
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Pat()
import CSPM.TypeChecker.Unification
import Util.Annotated
import Util.List
import Util.PrettyPrint

checkFunctionCall :: Doc -> [TCExp] -> [Type] -> TypeCheckMonad ()
checkFunctionCall func args expectedTypes = do
    actTypes <- mapM typeCheck args
    let
        as = zip4 [1..] args expectedTypes actTypes
        unifyArg (count, arg, texp, tact) = 
            setSrcSpan (loc arg) $ addErrorContext (
                hang (hang (text "In the" <+> speakNth count 
                            <+> text "argument of")
                            tabWidth (func <> comma) )
                    tabWidth (text "namely" <+> prettyPrint arg)
            ) (do
                -- When we computer the type of the function we evaluated all
                -- dots in the arguments. Therefore, here we can evaluate the
                -- dots and remove them all.
                tact <- evaluateDots tact
                -- We also evaluate the dots in the expected type. This is done
                -- because during type checking of a recursive function we may
                -- have not yet have evaluated its dots.
                texp <- evaluateDots texp
                -- We must disallow symmetric unification here as we don't want
                -- to allow patterns such as:
                --     x.y = B
                disallowSymmetricUnification (unify texp tact))
    -- NB. the argument counts must already be correct
    mapM unifyArg as
    return ()

instance TypeCheckable TCExp Type where
    errorContext an = Nothing
    typeCheck' an = do
        t <- setSrcSpan (loc an) $ typeCheck (inner an)
        setPType (snd (annotation an)) t
        return t
    typeCheckExpect an texp = do
        -- The unification is done in typeCheck (inner an)
        t <- setSrcSpan (loc an) $ typeCheckExpect (inner an) texp
        setPType (snd (annotation an)) t
        return t
instance TypeCheckable (Exp Name) Type where
    typeCheckExpect obj texp =
        case errorContext obj of
            Just c -> addErrorContext c m
            Nothing -> m
        where
            m = do
                tact <- typeCheck' obj
                unify texp tact

    errorContext e = Just $ 
        hang (text "In the expression:") tabWidth (prettyPrint e)
    
    typeCheck' (App f args) = do
        targs <- replicateM (length args) freshTypeVar
        tr <- freshTypeVar
        typeCheckExpect f (TFunction targs tr)
        checkFunctionCall (prettyPrint f) args targs
        return tr
    typeCheck' (BooleanBinaryOp op e1 e2) = (do
        case op of
            And -> checkFunctionCall (text "and") [e1,e2] [TBool, TBool]
            Or  -> checkFunctionCall (text "or") [e1,e2] [TBool, TBool]
            _   -> do
                t <- typeCheck e1
                t <- typeCheckExpect e2 t
                case op of  
                    Equals          -> ensureHasConstraint CEq t
                    NotEquals       -> ensureHasConstraint CEq t
                    LessThan        -> ensureHasConstraint COrd t
                    LessThanEq      -> ensureHasConstraint COrd t
                    GreaterThan     -> ensureHasConstraint COrd t
                    GreaterThanEq   -> ensureHasConstraint COrd t
                return ())
        >> return TBool
    typeCheck' (BooleanUnaryOp op e1) = do
        ensureIsBool e1
    typeCheck' (Concat e1 e2) = do
        t1 <- ensureIsList e1
        typeCheckExpect e2 t1
    typeCheck' (DotApp e1 e2) = do
        t1 <- typeCheck e1
        t2 <- typeCheck e2
        return $ TDot t1 t2
    typeCheck' (If e1 e2 e3) = do
        ensureIsBool e1
        t2 <- typeCheck e2
        typeCheckExpect e3 t2
    typeCheck' (Lambda ps exp) = do
        local (boundNames ps) $ do
            tr <- typeCheck exp
            targs <- mapM typeCheck ps
            return $ TFunction targs tr
    typeCheck' (Let decls exp) = do
        -- Add a new scope: typeCheckDecl will add vars into it 
        local (boundNames decls) $ do
            typeCheckDecls decls
            typeCheck exp
    typeCheck' (Lit lit) = typeCheck lit
    typeCheck' (List es) = do
        t <- ensureAreEqual es
        return $ TSeq t
    typeCheck' (ListComp es stmts) =
        typeCheckStmts TSeq stmts $ do
            t <- ensureAreEqual es
            return $ TSeq t
    typeCheck' (ListEnumFrom lb) = do
        ensureIsInt lb
        return $ TSeq TInt
    typeCheck' (ListEnumFromTo lb ub) = do
        ensureIsInt lb
        ensureIsInt ub
        return $ TSeq TInt
    typeCheck' (ListEnumFromComp lb stmts) = do
        typeCheckStmts TSeq stmts $ do
            ensureIsInt lb
            return $ TSeq TInt
    typeCheck' (ListEnumFromToComp lb ub stmts) = do
        typeCheckStmts TSeq stmts $ do
            ensureIsInt lb
            ensureIsInt ub
            return $ TSeq TInt
    typeCheck' (ListLength e) = do
        ensureIsList e
        return $ TInt
    typeCheck' (Map kvs) = do
        k <- ensureAreEqual (map fst kvs)
        v <- ensureAreEqual (map snd kvs)
        return $ TMap k v
    typeCheck' (MathsBinaryOp op e1 e2) = do
        ensureIsInt e1
        ensureIsInt e2
        return TInt
    typeCheck' (MathsUnaryOp op e1) = do
        ensureIsInt e1
        return TInt
    typeCheck' (Paren e) = typeCheck e
    typeCheck' (Set es) = do
        t <- ensureAreEqual es
        ensureHasConstraint CSet t
        return $ TSet t
    typeCheck' (SetComp es stmts) = 
        typeCheckStmts TSet stmts $ do
            t <- ensureAreEqual es
            ensureHasConstraint CSet t
            return $ TSet t
    typeCheck' (SetEnum es) =  do
        fv <- freshTypeVarWithConstraints [CYieldable]
        mapM (flip ensureIsExtendable fv) es
        return $ TSet fv
    typeCheck' (SetEnumComp es stmts) = 
        typeCheckStmts TSet stmts $ do
            fv <- freshTypeVarWithConstraints [CYieldable]
            mapM (flip ensureIsExtendable fv) es
            return $ TSet fv
    typeCheck' (SetEnumFrom lb) = do
        ensureIsInt lb
        return $ TSet TInt
    typeCheck' (SetEnumFromTo lb ub) = do
        ensureIsInt lb
        ensureIsInt ub
        return $ TSet TInt
    typeCheck' (SetEnumFromComp lb stmts) = do
        typeCheckStmts TSet stmts $ do
            ensureIsInt lb
            return $ TSet TInt
    typeCheck' (SetEnumFromToComp lb ub stmts) = do
        typeCheckStmts TSet stmts $ do
            ensureIsInt lb
            ensureIsInt ub
            return $ TSet TInt
    typeCheck' (Tuple es) = do
        ts <- mapM typeCheck es
        return $ TTuple ts
    typeCheck' (Var n) = do
        b <- isDeprecated n
        when b $ do
            r <- replacementForDeprecatedName n
            addWarning warnDeprecatedNamesUsed (deprecatedNameUsed n r)
        
        b <- isTypeUnsafe n
        when b $ addWarning warnUnsafeNamesUsed (unsafeNameUsed n)
        
        t <- getType n
        instantiate t

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
        let 
            fvsByField = map (\f -> (f, boundNames f)) fields
            fvs = concatMap snd fvsByField
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

    typeCheck' (SynchronisingExternalChoice e1 e2 e3) = do
        ensureIsProc e1
        typeCheckExpect e2 (TSet TEvent)
        ensureIsProc e3
        return TProc
    typeCheck' (SynchronisingInterrupt e1 e2 e3) = do
        ensureIsProc e1
        typeCheckExpect e2 (TSet TEvent)
        ensureIsProc e3
        return TProc

    -- Replicated Operators
    typeCheck' (ReplicatedAlphaParallel stmts alpha proc) =
        typeCheckReplicatedOp stmts $ do
            t1 <- typeCheck alpha
            unify (TSet TEvent) t1
            ensureIsProc proc
    typeCheck' (ReplicatedParallel alpha stmts proc) = do
        typeCheckExpect alpha (TSet TEvent)
        typeCheckReplicatedOp stmts $ do
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
    typeCheck' (ReplicatedSequentialComp stmts e1) = do
        typeCheckStmts TSeq stmts (ensureIsProc e1)
        return $ TProc
    typeCheck' (ReplicatedSynchronisingExternalChoice e1 stmts e3) = do
        typeCheckExpect e1 (TSet TEvent)
        typeCheckReplicatedOp stmts $ ensureIsProc e3
    typeCheck' x = panic ("No case for type checking a "++show x)


typeCheckField :: TCField -> (Type -> TypeCheckMonad a) -> TypeCheckMonad a
typeCheckField field tc = 
    let
        errCtxt = hang (text "In the field:") tabWidth (prettyPrint field)
        checkInput p e = do
            t <- typeCheck e
            tp <- addErrorContext errCtxt (do
                    tp <- typeCheck p
                    unify (TSet tp) t
                    ensureHasConstraint CInputable tp
                    return tp)
            tc tp
        chkInputNoSet p = do
            t <- addErrorContext errCtxt $ do
                    t <- typeCheck p
                    ensureHasConstraint CInputable t
            tc t
        check (NonDetInput p (Just e)) = checkInput p e
        check (Input p (Just e)) = checkInput p e
        check (NonDetInput p Nothing) = chkInputNoSet p
        check (Input p Nothing) = chkInputNoSet p
        check (Output e) = addErrorContext errCtxt (typeCheck e) >>= tc
    in setSrcSpan (loc field) (check (unAnnotate field))

-- | The first argument is a type constructor, which given a type, returns
-- that type encapsulate in some other type.
typeCheckStmt :: (Type -> Type) -> TCStmt -> TypeCheckMonad a -> TypeCheckMonad a
typeCheckStmt typc stmt tc = 
    let
        errCtxt = hang (text "In the statement of a comprehension:") tabWidth
                        (prettyPrint stmt)
        
        check (Qualifier e) = do
            addErrorContext errCtxt (ensureIsBool e)
            tc
        check (Generator p exp) = do
            texp <- addErrorContext errCtxt (typeCheck exp)
            addErrorContext errCtxt (do
                tpat <- typeCheck p
                unify (typc tpat) texp)
            tc
    in setSrcSpan (loc stmt) (check (unAnnotate stmt))

-- | Type check a series of statements. For each statement a new scope is added
-- to ensure that clauses only depend on variables already bound.
typeCheckStmts :: (Type -> Type) -> [TCStmt] -> TypeCheckMonad a -> TypeCheckMonad a
typeCheckStmts typc stmts tc = do
        -- Renaming ensures uniqueness, so introduce the free vars now.
        local (boundNames stmts) (check stmts)
    where
        check [] = tc
        check (stmt:stmts) = typeCheckStmt typc stmt (check stmts)

-- | Shortcut for replicated operators
typeCheckReplicatedOp :: [TCStmt] -> TypeCheckMonad a -> TypeCheckMonad a
typeCheckReplicatedOp = typeCheckStmts TSet
