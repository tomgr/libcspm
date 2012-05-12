{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module CSPM.TypeChecker.Expr (
    typeCheckStmts,
) where

import Control.Monad
import Control.Monad.Trans
import Data.List

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax hiding (getType)
import CSPM.DataStructures.Types
import CSPM.TypeChecker.BuiltInFunctions
import CSPM.TypeChecker.Common
import {-# SOURCE #-} CSPM.TypeChecker.Decl
import CSPM.TypeChecker.Dependencies
import CSPM.TypeChecker.Exceptions
import CSPM.TypeChecker.Pat
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Unification
import Util.Annotated
import Util.Exception
import Util.List
import Util.Monad
import Util.PrettyPrint

checkFunctionCall ::
    (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
        TypeCheckable (p Name) Type)
    => Doc -> [TCExp p] -> [Type] -> TypeCheckMonad ()
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

instance (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
            TypeCheckable (p Name) Type)
        => TypeCheckable (TCExp p) Type where
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
instance (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
            TypeCheckable (p Name) Type)
        => TypeCheckable (Exp Name p) Type where
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
                    Equals          -> ensureHasConstraint Eq t
                    NotEquals       -> ensureHasConstraint Eq t
                    LessThan        -> ensureHasConstraint Ord t
                    LessThanEq      -> ensureHasConstraint Ord t
                    GreaterThan     -> ensureHasConstraint Ord t
                    GreaterThanEq   -> ensureHasConstraint Ord t
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
    typeCheck' (Lambda p exp) = do
        fvs <- freeVars p
        local fvs $ do
            tr <- typeCheck exp
            targ <- typeCheck p
            return $ TFunction [targ] tr
    typeCheck' (Let decls exp) =
        -- Add a new scope: typeCheckDecl will add vars into it 
        local [] $ do
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
    typeCheck' (ListLength e) = do
        ensureIsList e
        return $ TInt
    typeCheck' (MathsBinaryOp op e1 e2) = do
        ensureIsInt e1
        ensureIsInt e2
        return TInt
    typeCheck' (MathsUnaryOp op e1) = do
        ensureIsInt e1
        return TInt
    typeCheck' (Paren e) = typeCheck e
    typeCheck' (Process p) = typeCheck' p
    typeCheck' (Set es) = do
        t <- ensureAreEqual es
        ensureHasConstraint Eq t
        return $ TSet t
    typeCheck' (SetComp es stmts) = 
        typeCheckStmts TSet stmts $ do
            t <- ensureAreEqual es
            return $ TSet t
    typeCheck' (SetEnum es) =  do
        mapM ensureIsChannel es
        return $ TSet TEvent
    typeCheck' (SetEnumComp es stmts) = 
        typeCheckStmts TSet stmts $ do
            mapM ensureIsChannel es
            return $ TSet TEvent
    typeCheck' (SetEnumFrom lb) = do
        ensureIsInt lb
        return $ TSet TInt
    typeCheck' (SetEnumFromTo lb ub) = do
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
            addWarning (deprecatedNameUsed n r)
        
        b <- isTypeUnsafe n
        when b $ addWarning (unsafeNameUsed n)
        
        t <- getType n
        instantiate t

-- | The first argument is a type constructor, which given a type, returns
-- that type encapsulate in some other type.
typeCheckStmt :: 
    (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
        TypeCheckable (p Name) Type)
        => (Type -> Type) -> TCStmt p -> TypeCheckMonad a -> TypeCheckMonad a
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
typeCheckStmts :: 
    (Eq (p Name), Dependencies (p Name), PrettyPrintable (p Name), 
        TypeCheckable (p Name) Type)
        => (Type -> Type) -> [TCStmt p] -> TypeCheckMonad a -> TypeCheckMonad a
typeCheckStmts typc stmts tc = do
        fvsByStmt <- mapM (\stmt -> do
                fvs <- freeVars stmt
                return (stmt, fvs)) stmts
        let 
            fvs = concatMap snd fvsByStmt
            namesToLocations = 
                [(n, loc f) | (f, fvs) <- fvsByStmt, n <- fvs]
        -- Throw an error if a name is defined multiple times
        when (not (noDups fvs)) (panic "Dupes found in stmts after renaming.")
        -- Renaming ensures uniqueness, so introduce the free vars now.
        local fvs (check stmts)
    where
        check [] = tc
        check (stmt:stmts) = typeCheckStmt typc stmt (check stmts)
