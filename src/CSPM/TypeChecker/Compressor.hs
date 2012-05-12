-- | Traverses the AST filling in all the type information, ensuring that each
-- type is fully compressed.
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module CSPM.TypeChecker.Compressor(
    Compressable, mcompress
) where

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Types
import CSPM.DataStructures.Syntax
import CSPM.TypeChecker.Monad
import Util.Annotated
import Util.Monad

class Compressable a where
    -- | Map compress.
    mcompress :: a -> TypeCheckMonad a

instance (Compressable a) => Compressable (Annotated (Maybe SymbolTable, PSymbolTable) a) where
    mcompress (An l (_, pt) v) = do
        symbtable <- readPSymbolTable pt
        symbtable' <- mapM (\ (n, t) -> do
            t' <- compressTypeScheme t
            return (n,t')) symbtable
        v' <- mcompress v
        return $ An l (Just symbtable', pt) v'
instance (Compressable a) => Compressable (Annotated (Maybe Type, PType) a) where
    mcompress (An l (_, pt) v) = do
        Just t <- readPType pt
        t' <- compress t
        v' <- mcompress v
        return $ An l (Just t', pt) v'
instance (Compressable a) => Compressable (Annotated () a) where
    mcompress (An l t v) = do
        v' <- mcompress v
        return $ An l t v'

instance Compressable a => Compressable [a] where
    mcompress as = mapM mcompress as
instance Compressable a => Compressable (Maybe a) where
    mcompress Nothing = return Nothing
    mcompress (Just v) = mcompress v >>= return . Just
instance (Compressable a, Compressable b) => Compressable (a, b) where
    mcompress (v1, v2) = do
        v1' <- mcompress v1
        v2' <- mcompress v2
        return (v1', v2')
instance Compressable (p a) => Compressable (Exp a p) where
    mcompress (App e es) = return App $$ mcompress e $$ mcompress es
    mcompress (BooleanBinaryOp op e1 e2) = return 
        (BooleanBinaryOp op) $$ mcompress e1 $$ mcompress e2
    mcompress (BooleanUnaryOp op e) = return
        (BooleanUnaryOp op) $$ mcompress e
    mcompress (Concat e1 e2) = return Concat $$ mcompress e1 $$ mcompress e2
    mcompress (DotApp e1 e2) = return DotApp $$ mcompress e1 $$ mcompress e2
    mcompress (If e1 e2 e3) = return If $$ mcompress e1 $$ mcompress e2 $$ mcompress e3
    mcompress (Lambda p e) = return Lambda $$ mcompress p $$ mcompress e
    mcompress (Let ds e) = return Let $$ mcompress ds $$ mcompress e
    mcompress (Lit l) = return Lit $$ mcompress l
    mcompress (List es) = return List $$ mcompress es
    mcompress (ListComp es stmts) = return ListComp $$ mcompress es $$ mcompress stmts  
    mcompress (ListEnumFrom e) = return ListEnumFrom $$ mcompress e
    mcompress (ListEnumFromTo e1 e2) = return ListEnumFromTo $$ mcompress e1 $$ mcompress e2
    mcompress (ListLength e) = return ListLength $$ mcompress e
    mcompress (MathsBinaryOp op e1 e2) = return 
        (MathsBinaryOp op) $$ mcompress e1 $$ mcompress e2
    mcompress (MathsUnaryOp op e) = return (MathsUnaryOp op) $$ mcompress e
    -- TODO: do we want to do this?
    mcompress (Paren e) = return unAnnotate $$ mcompress e
    mcompress (Process p) = return Process $$ mcompress p
    mcompress (Set es) = return Set $$ mcompress es
    mcompress (SetComp es stmts) = return SetComp $$ mcompress es $$ mcompress stmts
    mcompress (SetEnum es) = return SetEnum $$ mcompress es
    mcompress (SetEnumComp es stmts) = return SetEnumComp $$ mcompress es $$ mcompress stmts
    mcompress (SetEnumFrom e) = return SetEnumFrom $$ mcompress e
    mcompress (SetEnumFromTo e1 e2) = return SetEnumFromTo $$ mcompress e1 $$ mcompress e2
    mcompress (Tuple es) = return Tuple $$ mcompress es
    mcompress (Var n) = return (Var n)

instance Compressable (p a) => Compressable (Module a p) where
    mcompress (GlobalModule ds) = return GlobalModule $$ mcompress ds

instance Compressable (p a) => Compressable (Decl a p) where
    mcompress (FunBind n ms) = return (FunBind n) $$ mcompress ms
    mcompress (PatBind p e) = return PatBind $$ mcompress p $$ mcompress e
    mcompress (Assert a) = return Assert $$ mcompress a
    mcompress (External ns) = return (External ns)
    mcompress (Transparent ns) = return (Transparent ns)
    mcompress (Channel ns me) = return (Channel ns) $$ mcompress me
    mcompress (DataType n cs) = return (DataType n) $$ mcompress cs
    mcompress (NameType n e) = return (NameType n) $$ mcompress e

instance Compressable (p a) => Compressable (Assertion a p) where
    mcompress (Refinement e1 m e2 opts) = return 
        Refinement $$ mcompress e1 $$ mcompress m $$ mcompress e2 $$ mcompress opts
    mcompress (PropertyCheck e p m) = return 
        PropertyCheck $$ mcompress e $$ mcompress p $$ mcompress m

instance Compressable (p a) => Compressable (ModelOption a p) where
    mcompress (TauPriority e) = return TauPriority $$ mcompress e

instance Compressable (p a) => Compressable (DataTypeClause a p) where
    mcompress (DataTypeClause n me) = return (DataTypeClause n) $$ mcompress me

instance Compressable (p a) => Compressable (Match a p) where
    mcompress (Match pss e) = return Match $$ mcompress pss $$ mcompress e

instance Compressable (p a) => Compressable (Stmt a p) where
    mcompress (Generator p e) = return Generator $$ mcompress p $$ mcompress e
    mcompress (Qualifier e) = return Qualifier $$ mcompress e

instance Compressable (p a) => Compressable (InteractiveStmt a p) where
    mcompress (Bind d) = return Bind $$ mcompress d
    mcompress (Evaluate e) = return Evaluate $$ mcompress e
    mcompress (RunAssertion a) = return RunAssertion $$ mcompress a
    
instance Compressable (Pat a) where
    mcompress (PConcat p1 p2) = return PConcat $$ mcompress p1 $$ mcompress p2
    mcompress (PList ps) = return PList $$ mcompress ps
    mcompress (PDotApp p1 p2) = return PDotApp $$ mcompress p1 $$ mcompress p2
    mcompress (PDoublePattern p1 p2) =
        return PDoublePattern $$ mcompress p1 $$ mcompress p2
    mcompress (PLit l) = return PLit $$ mcompress l
    mcompress (PParen p) = return PParen $$ mcompress p
    mcompress (PSet ps) = return PSet $$ mcompress ps
    mcompress (PTuple ps) = return PTuple $$ mcompress ps
    mcompress (PVar n) = return (PVar n)
    mcompress (PWildCard) = return PWildCard

instance Compressable SemanticProperty where
    mcompress l = return l
instance Compressable Model where
    mcompress l = return l
instance Compressable Literal where
    mcompress l = return l
