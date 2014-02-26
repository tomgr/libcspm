{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module CSPM.HaskellEvaluator.ConcretiseAmbiguousTypes (
    ConcretiseAmbiguousTypes(concretiseAmbiguousTypes)
) where

import Control.Monad.State
import qualified Data.Set as S
import Prelude hiding ((&&))

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import Util.Annotated
import Util.Monad (($$))

data ConcretiseAmbiguousTypesState = ConcretiseAmbiguousTypesState {
        allowedTypeVars :: S.Set TypeVar
    }

type ConcretiseAmbiguousTypeMonad = State ConcretiseAmbiguousTypesState

class ConcretiseAmbiguousTypes a where
    concretiseAmbiguousTypes :: a -> a
    concretiseAmbiguousTypes value =
        let (value', st) = runState (concretise value) (ConcretiseAmbiguousTypesState S.empty)
        in value'

    concretise :: a -> ConcretiseAmbiguousTypeMonad a

instance ConcretiseAmbiguousTypes a => ConcretiseAmbiguousTypes [a] where
    concretise xs = mapM concretise xs

instance (ConcretiseAmbiguousTypes a, ConcretiseAmbiguousTypes b) => ConcretiseAmbiguousTypes (a,b) where
    concretise (a, b) = do
        a <- concretise a
        b <- concretise b
        return (a, b)

instance ConcretiseAmbiguousTypes a => ConcretiseAmbiguousTypes (Maybe a) where
    concretise Nothing = return Nothing
    concretise (Just x) = concretise x >>= return . Just

instance ConcretiseAmbiguousTypes a => ConcretiseAmbiguousTypes (Annotated () a) where
    concretise (An a b inner) = concretise inner >>= return . An a b

instance ConcretiseAmbiguousTypes a =>
        ConcretiseAmbiguousTypes (Annotated (Maybe Type, PType) a) where
    concretise (An location (Just typ, typRef) inner) = do
        typ <- concretise typ
        inner <- concretise inner
        return $! An location (Just typ, typRef) inner

instance ConcretiseAmbiguousTypes a =>
        ConcretiseAmbiguousTypes (Annotated (Maybe SymbolTable, PSymbolTable) a) where
    concretise (An location (Just symbolTable, symbolTableRef) inner) = do
        oldTypeVars <- gets allowedTypeVars
        modify (\ st -> st {
                allowedTypeVars = S.union (allowedTypeVars st) newVariables
            })
        inner <- concretise inner
        modify (\st -> st { allowedTypeVars = oldTypeVars })
        return $ An location (Just symbolTable, symbolTableRef) inner
        where
            newVariables = S.fromList
                [tv | ForAll tvs _ <- map snd symbolTable, (tv, _) <- tvs]

instance ConcretiseAmbiguousTypes (CSPMFile Name) where
    concretise (CSPMFile ds) = return CSPMFile $$ concretise ds

instance ConcretiseAmbiguousTypes (Decl Name) where
    concretise (FunBind n ms ta) = return FunBind $$ return n $$ concretise ms $$ return ta
    concretise (PatBind p e ta) = return PatBind $$ concretise p $$ concretise e $$ return ta
    concretise (Assert a) = return Assert $$ concretise a
    concretise (External ns) = return $ External ns
    concretise (Transparent ns) = return $ Transparent ns
    concretise (Channel ns me) = return Channel $$ return ns $$ concretise me
    concretise (DataType n cs) = return DataType $$ return n $$ concretise cs
    concretise (SubType n cs) = return SubType $$ return n $$ concretise cs
    concretise (NameType n e) = return NameType $$ return n $$ concretise e
    concretise (TimedSection mn f ds) = return TimedSection $$ return mn $$ concretise f $$ concretise ds
    concretise (PrintStatement s) = return $ PrintStatement s

instance ConcretiseAmbiguousTypes (Assertion Name) where
    concretise (Refinement e1 m e2 opts) = 
        return Refinement $$ concretise e1 $$ concretise m $$ concretise e2 $$ concretise opts
    concretise (PropertyCheck e p m) = 
        return PropertyCheck $$ concretise e $$ concretise p $$ concretise m
    concretise (ASNot a) = return ASNot $$ concretise a

instance ConcretiseAmbiguousTypes SemanticProperty where
    concretise e = return e
instance ConcretiseAmbiguousTypes Model where
    concretise m = return m
instance ConcretiseAmbiguousTypes (ModelOption Name) where
    concretise (TauPriority e) = return TauPriority $$ concretise e

instance ConcretiseAmbiguousTypes (DataTypeClause Name) where
    concretise (DataTypeClause n me) =
        return DataTypeClause $$ return n $$ concretise me

instance ConcretiseAmbiguousTypes (Match Name) where
    concretise (Match pss e) = return Match $$ concretise pss $$ concretise e

instance ConcretiseAmbiguousTypes (Exp Name) where
    concretise (App e es) = return App $$ concretise e $$ concretise es
    concretise (BooleanBinaryOp op e1 e2) = 
        return (BooleanBinaryOp op) $$ concretise e1 $$ concretise e2
    concretise (BooleanUnaryOp op e) =
        return (BooleanUnaryOp op) $$ concretise e
    concretise (Concat e1 e2) = return Concat $$ concretise e1 $$ concretise e2
    concretise (DotApp e1 e2) = return DotApp $$ concretise e1 $$ concretise e2
    concretise (If e1 e2 e3) = return If $$ concretise e1 $$ concretise e2 $$ concretise e3
    concretise (Lambda p e) = return Lambda $$ concretise p $$ concretise e
    concretise (Let ds e) = return Let $$ concretise ds $$ concretise e
    concretise (Lit l) = return Lit $$ concretise l
    concretise (List es) = return List $$ concretise es
    concretise (ListComp es stmts) = return ListComp $$ concretise es $$ concretise stmts
    concretise (ListEnumFrom e) = return ListEnumFrom $$ concretise e
    concretise (ListEnumFromTo e1 e2) = return ListEnumFromTo $$ concretise e1 $$ concretise e2
    concretise (ListEnumFromComp e stmts) =
        return ListEnumFromComp $$ concretise e $$ concretise stmts
    concretise (ListEnumFromToComp e1 e2 stmts) =
        return ListEnumFromToComp $$ concretise e1 $$ concretise e2 $$ concretise stmts
    concretise (ListLength e) = return ListLength $$ concretise e
    concretise (Map kvs) = return Map $$ concretise kvs
    concretise (MathsBinaryOp op e1 e2) = 
        return (MathsBinaryOp op) $$ concretise e1 $$ concretise e2
    concretise (MathsUnaryOp op e) = return (MathsUnaryOp op) $$ concretise e
    concretise (Paren e) = concretise e >>= return . unAnnotate
    concretise (Set es) = return Set $$ concretise es
    concretise (SetComp es stmts) = return SetComp $$ concretise es $$ concretise stmts
    concretise (SetEnum es) = return SetEnum $$ concretise es
    concretise (SetEnumComp es stmts) = return SetEnumComp $$ concretise es $$ concretise stmts
    concretise (SetEnumFrom e) = return SetEnumFrom $$ concretise e
    concretise (SetEnumFromTo e1 e2) = return SetEnumFromTo $$ concretise e1 $$ concretise e2
    concretise (SetEnumFromComp e stmts) =
        return SetEnumFromComp $$ concretise e $$ concretise stmts
    concretise (SetEnumFromToComp e1 e2 stmts) =
        return SetEnumFromToComp $$ concretise e1 $$ concretise e2 $$ concretise stmts
    concretise (Tuple es) = return Tuple $$ concretise es
    concretise (Var n) = return $ Var n

    concretise (AlphaParallel e1 e2 e3 e4) =
        return AlphaParallel $$ concretise e1 $$ concretise e2 $$ concretise e3 $$ concretise e4
    concretise (Exception e1 e2 e3) =
        return Exception $$ concretise e1 $$ concretise e2 $$ concretise e3
    concretise (ExternalChoice e1 e2) = return ExternalChoice $$ concretise e1 $$ concretise e2
    concretise (GenParallel e1 e2 e3) =
        return GenParallel $$ concretise e1 $$ concretise e2 $$ concretise e3
    concretise (GuardedExp e1 e2) = return GuardedExp $$ concretise e1 $$ concretise e2
    concretise (Hiding e1 e2) = return Hiding $$ concretise e1 $$ concretise e2
    concretise (InternalChoice e1 e2) = return InternalChoice $$ concretise e1 $$ concretise e2
    concretise (Interrupt e1 e2) = return Interrupt $$ concretise e1 $$ concretise e2
    concretise (Interleave e1 e2) = return Interleave $$ concretise e1 $$ concretise e2
    concretise (LinkParallel e1 ties stmts e2) = 
        return LinkParallel $$ concretise e1 $$ concretise ties $$ concretise stmts
                $$ concretise e2
    concretise (Prefix e1 fs e2) = do
        return Prefix $$ concretise e1 $$ concretise fs $$ concretise e2
    concretise (TimedPrefix n e) = do
        return TimedPrefix $$ concretise n $$ concretise e
    concretise (Rename e1 ties stmts) =
        return Rename $$ concretise e1 $$ concretise ties $$ concretise stmts
    concretise (SequentialComp e1 e2) = return SequentialComp $$ concretise e1 $$ concretise e2
    concretise (SlidingChoice e1 e2) = return SlidingChoice $$ concretise e1 $$ concretise e2
    concretise (SynchronisingExternalChoice e1 e2 e3) =
        return SynchronisingExternalChoice $$ concretise e1 $$ concretise e2 $$ concretise e3
    concretise (SynchronisingInterrupt e1 e2 e3) =
        return SynchronisingInterrupt $$ concretise e1 $$ concretise e2 $$ concretise e3
    
    concretise (ReplicatedAlphaParallel stmts e1 e2) =
        return ReplicatedAlphaParallel $$ concretise stmts $$ concretise e1 $$ concretise e2
    concretise (ReplicatedInterleave stmts e) =
        return ReplicatedInterleave $$ concretise stmts $$ concretise e
    concretise (ReplicatedExternalChoice stmts e) =
        return ReplicatedExternalChoice $$ concretise stmts $$ concretise e
    concretise (ReplicatedInternalChoice stmts e) =
        return ReplicatedInternalChoice $$ concretise stmts $$ concretise e
    concretise (ReplicatedParallel stmts e1 e2) =
        return ReplicatedParallel $$ concretise stmts $$ concretise e1 $$ concretise e2
    concretise (ReplicatedLinkParallel ties tiesStmts stmts e) =
        return ReplicatedLinkParallel $$ concretise ties $$ concretise tiesStmts
                $$ concretise stmts $$ concretise e
    concretise (ReplicatedSequentialComp stmts e) =
        return ReplicatedSequentialComp $$ concretise stmts $$ concretise e
    concretise (ReplicatedSynchronisingExternalChoice e1 stmts e2) =
        return ReplicatedSynchronisingExternalChoice $$ concretise e1 $$ concretise stmts $$ concretise e2
    
instance ConcretiseAmbiguousTypes (Field Name) where
    concretise (Output e) = return Output $$ concretise e
    concretise (Input p e) = return Input $$ concretise p $$ concretise e
    concretise (NonDetInput p e) =
        return NonDetInput $$ concretise p $$ concretise e

instance ConcretiseAmbiguousTypes (Stmt Name) where
    concretise (Generator p e) = return Generator $$ concretise p $$ concretise e
    concretise (Qualifier e) = return Qualifier $$ concretise e

instance ConcretiseAmbiguousTypes (InteractiveStmt Name) where
    concretise (Bind d) = return Bind $$ concretise d
    concretise (Evaluate e) = return Evaluate $$ concretise e
    concretise (RunAssertion a) = return RunAssertion $$ concretise a

instance ConcretiseAmbiguousTypes (Pat Name) where
    concretise (PCompList p1 p2 p3) =
        return PCompList $$ concretise p1 $$ concretise p2 $$ return p3
    concretise (PDotApp p1 p2) =
        return PDotApp $$ concretise p1 $$ concretise p2
    concretise (PDoublePattern p1 p2) =
        return PDoublePattern $$ concretise p1 $$ concretise p2
    concretise (PLit l) = return PLit $$ concretise l
    concretise (PParen p) = concretise p >>= return . unAnnotate
    concretise (PSet ps) = return PSet $$ concretise ps
    concretise (PTuple ps) = return PTuple $$ concretise ps
    concretise (PVar n) = return $ PVar n
    concretise (PWildCard) = return PWildCard

instance ConcretiseAmbiguousTypes Literal where
    concretise l = return l

--instance ConcretiseAmbiguousTypes TypeVarRef where
--    concretise tvref = do
--        allowedVariables <- gets allowedTypeVars
--        return $!
--            if S.member (typeVar tvref) allowedVariables then tvref
--            else 

instance ConcretiseAmbiguousTypes Type where
    concretise (TVar tvref) = do
        allowedVariables <- gets allowedTypeVars
        return $!
            if S.member (typeVar tvref) allowedVariables then TVar tvref
            else TInt
    concretise (TSet t) = return TSet $$ concretise t
    concretise (TSeq t) = return TSeq $$ concretise t
    concretise (TDot t1 t2) = return TDot $$ concretise t1 $$ concretise t2
    concretise (TMap t1 t2) = return TMap $$ concretise t1 $$ concretise t2
    concretise (TTuple ts) = return TTuple $$ concretise ts
    concretise (TFunction ts t) = return TFunction $$ concretise ts $$ concretise t
    concretise (TDatatype n) = return $ TDatatype n
    concretise (TDotable t1 t2) = return TDotable $$ concretise t1 $$ concretise t2
    --concretise (TExtendable t tvref) =
        --return TExtendable $$ concretise t $$ concretise tvref
    concretise TInt = return TInt
    concretise TBool = return TBool
    concretise TProc = return TProc
    concretise TEvent = return TEvent
    concretise TChar = return TChar
    concretise TExtendableEmptyDotList = return TExtendableEmptyDotList

instance ConcretiseAmbiguousTypes Name where
    concretise n = return n
