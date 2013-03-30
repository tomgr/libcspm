{-# LANGUAGE FlexibleInstances #-}
module CSPM.Desugar (Desugarable(..), runDesugar) where

import Control.Monad.Reader
import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.PrettyPrinter
import CSPM.Prelude
import Util.Annotated
import Util.Exception
import Util.Monad
import Util.PrettyPrint hiding (($$))

class FreeVars a where
    freeVars :: a -> [Name]

instance FreeVars a => FreeVars (Annotated b a) where
    freeVars (An loc _ inner) = freeVars inner

instance FreeVars (Pat Name) where
    freeVars (PConcat p1 p2) = freeVars p1++freeVars p2
    freeVars (PCompList p1 p2 _) = concatMap freeVars p1 ++ 
        case p2 of
            Just (p2, ps) -> freeVars p2 ++ concatMap freeVars ps
            Nothing -> []
    freeVars (PCompDot ps _) = concatMap freeVars ps
    freeVars (PDotApp p1 p2) = freeVars p1++freeVars p2
    freeVars (PDoublePattern p1 p2) = freeVars p1++freeVars p2
    freeVars (PList ps) = concatMap freeVars ps
    freeVars (PLit l) = []
    freeVars (PParen p) = freeVars p
    freeVars (PSet ps) = concatMap freeVars ps
    freeVars (PTuple ps) = concatMap freeVars ps
    freeVars (PVar n) | isNameDataConstructor n = []
    freeVars (PVar n) = [n]
    freeVars (PWildCard) = []

data DesugarState = DesugarState {
        inTimedSection :: Bool,
        currentLoc :: SrcSpan
    }

type DesugarMonad = ReaderT DesugarState IO

runDesugar :: MonadIO m => DesugarMonad a -> m a
runDesugar prog = liftIO $ runReaderT prog (DesugarState False Unknown)

maybeTimedSection :: DesugarMonad a -> DesugarMonad a -> DesugarMonad a
maybeTimedSection nonTimed timed = do
    b <- asks inTimedSection
    if b then timed else nonTimed

class Desugarable a where
    desugar :: a -> DesugarMonad a

instance Desugarable a => Desugarable [a] where
    desugar xs = mapM desugar xs
instance Desugarable a => Desugarable (Maybe a) where
    desugar Nothing = return Nothing
    desugar (Just a) = desugar a >>= return . Just
instance Desugarable a => Desugarable (Annotated b a) where
    desugar (An l b i) =
        local (\ st -> st { currentLoc = l }) $
            desugar i >>= \ x -> return (An l b x)
instance (Desugarable a, Desugarable b) => Desugarable (a,b) where
    desugar (a,b) = do
        a' <- desugar a
        b' <- desugar b
        return (a', b')

instance Desugarable (CSPMFile Name) where
    desugar (CSPMFile ds) = do
        t <- return CSPMFile $$ desugarDecls ds
        return t

desugarDecl :: TCDecl -> DesugarMonad [TCDecl]
desugarDecl (an@(An x y (PatBind p e ta))) = do
    p' <- desugar p
    e' <- desugar e
    case getSymbolTable an of
        -- Optimise by removing it
        [] -> return []
        [_] -> return [An x y (PatBind p' e' ta)]
        st -> do
            -- We are binding multiple things and thus need to make an extractor
            nameToBindTo <- mkFreshInternalName
            let typeOf n = head [ts | (n', ts) <- st, n' == n]
                expToBindTo = An Unknown dummyAnnotation (Var nameToBindTo)
                mkExtractorPat' n (An a b p) = An a b (mkExtractorPat n p) 
                mkExtractorPat n (PCompList p1 p2 pold) =
                    PCompList (map (mkExtractorPat' n) p1) (
                        case p2 of
                            Just (p, ps) -> Just (mkExtractorPat' n p, map (mkExtractorPat' n) ps)
                            Nothing -> Nothing) pold
                mkExtractorPat n (PCompDot ps pold) =
                    PCompDot (map (mkExtractorPat' n) ps) pold
                mkExtractorPat n (PDoublePattern p1 p2) =
                    PDoublePattern (mkExtractorPat' n p1) (mkExtractorPat' n p2)
                mkExtractorPat n (PLit l) = PLit l
                mkExtractorPat n (PSet ps) = PSet (map (mkExtractorPat' n) ps)
                mkExtractorPat n (PTuple ps) = PTuple (map (mkExtractorPat' n) ps)
                mkExtractorPat n PWildCard = PWildCard
                mkExtractorPat n (PVar n') | n == n' = PVar n
                mkExtractorPat n (PVar n') = PWildCard

                newPSymTableThunk = panic "new psymbole table evaluated"
                mkExtractor n = An (loc an)
                    (Just [(n, typeOf n)], newPSymTableThunk) 
                    (PatBind (mkExtractorPat' n p') expToBindTo Nothing)
                -- TODO: calculate the correct ForAll
                etype = ForAll (panic "incorrect polymorphism") (getType e')
                newPat = An (loc an)
                    (Just [(nameToBindTo, etype)], newPSymTableThunk)
                    (PatBind (An Unknown dummyAnnotation (PVar nameToBindTo)) e'
                        Nothing)
                extractors = map mkExtractor (freeVars p')
            return $ newPat : extractors
desugarDecl (An _ _ (Module n [] ds1 ds2)) = do
    -- We flatten here if there are no arguments (the renamer has
    -- already done the work).
    ds1' <- mapM desugarDecl ds1
    ds2' <- mapM desugarDecl ds2
    return $ concat $ ds1'++ds2'
desugarDecl (An x y d) = do
    d' <- case d of
            FunBind n ms ta -> return (FunBind n) $$ desugar ms $$ return ta
            Assert a -> return Assert $$ desugar a
            External ns -> return $ External ns
            Transparent ns -> return $ Transparent ns
            Channel ns me -> return (Channel ns) $$ desugar me
            DataType n cs -> return (DataType n) $$ desugar cs
            SubType n cs -> return (SubType n) $$ desugar cs
            NameType n e -> return (NameType n) $$ desugar e
            TimedSection (Just n) f ds ->
                return TimedSection $$ return (Just n) $$ desugar f $$
                    local (\st -> st { inTimedSection = True })
                        (concatMapM desugarDecl ds)
    return [An x y d']

desugarDecls :: [TCDecl] -> DesugarMonad [TCDecl]
desugarDecls = concatMapM desugarDecl

instance Desugarable (Assertion Name) where
    desugar (Refinement e1 m e2 opts) = 
        return Refinement $$ desugar e1 $$ desugar m $$ desugar e2 $$ desugar opts
    desugar (PropertyCheck e p m) = 
        return PropertyCheck $$ desugar e $$ desugar p $$ desugar m
    desugar (ASNot a) = return ASNot $$ desugar a

instance Desugarable SemanticProperty where
    desugar e = return e
instance Desugarable Model where
    desugar m = return m
instance Desugarable (ModelOption Name) where
    desugar (TauPriority e) = return TauPriority $$ desugar e

instance Desugarable (DataTypeClause Name) where
    desugar (DataTypeClause n me) = return (DataTypeClause n) $$ desugar me

instance Desugarable (Match Name) where
    desugar (Match pss e) = return Match $$ desugar pss $$ desugar e

instance Desugarable (Exp Name) where
    desugar (App e es) = return App $$ desugar e $$ desugar es
    desugar (BooleanBinaryOp op e1 e2) = 
        return (BooleanBinaryOp op) $$ desugar e1 $$ desugar e2
    desugar (BooleanUnaryOp op e) =
        return (BooleanUnaryOp op) $$ desugar e
    desugar (Concat e1 e2) = return Concat $$ desugar e1 $$ desugar e2
    desugar (DotApp e1 e2) = return DotApp $$ desugar e1 $$ desugar e2
    desugar (If e1 e2 e3) = return If $$ desugar e1 $$ desugar e2 $$ desugar e3
    desugar (Lambda p e) = return Lambda $$ desugar p $$ desugar e
    desugar (Let ds e) = return Let $$ desugarDecls ds $$ desugar e
    desugar (Lit l) = return Lit $$ desugar l
    desugar (List es) = return List $$ desugar es
    desugar (ListComp es stmts) = return ListComp $$ desugar es $$ desugar stmts
    desugar (ListEnumFrom e) = return ListEnumFrom $$ desugar e
    desugar (ListEnumFromTo e1 e2) = return ListEnumFromTo $$ desugar e1 $$ desugar e2
    desugar (ListEnumFromComp e stmts) =
        return ListEnumFromComp $$ desugar e $$ desugar stmts
    desugar (ListEnumFromToComp e1 e2 stmts) =
        return ListEnumFromToComp $$ desugar e1 $$ desugar e2 $$ desugar stmts
    desugar (ListLength e) = return ListLength $$ desugar e
    desugar (MathsBinaryOp op e1 e2) = 
        return (MathsBinaryOp op) $$ desugar e1 $$ desugar e2
    desugar (MathsUnaryOp op e) = return (MathsUnaryOp op) $$ desugar e
    desugar (Paren e) = desugar e >>= return . unAnnotate
    desugar (Set es) = return Set $$ desugar es
    desugar (SetComp es stmts) = return SetComp $$ desugar es $$ desugar stmts
    desugar (SetEnum es) = return SetEnum $$ desugar es
    desugar (SetEnumComp es stmts) = return SetEnumComp $$ desugar es $$ desugar stmts
    desugar (SetEnumFrom e) = return SetEnumFrom $$ desugar e
    desugar (SetEnumFromTo e1 e2) = return SetEnumFromTo $$ desugar e1 $$ desugar e2
    desugar (SetEnumFromComp e stmts) =
        return SetEnumFromComp $$ desugar e $$ desugar stmts
    desugar (SetEnumFromToComp e1 e2 stmts) =
        return SetEnumFromToComp $$ desugar e1 $$ desugar e2 $$ desugar stmts
    desugar (Tuple es) = return Tuple $$ desugar es
    desugar (Var n) | n == builtInName "STOP" =
        maybeTimedSection (return $ Var n) (mkApplication "TSTOP" [])
    desugar (Var n) | n == builtInName "SKIP" =
        maybeTimedSection (return $ Var n) (mkApplication "TSKIP" [])
    desugar (Var n) = return $ Var n

    desugar (AlphaParallel e1 e2 e3 e4) =
        return AlphaParallel $$ desugar e1 $$ desugar e2 $$ desugar e3 $$ desugar e4
    desugar (Exception e1 e2 e3) =
        return Exception $$ desugar e1 $$ desugar e2 $$ desugar e3
    desugar (ExternalChoice e1 e2) = return ExternalChoice $$ desugar e1 $$ desugar e2
    desugar (GenParallel e1 e2 e3) =
        return GenParallel $$ desugar e1 $$ desugar e2 $$ desugar e3
    desugar (GuardedExp e1 e2) = return GuardedExp $$ desugar e1 $$ desugar e2
    desugar (Hiding e1 e2) = return Hiding $$ desugar e1 $$ desugar e2
    desugar (InternalChoice e1 e2) = return InternalChoice $$ desugar e1 $$ desugar e2
    desugar (Interrupt e1 e2) = return Interrupt $$ desugar e1 $$ desugar e2
    desugar (Interleave e1 e2) = return Interleave $$ desugar e1 $$ desugar e2
    desugar (LinkParallel e1 ties stmts e2) = 
        maybeTimedSection
            (return LinkParallel $$ desugar e1 $$ desugar ties $$ desugar stmts
                $$ desugar e2)
            (asks currentLoc >>= \ loc ->
                throwSourceError [linkedParallelTimedSectionError loc])
    desugar (Prefix e1 fs e2) = do
        dp <- return Prefix $$ desugar e1 $$ desugar fs $$ desugar e2
        maybeTimedSection
            (return dp)
            (do
                n <- mkFreshInternalName
                srcSpan <- asks currentLoc
                ptype <- freshPType
                setPType ptype TProc
                return $ TimedPrefix n (An srcSpan (Just TProc, ptype) dp))
    desugar (Rename e1 ties stmts) =
        return Rename $$ desugar e1 $$ desugar ties $$ desugar stmts
    desugar (SequentialComp e1 e2) = return SequentialComp $$ desugar e1 $$ desugar e2
    desugar (SlidingChoice e1 e2) = return SlidingChoice $$ desugar e1 $$ desugar e2
    desugar (SynchronisingExternalChoice e1 e2 e3) =
        return SynchronisingExternalChoice $$ desugar e1 $$ desugar e2 $$ desugar e3
    desugar (SynchronisingInterrupt e1 e2 e3) =
        return SynchronisingInterrupt $$ desugar e1 $$ desugar e2 $$ desugar e3
    
    desugar (ReplicatedAlphaParallel stmts e1 e2) =
        return ReplicatedAlphaParallel $$ desugar stmts $$ desugar e1 $$ desugar e2
    desugar (ReplicatedInterleave stmts e) =
        return ReplicatedInterleave $$ desugar stmts $$ desugar e
    desugar (ReplicatedExternalChoice stmts e) =
        return ReplicatedExternalChoice $$ desugar stmts $$ desugar e
    desugar (ReplicatedInternalChoice stmts e) =
        return ReplicatedInternalChoice $$ desugar stmts $$ desugar e
    desugar (ReplicatedParallel stmts e1 e2) =
        return ReplicatedParallel $$ desugar stmts $$ desugar e1 $$ desugar e2
    desugar (ReplicatedLinkParallel ties tiesStmts stmts e) =
        maybeTimedSection
            (return ReplicatedLinkParallel $$ desugar ties $$ desugar tiesStmts
                $$ desugar stmts $$ desugar e)
            (asks currentLoc >>= \ loc ->
                throwSourceError [linkedParallelTimedSectionError loc])
    desugar (ReplicatedSequentialComp stmts e) =
        return ReplicatedSequentialComp $$ desugar stmts $$ desugar e
    desugar (ReplicatedSynchronisingExternalChoice e1 stmts e2) =
        return ReplicatedSynchronisingExternalChoice $$ desugar e1 $$ desugar stmts $$ desugar e2
    
instance Desugarable (Field Name) where
    desugar (Output e) = return Output $$ desugar e
    desugar (Input p e) = return Input $$ desugar p $$ desugar e
    desugar (NonDetInput p e) =
        maybeTimedSection (return NonDetInput $$ desugar p $$ desugar e)
            (do
                loc <- asks currentLoc
                throwSourceError [nondetFieldTimedSectionError loc])

instance Desugarable (Stmt Name) where
    desugar (Generator p e) = return Generator $$ desugar p $$ desugar e
    desugar (Qualifier e) = return Qualifier $$ desugar e

instance Desugarable (InteractiveStmt Name) where
    desugar (Bind d) = return Bind $$ desugarDecls d
    desugar (Evaluate e) = return Evaluate $$ desugar e
    desugar (RunAssertion a) = return RunAssertion $$ desugar a

instance Desugarable (Pat Name) where
    desugar (PConcat p1 p2) = 
        let
            combine (as1, Just (p, bs1)) (as2, Nothing) = (as1, Just (p, bs1++as2))
            combine (as1, Nothing) (as2, p) = (as1++as2, p)
            combine _ _ = throwSourceError [mkErrorMessage l err]
                where
                    l = loc p1
                    err = prettyPrint (PConcat p1 p2) <+> 
                        text "is not a valid sequence pattern."

            extractCompList :: TCPat -> ([TCPat], Maybe (TCPat, [TCPat]))
            extractCompList (An _ _ (PCompList ps mp _)) = (ps, mp)
            extractCompList p = ([], Just (p, []))
        in do
            p1' <- desugar p1
            p2' <- desugar p2
            let (start, end) = combine (extractCompList p1') (extractCompList p2')
            return $ PCompList start end (PConcat p1 p2)
    desugar (PList ps) =
        return PCompList $$ desugar ps $$ return Nothing $$ return (PList ps)
    desugar (PDotApp p1 p2) = 
        let
            extractDotList (An _ _ (PCompDot ds _)) = ds
            extractDotList d = [d]
        in do
            p1' <- desugar p1
            p2' <- desugar p2
            return $ PCompDot (extractDotList p1'++extractDotList p2') (PDotApp p1 p2)
    desugar (PDoublePattern p1 p2) =
        return PDoublePattern $$ desugar p1 $$ desugar p2
    desugar (PLit (String s)) = return $
        PCompList (map (\ c -> An Unknown () (PLit (Char c))) s)
            Nothing (PLit (String s))
    desugar (PLit l) = return PLit $$ desugar l
    -- We don't remove the Paren as people may pretty print a desugared
    -- expression, which would then not have parenthesis needed to
    -- remove ambiguity
    desugar (PParen p) = desugar p >>= return . unAnnotate
    desugar (PSet []) = return $ PSet []
    desugar (PSet [p]) = return PSet $$ (desugar p >>= (\x -> return [x]))
    desugar (PSet ps) = do
        loc <- asks currentLoc
        throwSourceError [invalidSetPatternMessage (PSet ps) loc]
    desugar (PTuple ps) = return PTuple $$ desugar ps
    desugar (PVar n) = return $ PVar n
    desugar (PWildCard) = return PWildCard

instance Desugarable Literal where
    desugar l = return l

invalidSetPatternMessage :: Pat Name -> SrcSpan -> ErrorMessage
invalidSetPatternMessage pat loc = mkErrorMessage loc $
    text "The pattern" <+> prettyPrint pat <+> 
    text "is invalid as set patterns may only match at most one element."

linkedParallelTimedSectionError :: SrcSpan -> ErrorMessage
linkedParallelTimedSectionError loc = mkErrorMessage loc $
    text "The linked parallel operator is not allowed in a timed section."

nondetFieldTimedSectionError :: SrcSpan -> ErrorMessage
nondetFieldTimedSectionError loc = mkErrorMessage loc $
    text "A non-deterministic input (i.e. $) is not allowed in a timed section."

mkApplication :: String -> [AnExp Name] -> DesugarMonad (Exp Name)
mkApplication fn args = do
    srcSpan <- asks currentLoc
    ptype <- freshPType
    setPType ptype TProc
    return $ App (An srcSpan (Just TProc, ptype) (Var (builtInName fn))) args
