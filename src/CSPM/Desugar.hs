{-# LANGUAGE FlexibleInstances #-}
module CSPM.Desugar (Desugarable(..), runDesugar) where

import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.List (nub, sort)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S

import CSPM.PrettyPrinter
import CSPM.Prelude
import CSPM.Syntax.AST hiding (timedSectionTockName)
import CSPM.Syntax.Literals
import CSPM.Syntax.FreeVars
import CSPM.Syntax.Names
import CSPM.Syntax.Types
import Util.Annotated
import Util.Exception
import Util.Monad
import Util.PrettyPrint hiding (($$))

data DesugarState = DesugarState {
        inTimedSection :: Bool,
        timedSectionTockName :: Maybe Name,
        currentLoc :: SrcSpan,
        variableSubstitution :: M.Map Name Name,
        parentSubstitutionForModules :: M.Map Name (M.Map Name Name)
    }

type DesugarMonad = StateT DesugarState IO

runDesugar :: MonadIO m => DesugarMonad a -> m a
runDesugar prog = liftIO $
    runStateT prog (DesugarState False Nothing Unknown M.empty M.empty)
    >>= return . fst

maybeTimedSection :: DesugarMonad a -> (Name -> DesugarMonad a) -> DesugarMonad a
maybeTimedSection nonTimed timed = do
    b <- gets inTimedSection
    if b then do
        Just tn <- gets timedSectionTockName
        timed tn
    else nonTimed

timedSection :: Name -> DesugarMonad a -> DesugarMonad a
timedSection tockName prog = do
    inTimed <- gets inTimedSection
    oldTockName <- gets timedSectionTockName
    modify (\st -> st {
            inTimedSection = True,
            timedSectionTockName = Just tockName
        })
    a <- prog
    modify (\st -> st {
            inTimedSection = inTimed,
            timedSectionTockName = oldTockName
        })
    return a

addParentModuleSubstitution :: Name -> M.Map Name Name -> DesugarMonad ()
addParentModuleSubstitution mn sub = modify (\ st -> st {
        parentSubstitutionForModules =
            M.insert mn sub (parentSubstitutionForModules st)
    })

parentModuleSubstitution :: Name -> DesugarMonad (M.Map Name Name)
parentModuleSubstitution mn = do
    m <- gets parentSubstitutionForModules
    return $! case M.lookup mn m of
                Nothing -> M.empty
                Just xs -> xs

composeSubstitutions :: M.Map Name Name -> M.Map Name Name -> M.Map Name Name
composeSubstitutions oldMap newMap =
    M.mapWithKey combine (M.union oldMap newMap)
    where combine oldValue newValue =
            case M.lookup newValue newMap of
                Nothing -> newValue
                Just x -> x

bindSubtitution :: M.Map Name Name -> DesugarMonad a -> DesugarMonad a
bindSubtitution varMap prog = do
    sub <- gets variableSubstitution
    modify (\ st -> st { variableSubstitution = composeSubstitutions sub varMap })
    a <- prog
    modify (\ st -> st { variableSubstitution = sub })
    return a

substituteName :: Name -> DesugarMonad Name
substituteName n = do
    vm <- gets variableSubstitution
    case M.lookup n vm of
        Nothing -> return $ n
        Just mn -> return $ mn

class Desugarable a where
    desugar :: a -> DesugarMonad a

instance Desugarable a => Desugarable [a] where
    desugar xs = mapM desugar xs
instance Desugarable a => Desugarable (Maybe a) where
    desugar Nothing = return Nothing
    desugar (Just a) = desugar a >>= return . Just

instance Desugarable a => Desugarable (Annotated (Type, PType) a) where
    desugar (An l (t, pt) i) = do
        modify (\ st -> st { currentLoc = l })
        x <- desugar i
        y <- desugar t
        return (An l (y, pt) x)

instance Desugarable a => Desugarable (Annotated () a) where
    desugar (An l b i) = do
        modify (\ st -> st { currentLoc = l })
        desugar i >>= \ x -> return (An l b x)

instance (Desugarable a, Desugarable b) => Desugarable (a,b) where
    desugar (a,b) = do
        a' <- desugar a
        b' <- desugar b
        return (a', b')

instance Desugarable (CSPMFile Name) where
    desugar (CSPMFile ds) = return CSPMFile $$ do
        ds <- desugarDecls ds
        return ds

desugarDecl :: TCDecl -> DesugarMonad [TCDecl]
desugarDecl (an@(An x y (PatBind p e ta))) = do
    p' <- desugar p
    e' <- desugar e
    case getSymbolTable an of
        -- Optimise by removing it
        [] -> return []
        [_] -> return [An x y (PatBind p' e' Nothing)]
        st -> do
            -- We are binding multiple things and thus need to make an extractor
            nameToBindTo <- mkFreshInternalName
            let typeOf n = head [ts | (n', ts) <- st, n' == n]
                expToBindTo = An Unknown (annotation e') (Var nameToBindTo)
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
                mkExtractorPat _ (PVar n') | isNameDataConstructor n' = PVar n'
                mkExtractorPat n (PVar n') | n == n' = PVar n
                mkExtractorPat n (PVar n') = PWildCard

                newPSymTableThunk = panic "new psymbole table evaluated"
                mkExtractor n = An (loc an)
                    ([(n, typeOf n)], newPSymTableThunk) 
                    (PatBind (mkExtractorPat' n p') expToBindTo Nothing)
                -- TODO: calculate the correct ForAll
                etype = ForAll (nub (sort (concat [ts | (_, ForAll ts _) <- st]))) (getType e')
                newPat = An (loc an)
                    ([(nameToBindTo, etype)], newPSymTableThunk)
                    (PatBind (An Unknown (annotation e') (PVar nameToBindTo)) e' Nothing)
                extractors = map mkExtractor (boundNames p')
            return $ newPat : extractors
desugarDecl (An _ _ (Module n [] ds1 ds2)) = do
    -- We flatten here if there are no arguments (the renamer has
    -- already done the work).
    desugarDecls (ds1++ds2)
desugarDecl (d@(An _ _ (Module _ _ _ _))) = return []
desugarDecl (d@(An _ _ (ModuleInstance nax nt args nm (Just mod)))) = do
    -- We flatten here by creating new names for the arguments, substituting
    -- these in the bodies of ds1 and ds2, and then binding them using patterns.
    parentSub <- parentModuleSubstitution nt
    let An _ _ (Module _ pats ds1 ds2) = mod
        fvs = nub (sort (concatMap boundNames pats))
    freshVars <- replicateM (length fvs) mkFreshInternalName
    let sub = composeSubstitutions parentSub $!
                M.union nm (M.fromList (zip fvs freshVars))
    mapM_ (\ ad -> case unAnnotate ad of
        Module mName _ _ _ ->
            case M.lookup mName sub of
                Just m -> addParentModuleSubstitution m sub
                Nothing -> panic "Could not find module"
        _ -> return ()) (ds1 ++ ds2)
    bindSubtitution sub $ do
        vm <- gets variableSubstitution
        let extractTypes' (PVar n) t = [(n, t)]
            extractTypes' (PConcat p1 p2) (TSeq t) =
                extractTypes p1 t ++ extractTypes p2 t
            extractTypes' (PDotApp p1 p2) (TDot t1 t2) =
                extractTypes p1 t1 ++ extractTypes p2 t2
            extractTypes' (PList ps) (TSeq t) =
                concatMap (\ p -> extractTypes p t) ps
            extractTypes' (PWildCard) _ = []
            extractTypes' (PTuple ps) (TTuple ts) =
                concat (zipWith extractTypes ps ts)
            extractTypes' (PSet ps) (TSet t) =
                concatMap (\ p -> extractTypes p t) ps
            extractTypes' (PParen p) t = extractTypes p t
            extractTypes' (PLit l) _ = []
            extractTypes' (PDoublePattern p1 p2) t =
                extractTypes p1 t ++ extractTypes p2 t
            extractTypes = extractTypes' . unAnnotate

            makePatBind :: TCPat -> TCExp -> TCDecl
            makePatBind p e =
                let fvs = boundNames p
                    nts = extractTypes p (getType e)
                    symTable = [(fromJust (M.lookup n sub), ForAll [] t)
                                    | (n, t) <- nts, n `elem` fvs]
                in An (loc p)
                    (symTable, panic "New symbol table evaluated")
                    (PatBind p e Nothing)
            patBinds = zipWith makePatBind pats args
        desugarDecls $ patBinds ++ ds1 ++ ds2
desugarDecl (An x y (Transparent ns)) =
    concatMapM makeAliasDefinition ns
desugarDecl (An x y (External ns)) = 
    concatMapM makeAliasDefinition ns
desugarDecl (An x y d) = do
    d' <- case d of
            FunBind n ms ta ->
                return FunBind $$ substituteName n $$ desugar ms $$ return ta
            Assert a -> return Assert $$ desugar a
            External ns -> return External $$ mapM substituteName ns
            Transparent ns -> return Transparent $$ mapM substituteName ns
            Channel ns me ta ->
                return Channel $$ mapM substituteName ns $$ desugar me $$ return ta
            DataType n cs -> return DataType $$ substituteName n $$ desugar cs
            SubType n cs -> return SubType $$ substituteName n $$ desugar cs
            NameType n e ta -> return NameType $$ substituteName n $$ desugar e $$ return ta
            TimedSection (Just n) f ds -> do
                tn <- substituteName n
                return TimedSection $$ (return $ Just tn) $$
                    desugar f $$ timedSection tn (desugarDecls ds)
            PrintStatement s -> return $ PrintStatement s
    return [An x y d']

makeAliasDefinition :: Name -> DesugarMonad [TCDecl]
makeAliasDefinition n = do
    n' <- substituteName n
    if n' == n then return []
    else do
        let newPSymTableThunk = panic "new psymbol table evaluated"
            typs@(ForAll _ typ) = typeScheme (builtInWithName n)
        return [
            An Unknown
                ([(n', typs)], newPSymTableThunk)
                (PatBind
                    (An Unknown (typ, dummyAnnotation) (PVar n'))
                    (An Unknown (typ, dummyAnnotation) (Var n))
                    Nothing)
            ]

desugarDecls :: [TCDecl] -> DesugarMonad [TCDecl]
desugarDecls ds = do
    let desugarSymbolTable (An x (st, y) d) = do
            st <- mapM (\ (n, t) -> do
                n <- substituteName n
                t <- desugar t
                return $! (n, t)) st
            return $ An x (st, y) d
    ds <- mapM desugarSymbolTable ds
    concatMapM desugarDecl ds

instance Desugarable (Assertion Name) where
    desugar (Refinement e1 m e2 opts) = 
        return Refinement $$ desugar e1 $$ desugar m $$ desugar e2 $$ desugar opts
    desugar (PropertyCheck e p m opts) = 
        return PropertyCheck $$ desugar e $$ desugar p $$ desugar m $$ desugar opts
    desugar (ASNot a) = return ASNot $$ desugar a

instance Desugarable (SemanticProperty Name) where
    desugar (HasTrace evs) = return HasTrace $$ desugar evs
    desugar e = return e
instance Desugarable Model where
    desugar m = return m
instance Desugarable (ModelOption Name) where
    desugar (TauPriority e) = return TauPriority $$ desugar e
    desugar (PartialOrderReduce m) = return $ PartialOrderReduce m

instance Desugarable (DataTypeClause Name) where
    desugar (DataTypeClause n me ta) =
        return DataTypeClause $$ substituteName n $$ desugar me $$ return ta

instance Desugarable (Match Name) where
    desugar (Match pss e) = do
        pss <- desugar pss
        e <- desugar e
        let usedVars = S.fromList $ freeVars e
            pss' = map (map (restrictPatternsToVars usedVars)) pss
        return $! Match pss' e

instance Desugarable (Exp Name) where
    desugar (App e es) = return App $$ desugar e $$ desugar es
    desugar (BooleanBinaryOp op e1 e2) = 
        return (BooleanBinaryOp op) $$ desugar e1 $$ desugar e2
    desugar (BooleanUnaryOp op e) =
        return (BooleanUnaryOp op) $$ desugar e
    desugar (Concat e1 e2) = return Concat $$ desugar e1 $$ desugar e2
    desugar (DotApp e1 e2) = return DotApp $$ desugar e1 $$ desugar e2
    desugar (If e1 e2 e3) = return If $$ desugar e1 $$ desugar e2 $$ desugar e3
    desugar (Lambda p e) = do
        p <- desugar p
        e <- desugar e
        let fvs = S.fromList $ freeVars e
        return $! Lambda (map (restrictPatternsToVars fvs) p) e
    desugar (Let ds e) = do
        ds <- desugarDecls ds
        e <- desugar e
        -- Remove unused let bindings 
        let usedVars = S.fromList $ freeVars ds ++ freeVars e
            ds' = filter (\ d -> or (map (\n -> S.member n usedVars) (boundNames d))) ds
        case ds' of
            [] -> return $! unAnnotate e
            _ -> return $! Let ds' e
    desugar (Lit l) = return Lit $$ desugar l
    desugar (List es) = return List $$ desugar es
    desugar (ListComp es stmts) = do
        es <- desugar es
        stmts <- desugar stmts
        return $! ListComp es (restrictStatements es stmts)
    desugar (ListEnumFrom e) = return ListEnumFrom $$ desugar e
    desugar (ListEnumFromTo e1 e2) = return ListEnumFromTo $$ desugar e1 $$ desugar e2
    desugar (ListEnumFromComp e stmts) = do
        e <- desugar e
        stmts <- desugar stmts
        return $! ListEnumFromComp e (restrictStatements e stmts)
    desugar (ListEnumFromToComp e1 e2 stmts) = do
        e1 <- desugar e1
        e2 <- desugar e2
        stmts <- desugar stmts
        return $! ListEnumFromToComp e1 e2 (restrictStatements [e1, e2] stmts)
    desugar (ListLength e) = return ListLength $$ desugar e
    desugar (Map kvs) = return Map $$ desugar kvs
    desugar (MathsBinaryOp op e1 e2) = 
        return (MathsBinaryOp op) $$ desugar e1 $$ desugar e2
    desugar (MathsUnaryOp op e) = return (MathsUnaryOp op) $$ desugar e
    desugar (Paren e) = desugar e >>= return . unAnnotate
    desugar (Set es) = return Set $$ desugar es
    desugar (SetComp es stmts) = do
        es <- desugar es
        stmts <- desugar stmts
        return $! SetComp es (restrictStatements es stmts)
    desugar (SetEnum es) = return SetEnum $$ desugar es
    desugar (SetEnumComp es stmts) = do
        es <- desugar es
        stmts <- desugar stmts
        return $! SetEnumComp es (restrictStatements es stmts)
    desugar (SetEnumFrom e) = return SetEnumFrom $$ desugar e
    desugar (SetEnumFromTo e1 e2) = return SetEnumFromTo $$ desugar e1 $$ desugar e2
    desugar (SetEnumFromComp e stmts) = do
        e <- desugar e
        stmts <- desugar stmts
        return $! SetEnumFromComp e (restrictStatements e stmts)
    desugar (SetEnumFromToComp e1 e2 stmts) = do
        e1 <- desugar e1
        e2 <- desugar e2
        stmts <- desugar stmts
        return $! SetEnumFromToComp e1 e2 (restrictStatements [e1, e2] stmts)
    desugar (Tuple es) = return Tuple $$ desugar es
    desugar (Var n) | n == builtInName "timed_priority" =
        maybeTimedSection (return $ Var n)
            (\ tn -> do
                tock <- mkVar tn TEvent
                mkApplication (builtInName "timed_priority")
                    (TFunction [TEvent] (TFunction [TProc] TProc)) [tock])
    desugar (Var n) | n == builtInName "WAIT" =
        maybeTimedSection (return $ Var n)
            (\ tn -> do
                tock <- mkVar tn TEvent
                mkApplication (builtInName "WAIT")
                    (TFunction [TEvent] (TFunction [TInt] TProc)) [tock])
    desugar (Var n) | n == builtInName "STOP" =
        maybeTimedSection (return $ Var n)
            (\ tn -> do
                tock <- mkVar tn TEvent
                mkApplication (builtInName "TSTOP")
                    (TFunction [TEvent] TProc) [tock])
    desugar (Var n) | n == builtInName "SKIP" =
        maybeTimedSection
            (return $ Var n)
            (\ tn -> do
                tock <- mkVar tn TEvent
                mkApplication (builtInName "TSKIP")
                    (TFunction [TEvent] TProc) [tock])
    desugar (Var n) | S.member n locatedBuiltins = do
        loc <- gets currentLoc
        lit <- mkLit (Loc loc)
        mkApplication n (error "TODO") [lit]
    desugar (Var n) = substituteName n >>= return . Var

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
            (\ _ -> do
                loc <- gets currentLoc
                throwSourceError [linkedParallelTimedSectionError loc])
    desugar (Prefix e1 fs e2) = do
        e1 <- desugar e1
        fs <- desugar fs
        e2 <- desugar e2

        -- Restrict patterns
        let namesToLeave = S.fromList $ freeVars fs ++ freeVars e2
        
            patIsAllowed (An _ _ (PDoublePattern _ _)) = False
            patIsAllowed (An _ _ (PCompDot ps _)) = and (map patIsAllowed ps)
            patIsAllowed _ = True
            
            checkPattern p =
                if patIsAllowed p then return () else do
                loc <- gets currentLoc
                throwSourceError [doublePatternInPrefixError loc p]
            
            restrictField (An a b (Input p e)) = do
                checkPattern p
                return $ An a b (Input (restrictPatternsToVars namesToLeave p) e)
            restrictField (An a b (NonDetInput p e)) = do
                checkPattern p
                return $ An a b (NonDetInput (restrictPatternsToVars namesToLeave p) e)
            restrictField (An a b (Output e)) = return $ An a b (Output e)

        fs <- mapM restrictField fs
        let dp = Prefix e1 fs e2
        maybeTimedSection
            (return dp)
            (\ _ -> do
                n <- mkFreshInternalName
                srcSpan <- gets currentLoc
                ptype <- freshPType
                setPType ptype TProc
                return $ TimedPrefix n (An srcSpan (TProc, ptype) dp))
    desugar (Project e1 e2) = return Project $$ desugar e1 $$ desugar e2
    desugar (Rename e1 ties stmts) = do
        e1 <- desugar e1
        ties <- desugar ties
        stmts <- desugar stmts
        let fvs = S.fromList $ freeVars e1 ++ freeVars (map fst ties++map snd ties)
        return $! Rename e1 ties (restrictStatementsToVars fvs stmts)
    desugar (SequentialComp e1 e2) = return SequentialComp $$ desugar e1 $$ desugar e2
    desugar (SlidingChoice e1 e2) = return SlidingChoice $$ desugar e1 $$ desugar e2
    desugar (SynchronisingExternalChoice e1 e2 e3) =
        return SynchronisingExternalChoice $$ desugar e1 $$ desugar e2 $$ desugar e3
    desugar (SynchronisingInterrupt e1 e2 e3) =
        return SynchronisingInterrupt $$ desugar e1 $$ desugar e2 $$ desugar e3
    
    desugar (ReplicatedAlphaParallel stmts e1 e2) = do
        e1 <- desugar e1
        e2 <- desugar e2
        stmts <- desugar stmts
        return $! ReplicatedAlphaParallel (restrictStatements [e1, e2] stmts) e1 e2
    desugar (ReplicatedInterleave stmts e) = do
        e <- desugar e
        stmts <- desugar stmts
        return $! ReplicatedInterleave (restrictStatements e stmts) e
    desugar (ReplicatedExternalChoice stmts e) = do
        e <- desugar e
        stmts <- desugar stmts
        return $! ReplicatedExternalChoice (restrictStatements e stmts) e
    desugar (ReplicatedInternalChoice stmts e) = do
        e <- desugar e
        stmts <- desugar stmts
        return $! ReplicatedInternalChoice (restrictStatements e stmts) e
    desugar (ReplicatedParallel e1 stmts e2) = do
        e1 <- desugar e1
        stmts <- desugar stmts
        e2 <- desugar e2
        return $! ReplicatedParallel e1 (restrictStatements [e1, e2] stmts) e2
    desugar (ReplicatedLinkParallel ties tiesStmts stmts e) =
        maybeTimedSection
            (do
                ties <- desugar ties
                tiesStmts <- desugar tiesStmts
                stmts <- desugar stmts
                e <- desugar e
                let tiesEs = concatMap (\ (a,b) -> [a,b]) ties
                    tiesStmts' = restrictStatements tiesEs tiesStmts
                    allVars = freeVars tiesEs ++ freeVars tiesStmts ++ freeVars e
                    stmts' = restrictStatementsToVars (S.fromList allVars) stmts
                return $! ReplicatedLinkParallel ties tiesStmts' stmts' e)
            (\ _ -> do
                loc <- gets currentLoc
                throwSourceError [linkedParallelTimedSectionError loc])
    desugar (ReplicatedSequentialComp stmts e) = do
        e <- desugar e
        stmts <- desugar stmts
        return $! ReplicatedSequentialComp (restrictStatements e stmts) e
    desugar (ReplicatedSynchronisingExternalChoice e1 stmts e2) = do
        e1 <- desugar e1
        stmts <- desugar stmts
        e2 <- desugar e2
        return $! ReplicatedSynchronisingExternalChoice e1
            (restrictStatements [e1, e2] stmts) e2
    
instance Desugarable (Field Name) where
    desugar (Output e) = return Output $$ desugar e
    desugar (Input p e) = return Input $$ desugar p $$ desugar e
    desugar (NonDetInput p e) =
        maybeTimedSection (return NonDetInput $$ desugar p $$ desugar e)
            (\ _ -> do
                loc <- gets currentLoc
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
        PCompList (map (\ c -> An Unknown (TSeq TChar, dummyAnnotation) (PLit (Char c))) (B.unpack s))
            Nothing (PLit (String s))
    desugar (PLit l) = return PLit $$ desugar l
    -- We don't remove the Paren as people may pretty print a desugared
    -- expression, which would then not have parenthesis needed to
    -- remove ambiguity
    desugar (PParen p) = desugar p >>= return . unAnnotate
    desugar (PSet []) = return $ PSet []
    desugar (PSet [p]) = return PSet $$ (desugar p >>= (\x -> return [x]))
    desugar (PSet ps) = do
        loc <- gets currentLoc
        throwSourceError [invalidSetPatternMessage (PSet ps) loc]
    desugar (PTuple ps) = return PTuple $$ desugar ps
    desugar (PVar n) = substituteName n >>= return . PVar
    desugar (PWildCard) = return PWildCard

instance Desugarable Literal where
    desugar l = return l

instance Desugarable TypeScheme where
    desugar (ForAll cs t) = do
        t <- desugar t
        let cs' = map (\ (a, b) -> (typeVar a, b)) (collectConstraints t)
        return $! ForAll cs' t

instance Desugarable Type where
    desugar (TVar tvref) = return $ TVar tvref
    desugar (TSet t) = return TSet $$ desugar t
    desugar (TSeq t) = return TSeq $$ desugar t
    desugar (TDot t1 t2) = return TDot $$ desugar t1 $$ desugar t2
    desugar (TMap t1 t2) = return TMap $$ desugar t1 $$ desugar t2
    desugar (TTuple ts) = return TTuple $$ desugar ts
    desugar (TFunction ts t) = return TFunction $$ desugar ts $$ desugar t
    desugar (TDatatype n) = return TDatatype $$ substituteName n
    desugar (TDotable t1 t2) = return TDotable $$ desugar t1 $$ desugar t2
    desugar (TExtendable t tvref) =
        return TExtendable $$ desugar t $$ return tvref
    desugar TInt = return TInt
    desugar TBool = return TBool
    desugar TProc = return TProc
    desugar TEvent = return TEvent
    desugar TChar = return TChar
    desugar TExtendableEmptyDotList = return TExtendableEmptyDotList

-- | Restricts any patterns in the statements to the free variables of the
-- given expression. This function will also look for free vars inside the
-- statments themselves.
restrictStatements :: FreeVars a => a -> [TCStmt] -> [TCStmt]
restrictStatements x = restrictStatementsToVars (S.fromList $ freeVars x)

-- | Restricts any patterns in the statements to the given set of variables.
-- This function will also look for free vars inside the statments themselves.
restrictStatementsToVars :: S.Set Name -> [TCStmt] -> [TCStmt]
restrictStatementsToVars ns stmts =
    let
        vars = S.union ns (S.fromList $ freeVars stmts)
        restrictStmt (An a b (Qualifier e)) = An a b (Qualifier e)
        restrictStmt (An a b (Generator p e)) =
            An a b (Generator (restrictPatternsToVars vars p) e)
    in map restrictStmt stmts

-- | Takes a pattern and replaces any variables that are not used by wildcard
-- patterns.
restrictPatternsToVars :: S.Set Name -> TCPat -> TCPat
restrictPatternsToVars ns =
    let
        restrict (An a b x) = An a b (restrict' x)
        restrict' (PConcat p1 p2) =
            PConcat (restrict p1) (restrict p2)
        restrict' (PCompList p1 Nothing p) =
            PCompList (map restrict p1) Nothing p
        restrict' (PCompList p1 (Just (p2, ps)) p) =
            PCompList (map restrict p1) (Just (restrict p2, map restrict ps)) p
        restrict' (PCompDot ps p) = PCompDot (map restrict ps) p
        restrict' (PDoublePattern p1 p2) =
            PDoublePattern (restrict p1) (restrict p2)
        restrict' (PLit l) = PLit l
        restrict' (PParen p) = PParen (restrict p)
        restrict'(PSet ps) = PSet (map restrict ps)
        restrict' (PTuple ps) = PTuple (map restrict ps)
        restrict' (PVar n) | nameIsConstructor n = PVar n
        restrict' (PVar n) | S.member n ns = PVar n
        restrict' (PVar _) = PWildCard
        restrict' PWildCard = PWildCard
    in restrict

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

doublePatternInPrefixError :: SrcSpan -> TCPat -> ErrorMessage
doublePatternInPrefixError loc pat = mkErrorMessage loc $
    text "The pattern" <+> prettyPrint pat
    <+> text "is not allowed in a prefix expression as it contains @@."

mkApplication :: Name -> Type -> [AnExp Name] -> DesugarMonad (Exp Name)
mkApplication fn fnTyp args = do
    srcSpan <- gets currentLoc
    ptype <- freshPType
    setPType ptype fnTyp
    var <- mkVar fn fnTyp
    return $ App var args

mkLit :: Literal -> DesugarMonad TCExp
mkLit l = do
    srcSpan <- gets currentLoc
    let typ = error "typ"
    ptype <- freshPType
    setPType ptype typ
    return $ An srcSpan (typ, ptype) (Lit l)

mkVar :: Name -> Type -> DesugarMonad TCExp
mkVar n typ = do
    srcSpan <- gets currentLoc
    ptype <- freshPType
    setPType ptype typ
    return $ An srcSpan (typ, ptype) (Var n)
