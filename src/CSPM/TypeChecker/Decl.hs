{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module CSPM.TypeChecker.Decl (typeCheckDecls) where

import Prelude hiding ((<>))

import Control.Monad
import Data.List (sort)
import Data.Graph.Wrapper
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ((\\), sortBy)
import Data.Maybe (fromJust)

import CSPM.Syntax.FreeVars
import CSPM.Syntax.Names
import CSPM.Syntax.AST hiding (getType)
import CSPM.Syntax.Types
import CSPM.PrettyPrinter
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Exceptions
import CSPM.TypeChecker.Expr()
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Pat()
import CSPM.TypeChecker.Unification
import Util.Annotated
import Util.List
import Util.PrettyPrint

takeFirstJust :: [Maybe a] -> Maybe a
takeFirstJust [] = Nothing
takeFirstJust (Just x : _) = Just x
takeFirstJust (Nothing : xs) = takeFirstJust xs

pathBetweenVerticies :: Ord i => Graph i v -> i -> i -> Maybe [i]
pathBetweenVerticies g start end = visit start (S.singleton start)
    where
        visit n visited | n == end = Just [end]
        visit n visited =
            case successors g n of
                [] -> Nothing
                ns -> takeFirstJust [
                            case visit n' (S.insert n' visited) of
                                Nothing -> Nothing
                                Just p -> Just (n:p)
                            | n' <- successors g n, not (S.member n' visited)
                        ]

-- | Type check a list of possibly mutually recursive functions
typeCheckDecls :: Bool -> Bool -> [TCDecl] -> TypeCheckMonad ()
typeCheckDecls checkAmbiguity generaliseTypes decls = do

    -- Flatten the decls so that definitions in modules are also type-checked
    -- in the correct order.
    let flattenDecl :: TCDecl -> [TCDecl]
        flattenDecl (An a b (Module mn [] ds1 ds2)) =
            (An a b (Module mn [] ds1 ds2))
            : concatMap flattenDecl (ds1 ++ ds2)
        flattenDecl (An a b (Module mn args ds1 ds2)) =
            [An a b (Module mn args ds1 ds2)]
        flattenDecl (d@(An _ _ (TimedSection _ _ ds))) =
            -- We need to type-check the function in the timed section, but we
            -- flatten the decls so that dependencies work out ok.
            d : concatMap flattenDecl ds
        flattenDecl x = [x]
        flatDecls = concatMap flattenDecl decls

        isInstance (An _ _ (ModuleInstance _ _ _ _ _)) = True
        isInstance _ = False

        instanceDecls = filter isInstance flatDecls

        -- | Map from declarations to integer identifiers
        declMap = zip flatDecls [0..]
        invDeclMap = M.fromList [(did, ds) | (ds, did) <- declMap]
        declarationsInGroup did =
            case M.lookup did invDeclMap of
                Just ds -> ds
                Nothing -> panic $ "Could not find declarations in "++show did
    
    let
        namesBoundByDecls = concatMap (\ (decl, declId) ->
            case decl of
                An _ _ (TimedSection _ _ _) -> []
                _ -> [(declId, boundNames decl)]) declMap

        -- | Map from names to the identifier of the declaration that it is
        -- defined by.
        varToDeclIdMap = M.fromList $
            [(n, declId) | (declId, ns) <- namesBoundByDecls, n <- ns]
        variableDeclaration n =
            case M.lookup n varToDeclIdMap of
                Just d -> d
                Nothing -> panic $ "Could not find declaration of "++show n
        boundVars = S.fromList $! M.keys varToDeclIdMap

    -- Map from decl id -> [decl id] meaning decl id depends on the list of
    -- ids
    let declDeps = map (\ (decl, declId) ->
            let deps = freeVars decl
                depsInThisGroup = filter (flip S.member boundVars) deps
                declIdDeps = sortedNub $ sort $
                                map variableDeclaration depsInThisGroup
            in (declId, declIdDeps)) declMap

        -- | Edge from n -> n' iff n uses n'
        declGraph :: Graph Int Int
        declGraph = fromListSimple [(id, deps) | (id, deps) <- declDeps]
        -- | The graph of strongly connected components, with an edge
        -- from scc i to scc j if j depends on i, but i does not depend
        -- on j.
        sccgraph :: Graph (S.Set Int) (M.Map Int Int)
        sccgraph = transpose (sccGraph declGraph)
        -- | The strongly connected components themselves, topologically sorted
        sccs :: [S.Set Int]
        sccs = topologicalSort sccgraph
        
        -- | Get the declarations corresponding to certain ids
        typeInferenceGroup dids = map declarationsInGroup dids

        -- | Checks that this SCC does not contain both a module and an
        -- instance of the module.
        checkSCCForModuleCycles :: [TCDecl] -> TypeCheckMonad ()
        checkSCCForModuleCycles decls =
            let 
                instances = [i | An _ _ (i@(ModuleInstance _ _ _ _ _)) <- decls]
                mods = [m | An _ _ (m@(Module _ _ _ _)) <- decls]

                instancesOfMod n =
                    [i | i@(ModuleInstance _ nt _ _ _) <- instances, nt == n]

                checkMod (Module nm _ ds1 ds2) = 
                    mapM_ (\ (ModuleInstance ni _ _ instanceMap _) -> mapM_ (\ n -> 
                        -- Check to see if there is a path from the module to
                        -- this var of this instance of the module.
                        when (hasPath declGraph (variableDeclaration nm)
                                (variableDeclaration n)) $ do
                            let Just path = pathBetweenVerticies declGraph
                                                (variableDeclaration nm)
                                                (variableDeclaration n)
                                p = map declarationsInGroup path
                                firstName = head $ boundNames $ head $ p
                            setSrcSpan (nameDefinition firstName) $! raiseMessageAsError $
                                illegalModuleInstanceCycleErrorMessage decls nm ni p
                        ) (M.elems instanceMap)) (instancesOfMod nm)
            in mapM_ checkMod mods

        -- When an error occurs continue type checking, but only
        -- type check groups that do not depend on the failed group.
        -- failM is called at the end if any error has occured.
        typeCheckGroups [] b = if b then failM else return ()
        typeCheckGroups (g:gs) b = do
            err <- tryAndRecover True (do
                let ds = typeInferenceGroup $ S.toList g
                checkSCCForModuleCycles ds
                typeCheckMutualyRecursiveGroup checkAmbiguity generaliseTypes ds
                return False
                ) (return True)
            if not err then typeCheckGroups gs b
            -- Else, continue type checking but remove all declaration groups
            -- that are reachable from this group. Also, set the flag to be 
            -- True to indicate that an error has occured so that failM is 
            -- called at the end.
            else typeCheckGroups (gs \\ (reachableVertices sccgraph g)) True

    -- Start type checking the groups
    typeCheckGroups sccs False

    let
        annotate (decl@(An _ psymbtable (Module _ _ ds1 ds2))) = do
            let ns = boundNames decl
            ts <- mapM getType ns
            setPSymbolTable (snd psymbtable) (zip ns ts)
            mapM_ annotate (ds1++ds2)
        annotate (decl@(An _ psymbtable (TimedSection _ _ ds))) = do
            let ns = boundNames decl
            ts <- mapM getType ns
            setPSymbolTable (snd psymbtable) (zip ns ts)
            mapM_ annotate ds
        annotate (decl@(An _ psymbtable _)) = do
            let ns = boundNames decl
            ts <- mapM getType ns
            setPSymbolTable (snd psymbtable) (zip ns ts)

    -- Add the type of each declaration (if one exists to each declaration)
    mapM_ annotate decls

-- | Type checks a group of certainly mutually recursive functions. Only 
-- functions that are mutually recursive should be included otherwise the
-- types could end up being less general.
typeCheckMutualyRecursiveGroup :: Bool -> Bool -> [TCDecl] -> TypeCheckMonad ()
typeCheckMutualyRecursiveGroup checkAmbiguity generaliseTypes ds' = do
    -- TODO: fix temporary hack
    let 
        cmp x y = case (unAnnotate x, unAnnotate y) of
            (DataType _ _, DataType _ _) -> EQ
            (DataType _ _, _) -> LT
            (_, DataType _ _) -> GT
            (_, _) -> EQ
        ds = sortBy cmp ds'
        isTimedSection (An _ _ (TimedSection _ _ _)) = True
        isTimedSection _ = False
        -- All the free variables that we are going to type-check in this
        -- group. This excludes time section free variables becuase these are
        -- type-checked independently (see flattenDecls above).
        fvs = boundNames $ filter (not . isTimedSection) ds

        hasTypeAnnotation (An _ _ (FunBind _ _ (Just _))) = True
        hasTypeAnnotation (An _ _ (PatBind _ _ (Just _))) = True
        hasTypeAnnotation _ = False

        toGeneralise = boundNames $ filter hasTypeAnnotation ds

        extractDataTypeClauseNames (An _ _ (DataType _ cs)) =
            [n | An _ _ (DataTypeClause n _ _) <- cs]
        extractDataTypeClauseNames _ = []
        dataTypeClauseNames = S.fromList $ concatMap extractDataTypeClauseNames ds

        dataTypeWithClause n = head $ filter (\ x ->
            n `elem` extractDataTypeClauseNames x) ds

        extractChannelNames (An _ _ (Channel ns _ _)) = ns
        extractChannelNames _ = []
        channelNames = S.fromList $ concatMap extractChannelNames ds
        channelDeclWithName n = head $ filter (\ x ->
            n `elem` extractChannelNames x) ds
            
        typeCheckAndGeneralise =
            if generaliseTypes then generaliseGroup (map typeCheck ds)
            else generaliseSubGroup toGeneralise (map typeCheck ds)
            
        hasDataType =
            case ds of
                An _ _ (DataType _ _) : _ -> True
                _ -> False
        
        dataTypeNames = [n | An _ _ (DataType n _) <- ds]

    freshTypeVariableContext $ do
        ftvs <- replicateM (length fvs) freshRegisteredTypeVar
        zipWithM setType fvs (map (ForAll []) ftvs)

        -- Type check each declaration then generalise the types. We firstly guess that all datatypes are comparable for
        -- equality and if this fails, we unmark them as comparable for equality and retry (recalling that they are
        -- mutually recursive).
        (if hasDataType then do
            -- In this case we need to mark all datatypes as comparable for equality, and backtrack if necessary
            mapM_ markDatatypeAsComparableForEquality dataTypeNames
            b <- tryAndRecover False 
                    (typeCheckAndGeneralise >> return True)
                    (return False)
            when (not b) $ do
                mapM_ unmarkDatatypeAsComparableForEquality dataTypeNames
                typeCheckAndGeneralise
                return ()
        else do
            typeCheckAndGeneralise
            return ())

        -- Compress all the types we have inferred here (they should never be 
        -- touched again)
        mapM_ (\ n -> do
            t <- getType n
            t' <- compressTypeScheme t
            when (checkAmbiguity && S.member n channelNames) $
                -- Check that t' is not polymorphic
                case t' of
                    ForAll [] _ -> return ()
                    _ -> do
                        let d@(An _ _ (Channel _ cs _)) = channelDeclWithName n
                            Just errCtxt = errorContext (unAnnotate d)
                        addErrorContext errCtxt $ setSrcSpan (loc d) $
                            raiseMessageAsError $ ambiguousChannelError n t'
            when (checkAmbiguity && S.member n dataTypeClauseNames) $
                -- Check that t' is not polymorphic
                case t' of
                    ForAll [] _ -> return ()
                    _ -> do
                        let d@(An _ _ (DataType _ cs)) = dataTypeWithClause n
                            c = head [c | c@(An _ _ (DataTypeClause n' _ _)) <- cs, n == n']
                            Just errCtxt1 = errorContext (unAnnotate d)
                            Just errCtxt2 = errorContext (unAnnotate c)
                        addErrorContext errCtxt1 $ addErrorContext errCtxt2 $ setSrcSpan (loc c) $
                            raiseMessageAsError $ ambiguousDataTypeClauseError n t'
            setType n t') fvs

-- | Takes a type and returns the inner type, i.e. the type that this
-- is a set of. For example TSet t1 -> t, TTuple [TSet t1, TSet t2] -> (t1, t2).
-- The type that is returned is guaranteed to satisfy Eq since, at the
-- recursion only bottoms out on reaching something that is of type TSet.
evalTypeExpression :: Type -> TypeCheckMonad Type
evalTypeExpression (TTuple ts) = do
    -- TTuple [TSet t1,...] = TSet (TTuple [t1,...])
    ts' <- mapM evalTypeExpression ts
    return $ TTuple ts'
evalTypeExpression (TDot t1 t2) = do
    -- TDot (TSet t1) (TSet t2) = TSet (TDot t1 t2)
    t1' <- evalTypeExpression t1
    t2' <- evalTypeExpression t2
    return $ TDot t1' t2'
-- Otherwise, it must be a set.
evalTypeExpression t = do
    fv <- freshRegisteredTypeVar
    unify t (TSet fv)
    return fv

instance TypeCheckable TCDecl [(Name, Type)] where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)

instance TypeCheckable (Decl Name) [(Name, Type)] where
    errorContext (FunBind n ms _) = Just $ 
        -- This will only be helpful if the equations don't match in
        -- type
        (text "In the declaration of:" <+> prettyPrint n, [])
    errorContext (p@(PatBind pat exp _)) = Just $
        (hang (text "In a pattern binding:") tabWidth (prettyPrint p), [])
    errorContext (DataType n cs) = Just $
        (text "In the declaration of:" <+> prettyPrint n, [])
    errorContext (SubType n cs) = Just $
        (text "In the declaration of:" <+> prettyPrint n, [])
    errorContext (NameType n e _) = Just $
        (text "In the declaration of:" <+> prettyPrint n, [])
    errorContext (Channel ns es _) = Just $
        (text "In the declaration of:" <+> list (map prettyPrint ns), [])
    errorContext (Assert a) = Just $
        (text "In the assertion:" <+> prettyPrint a, [])
    errorContext (TimedSection _ _ _) = Nothing
    errorContext (Transparent ns) = Nothing
    errorContext (External ns) = Nothing
    errorContext (ModuleInstance n _ _ _ _) = Just $
        (text "In the declaration of the module instance:" <+> prettyPrint n, [])
    errorContext (Module _ _ _ _) = Nothing
    errorContext (PrintStatement _) = Nothing
    
    typeCheck' (FunBind n ms mta) = do
        let boundTypeVars =
                case mta of
                    Just (An _ _ (STypeScheme boundNs _ _)) -> boundNs
                    _ -> []
        ts <- local boundTypeVars $ do
            mta <- case mta of
                    Just ta -> do
                        ForAll _ t <- typeCheck ta
                        return $ Just t
                    Nothing -> return Nothing
            mapM (\ m -> addErrorContext (matchCtxt m) $ 
                case mta of
                    Nothing -> typeCheck m
                    Just ta -> typeCheckExpect m ta) ms
        ForAll [] t <- getType n
        -- This unification also ensures that each equation has the same number
        -- of arguments.
        (t' @ (TFunction tsargs _)) <- unifyAll (t:ts)
        return [(n, t')]
        where
            matchCtxt an = 
                (hang (text "In an equation for" <+> prettyPrint n <> colon) 
                    tabWidth (prettyPrintMatch n an),
                [])
    typeCheck' (p@(PatBind pat exp mta)) = do
        let boundTypeVars =
                case mta of
                    Just (An _ _ (STypeScheme boundNs _ _)) -> boundNs
                    _ -> []
        (texp, tpat) <- local boundTypeVars $ do
            case mta of
                Nothing -> do
                    tpat <- typeCheck pat
                    texp <- typeCheck exp
                    return (tpat, texp)
                Just ta -> do
                    -- todo: check
                    ForAll _ typ <- typeCheck ta
                    tpat <- typeCheckExpect pat typ
                    texp <- typeCheckExpect exp typ
                    return (tpat, texp)
        -- We evaluate the dots to implement the 'longest match' rule. For
        -- example, suppose we have the following declaration:
        --   datatype A = B.Integers.Integers
        --   f(B.x) = x
        -- Then we make the decision that x should be of type Int.Int.
        tpat <- evaluateDots tpat
        texp <- evaluateDots texp
        -- We must disallow symmetric unification here as we don't want
        -- to allow patterns such as:
        --     x.y = B
        disallowSymmetricUnification (unify texp tpat)
        let ns = boundNames p
        ts <- mapM getType ns
        return $ zip ns [t | ForAll _ t <- ts]
    -- The following two clauses rely on the fact that they have been 
    -- prebound.
    typeCheck' (Channel ns Nothing ta) = do
        case ta of
            Nothing -> return ()
            Just ta -> typeCheckExpect ta TEvent >> return ()
        -- We now unify the each type to be a TEvent
        mapM (\ n -> do
            ForAll [] t <- getType n
            unify TEvent t) ns
        -- (Now getType n for any n in ns will return TEvent)
        return [(n, TEvent) | n <- ns]
    typeCheck' (Channel ns (Just e) mta) = do
        let boundTypeVars =
                case mta of
                    Just (An _ _ (STypeScheme boundNs _ _)) -> boundNs
                    _ -> []
        dotList <- local boundTypeVars $ do
            t <- case mta of
                Nothing -> typeCheck e
                Just ta -> do
                    ForAll _ valueType <- typeCheck ta
                    (tfs, urt) <- dotableToDotList valueType
                    unify TEvent urt
                    fv <- freshRegisteredTypeVar
                    unify (TDotable fv TEvent) valueType
                    typeCheckExpect e (foldr1 TDot (map TSet tfs))
            t <- evaluateDots t
            valueType <- evalTypeExpression t
            -- Events must be comparable for equality.
            valueType <- ensureHasConstraints [CEq, CComplete] valueType
            typeToDotList valueType
        let t = foldr TDotable TEvent dotList
        mapM (\ n -> do
            ForAll [] t' <- getType n
            unify t' t) ns
        return $ [(n, t) | n <- ns]
    typeCheck' (SubType n clauses) = do
        -- Get the type fromthe first clause
        parentType <- freshRegisteredTypeVar
        mapM_ (\ clause -> do
                let nclause = case unAnnotate clause of
                            DataTypeClause x _ _ -> x
                (_, tsFields) <- typeCheck clause
                ts <- getType nclause
                ForAll [] typeCon <- getType nclause
                (actFields, dataType) <- dotableToDotList typeCon
                when (length tsFields > length actFields) $
                    raiseMessageAsError $ incorrectSubtypeFieldCountMessage
                        (prettyPrint nclause) (length actFields) (length tsFields)
                -- Check that the datatype is the correct subtype.
                tvref' <- freshRegisteredTypeVarRef []
                unify (TExtendable parentType tvref') dataType
                -- Check that the fields are compatible with the expected fields.
                zipWithM unify actFields tsFields
            ) clauses
        ForAll [] t <- getType n
        t' <- unify t (TSet parentType)
        return [(n, TSet parentType)]
    typeCheck' (DataType n clauses) = do
        ForAll [] t <- getType n
        unify t (TSet (TDatatype n))
        ntss <- mapM (\ clause -> do
            let 
                n' = case unAnnotate clause of
                        DataTypeClause x _ _ -> x
            ForAll [] t <- getType n'
            (n', ts) <-
                case dataTypeClauseDeclaredType (unAnnotate clause) of
                    Nothing -> typeCheck clause
                    Just ta -> do
                        let boundTypeVars =
                                case ta of
                                    An _ _ (STypeScheme boundNs _ _) -> boundNs
                        local boundTypeVars $ do
                            ForAll _ tsig <- ensureIsExtendable ta (TDatatype n)
                            (n', ts) <- typeCheck clause
                            setSrcSpan (loc clause) $
                                addErrorContext (fromJust (errorContext (unAnnotate clause))) $
                                    unify tsig (foldr TDotable (TDatatype n) ts)
                            return (n', ts)
            let texp = foldr TDotable (TDatatype n) ts
            t <- unify texp t
            return ((n', t), ts)
            ) clauses
        let (nts, tcss) = unzip ntss
            tclauses = concat tcss
        tclauses <- mapM (\t -> compress t) tclauses
        -- We now need to decide if we should allow this type to be comparable
        -- for equality. Thus, we check to see if each of the fields in each of
        -- the constructors is comparable for equality.
        b <- datatypeIsComparableForEquality n
        when b $ mapM_ (ensureHasConstraint CEq) tclauses
        ForAll [] t <- getType n
        t' <- unify t (TSet (TDatatype n))
        return $ (n, t'):nts
    typeCheck' (NameType n e mta) = do
        let boundTypeVars =
                case mta of
                    Just (An _ _ (STypeScheme boundNs _ _)) -> boundNs
                    _ -> []
        valueType <- local boundTypeVars $ do
            case mta of
                Nothing -> do
                    t <- typeCheck e
                    evalTypeExpression t
                Just ta -> do
                    ForAll _ typ <- typeCheck ta
                    t <- typeCheckExpect e typ
                    evalTypeExpression t
        valueType <- ensureHasConstraint CComplete valueType
        return [(n, TSet valueType)]
    typeCheck' (Transparent ns) = return []
    typeCheck' (External ns) = return []
    typeCheck' (Assert a) = typeCheck a >> return []
    typeCheck' (TimedSection (Just tn) f _) = do
        typeCheckExpect (Var tn) TEvent
        case f of
            Just f -> typeCheckExpect f (TFunction [TEvent] TInt) >> return ()
            Nothing -> return ()
        return []
    typeCheck' (Module n [] _ _) = return [(n, TTuple [])]
    typeCheck' (Module n args pubDs privDs) = do
        let fvs = boundNames args
        local fvs $ do
            tpats <- mapM (\ pat -> typeCheck pat >>= evaluateDots) args
            typeCheckDecls False True (pubDs ++ privDs)
            tpats <- mapM (\ pat -> typeCheck pat >>= evaluateDots) args
            return [(n, TTuple tpats)]

    typeCheck' (ModuleInstance n nt args nm (Just mod)) = do
        ts <- getType nt
        addModuleInstanceMap nm
        (TTuple ts, sub) <- instantiate' ts
        when (length ts /= length args) $
            raiseMessageAsError $ incorrectModuleArgumentCountMessage
                (prettyPrint nt) (length ts) (length args)
        zipWithM typeCheckExpect args ts
        let subName n = case M.lookup n nm of
                            Just n' -> n'
                            Nothing -> n
        -- Set the types of each of our arguments
        nts <- mapM (\ (theirName, ourName) -> do
                ForAll _ t <- getType theirName >>= substituteTypeScheme sub
                -- We also need to change any datatype according to name map
                let sub (TVar tvref) = do
                        res <- readTypeRef tvref
                        case res of 
                            Left _ -> return $ TVar tvref
                            Right t -> sub t
                    sub (TSet t) = sub t >>= return . TSet
                    sub (TSeq t) = sub t >>= return . TSeq
                    sub (TDot t1 t2) = do
                        t1 <- sub t1
                        t2 <- sub t2
                        return $! TDot t1 t2
                    sub (TTuple ts) = mapM sub ts >>= return . TTuple
                    sub (TMap t1 t2) = do
                        t1 <- sub t1
                        t2 <- sub t2
                        return $! TMap t1 t2
                    sub (TFunction ts t) = do
                        ts <- mapM sub ts
                        t <- sub t
                        return  $! TFunction ts t
                    sub (TDatatype n) = return $ TDatatype $! subName n
                    sub (TDotable t1 t2) = do
                        t1 <- sub t1
                        t2 <- sub t2
                        return $! TDotable t1 t2
                    sub (TExtendable t tvref) = do
                        t' <- sub t
                        res <- readTypeRef tvref
                        case res of
                            Left _ -> return $ TExtendable t tvref
                            Right t -> do
                                tsub' <- sub t
                                writeTypeRef tvref tsub'
                                return $ TExtendable t' tvref
                    sub TInt = return TInt
                    sub TBool = return TBool
                    sub TProc = return TProc
                    sub TEvent = return TEvent
                    sub TChar = return TChar
                    sub TExtendableEmptyDotList = return TExtendableEmptyDotList
                t <- sub t
                xs <- freeTypeVars t
                setType ourName $ ForAll xs t
                return (ourName, t)
            ) (M.toList nm)

        let An _ _ (Module _ _ privDs pubDs) = mod

            isParamModule (An _ _ (Module _ (_:_) _ _)) = True
            isParamModule _ = False

            boundSubNames = boundNames (filter (not . isParamModule) (privDs ++ pubDs))

        -- Check channels/datatypes for ambigutity and mark datatypes as
        -- comparable if appropriate.
        mapM_ (\ ourName -> do
            canonicalName <- canonicalNameOfInstanceName ourName
            if canonicalName `elem` boundSubNames then do
                ForAll xs t <- getType ourName
                case (xs, t) of
                    (_:_, TDotable _ _) -> do
                        (fs, urt) <- dotableToDotList t
                        case urt of
                            TEvent -> raiseMessageAsError $
                                ambiguousChannelError ourName (ForAll xs t)
                            TDatatype n -> raiseMessageAsError $
                                ambiguousDataTypeClauseError ourName (ForAll xs t)
                            _ -> return ()
                    _ -> return ()
            else return ()
            ) (M.elems nm)

        -- Mark datatypes as comparable as appropriate
        mapM_ (\ d -> case unAnnotate d of
            DataType n clauses -> do
                let n' = subName n
                tclauses <- mapM (\ (An _ _ (DataTypeClause n _ _)) -> do
                    ForAll _ t <- getType (subName n)
                    return t) clauses
                markDatatypeAsComparableForEquality n'
                b <- tryAndRecover False 
                        (mapM_ (ensureHasConstraint CEq) tclauses >> return True)
                        (return False)
                when (not b) $ unmarkDatatypeAsComparableForEquality n'

            _ -> return ()
            ) (privDs ++ pubDs)

        ForAll _ ts <- getType nt
        return $! (n, ts) : nts
    typeCheck' (PrintStatement _) = return []

instance TypeCheckable TCAssertion () where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)

instance TypeCheckable (Assertion Name) () where
    errorContext a = Just $ 
        (hang (text "In the assertion" <> colon) tabWidth (prettyPrint a), [])
    typeCheck' (PropertyCheck e1 p m opts) = do
        ensureIsProc e1
        typeCheck p
        mapM_ typeCheck opts
    typeCheck' (Refinement e1 m e2 opts) = do
        ensureIsProc e1
        ensureIsProc e2
        mapM_ typeCheck opts
    typeCheck' (ASNot a) = typeCheck a

instance TypeCheckable TCModelOption () where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)

instance TypeCheckable (ModelOption Name) () where
    errorContext a = Nothing
    typeCheck' (TauPriority e) = do
        typeCheckExpect e (TSet TEvent)
        return ()
    typeCheck' (PartialOrderReduce _) = return ()

instance TypeCheckable (SemanticProperty Name) () where
    errorContext a = Nothing
    typeCheck' (HasTrace es) = mapM_ (\ e -> typeCheckExpect e TEvent) es
    typeCheck' _ = return ()

instance TypeCheckable TCDataTypeClause (Name, [Type]) where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)

instance TypeCheckable (DataTypeClause Name) (Name, [Type]) where
    errorContext c = Just $
        (hang (text "In the data type clause" <> colon) tabWidth 
            (prettyPrint c), [])
    typeCheck' (DataTypeClause n' Nothing _) = do
        return (n', [])
    typeCheck' (DataTypeClause n' (Just e) _) = do
        t <- typeCheck e
        t <- evaluateDots t
        valueType <- evalTypeExpression t
        valueType <- ensureHasConstraint CComplete valueType
        dotList <- typeToDotList valueType
        return (n', dotList)

instance TypeCheckable TCMatch Type where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)
    typeCheckExpect an t = setSrcSpan (loc an) $ typeCheckExpect (inner an) t
instance TypeCheckable (Match Name) Type where
    -- We create the error context in FunBind as that has access
    -- to the name
    errorContext (Match groups exp) = Nothing
    typeCheck' (Match groups exp) = do
        -- Introduce free variables for all the parameters
        let fvs = boundNames groups
        local fvs $ do
            tgroups <- mapM (\ pats -> mapM (\ pat -> 
                    -- We evaluate the dots here to implment the longest 
                    -- match rule
                    typeCheck pat >>= evaluateDots
                ) pats) groups
    
            -- We evaluate the dots here to implment the longest match rule
            tr <- typeCheck exp >>= evaluateDots
            
            -- We need to evaluate the dots in the patterns twice just in case 
            -- the type inferences on the RHS have resulted in extra dots on 
            -- the left being able to be removed.
            tgroups <- mapM (\ pats -> mapM (\ pat -> 
                    typeCheck pat >>= evaluateDots
                ) pats) groups

            return $ foldr (\ targs tr -> TFunction targs tr) tr tgroups
    typeCheckExpect (Match groups exp) tsig = do
        -- Introduce free variables for all the parameters
        let fvs = boundNames groups
        local fvs $ do
            -- Check that the function signature is of a plausible shape
            rt <- freshRegisteredTypeVar
            argts <- mapM (flip replicateM freshRegisteredTypeVar) (map length groups)
            unify tsig $ foldr (\ targs tr -> TFunction targs tr) rt argts

            -- The rest of the code is as before (comments before also apply)
            tgroups <- zipWithM (\ pats argts -> zipWithM (\ pat argt -> do
                    argt <- compress argt >>= evaluateDots
                    tact <- typeCheck pat >>= evaluateDots
                    -- We must disallow symmetric unification here as we don't
                    -- want to allow type signatures that require a type B, but
                    -- actually have a pattern of type x.y
                    disallowSymmetricUnification (unify tact argt) >>= evaluateDots
                ) pats argts) groups argts
            tr <- typeCheckExpect exp rt >>= evaluateDots
            tgroups <- mapM (\ pats -> mapM (\ pat -> 
                    typeCheck pat >>= evaluateDots
                ) pats) groups
            return $ foldr (\ targs tr -> TFunction targs tr) tr tgroups

instance TypeCheckable TCSTypeScheme TypeScheme where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)
    typeCheckExpect an t = setSrcSpan (loc an) $ typeCheckExpect (inner an) t
instance TypeCheckable (STypeScheme Name) TypeScheme where
    errorContext _ = Nothing
    typeCheck' (STypeScheme boundNs cs t) = do
        tvs <- mapM (\ n -> do
            let ncs = map (\ (STypeConstraint c _) -> c) $
                        filter (\ (STypeConstraint _ n') -> n == n') $
                        (map unAnnotate cs)
            t@(TVar tvref) <- freshRegisteredRigidTypeVarWithConstraints n ncs
            setType n (ForAll [] t)
            return (typeVar tvref, constraints tvref)
            ) boundNs
        t' <- typeCheck t
        return $ ForAll tvs t'

    typeCheckExpect tv t = do
        ts@(ForAll _ t') <- typeCheck' tv
        unify t' t
        return ts

instance TypeCheckable TCSType Type where
    errorContext _ = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)
    typeCheckExpect an t = setSrcSpan (loc an) $ typeCheckExpect (inner an) t
instance TypeCheckable (SType Name) Type where
    errorContext _ = Nothing
    typeCheck' (STVar var) = getType var >>= \ (ForAll [] t) -> return t
    typeCheck' (STExtendable t var) = do
        t <- typeCheck t
        TVar tvref <- getType var >>= \ (ForAll [] t) -> return t
        return $ TExtendable t tvref
    typeCheck' (STSet t) = typeCheck t >>= return . TSet
    typeCheck' (STSeq t) = typeCheck t >>= return . TSeq
    typeCheck' (STDot t1 t2) = do
        t1' <- typeCheck t1
        t2' <- typeCheck t2
        return $ TDot t1' t2'
    typeCheck' (STMap t1 t2) = do
        t1' <- typeCheck t1
        t2' <- typeCheck t2
        return $ TMap t1' t2'
    typeCheck' (STTuple ts) = mapM typeCheck ts >>= return . TTuple
    typeCheck' (STFunction args rt) = do
        targs <- mapM typeCheck args
        trt <- typeCheck rt
        return $ TFunction targs trt
    typeCheck' (STDotable t1 t2) = do
        t1' <- typeCheck t1
        t2' <- typeCheck t2
        return $ TDotable t1' t2'
    typeCheck' (STParen t) = typeCheck t

    typeCheck' (STDatatype n) = return $ TDatatype n
    typeCheck' STProc = return TProc
    typeCheck' STInt = return TInt
    typeCheck' STBool = return TBool
    typeCheck' STChar = return TChar
    typeCheck' STEvent = return TEvent

    typeCheckExpect tv t = do
        t' <- typeCheck' tv
        unify t' t
