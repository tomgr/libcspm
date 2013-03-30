{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Renames all variables to unique Names, in the process converting all
-- UnRenamedName into Name. This simplifies many subsequent phases as every
-- name is guaranteed to be unique so flat maps may be used, rather than
-- Hierarchical maps. Further, this also flags patterns that match channels
-- and datatype clauses.
module CSPM.Renamer (
    RenamerState,
    RenamerMonad,
    runFromStateToState,
    initRenamer,
    rename,
    newScope,
    getBoundNames,
)  where

import Control.Monad.State
import Data.List

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Prelude
import CSPM.PrettyPrinter
import Util.Annotated
import Util.Exception
import Util.FuzzyLookup
import qualified Util.HierarchicalMap as HM
import Util.Monad
import Util.PartialFunctions
import Util.PrettyPrint hiding (($$))
import qualified Util.PrettyPrint as P

type RenameEnvironment = HM.HierarchicalMap UnRenamedName Name
type TypeRenameEnvironment = HM.HierarchicalMap UnRenamedName (SType Name)

data RenamerState = RenamerState {
        -- | The current rename map for variables.
        environment :: RenameEnvironment,
        -- | The current rename map for type-variables.
        typeEnvironment :: TypeRenameEnvironment,
        -- | The last source span seen, for error messages.
        srcSpan :: SrcSpan,
        -- | Errors that have occured.
        errors :: [ErrorMessage],
        -- | The modules that names should be qualified with.
        currentModuleQualificationStack :: [OccName]
    }

type RenamerMonad = StateT RenamerState IO

-- | Initialises the renamer.
initRenamer :: IO (RenamerState)
initRenamer = do
    let bs = map (\b -> (UnQual (OccName (stringName b)), name b)) (builtins False)
        bts = map (\ (s, t) -> (UnQual (OccName s), t)) [
                    ("Proc", STProc),
                    ("Int", STInt),
                    ("Bool", STBool),
                    ("Char", STChar),
                    ("Event", STEvent)
                ]
    return $ RenamerState {
        -- We insert a new layer to allow builtins to be overridden
        environment = HM.newLayer (HM.updateMulti HM.new bs),
        typeEnvironment = HM.newLayer (HM.updateMulti HM.new bts),
        srcSpan = Unknown,
        errors = [],
        currentModuleQualificationStack = []
    }

-- | Runs the renamer starting at the given state and returning the given state.
runFromStateToState :: RenamerState -> RenamerMonad a -> IO (a, RenamerState)
runFromStateToState s p = do
    (a, st') <- runStateT p s
    -- Check if any errors have been thrown.
    case errors st' of
        [] -> return (a, st')
        es -> throwSourceError es

getBoundNames :: RenamerMonad [Name]
getBoundNames = do
    env <- gets environment
    let isInteresting n = nameType n `elem` [ExternalName, WiredInName]
    return $ filter isInteresting $ map snd (HM.flatten env)

-- **********************
-- Monad Operations

addTypeScope :: RenamerMonad a -> RenamerMonad a
addTypeScope m = do
    env <- gets typeEnvironment
    let env' = HM.newLayer env
    modify (\ st -> st { typeEnvironment = env' })
    a <- m
    modify (\ st -> st { typeEnvironment = env })
    return a

lookupType :: UnRenamedName -> RenamerMonad (Maybe (SType Name))
lookupType n = do
    env <- gets typeEnvironment
    return $! HM.maybeLookup env n

setType :: UnRenamedName -> SType Name -> RenamerMonad ()
setType rn t = modify (\ st -> st {
        typeEnvironment = HM.update (typeEnvironment st) rn t
    })

addScope :: RenamerMonad a -> RenamerMonad a
addScope m = do
    env <- gets environment
    let env' = HM.newLayer env
    modify (\ st -> st { environment = env' })
    a <- m
    modify (\ st -> st { environment = env })
    return a

newScope :: RenamerMonad ()
newScope = do
    env <- gets environment
    let env' = HM.newLayer env
    modify (\ st -> st { environment = env' })
    
lookupName :: UnRenamedName -> RenamerMonad (Maybe Name)
lookupName n = do
    env <- gets environment
    return $! HM.maybeLookup env n

qualifyName :: UnRenamedName -> RenamerMonad UnRenamedName
qualifyName (UnQual on) = do
    ns <- gets currentModuleQualificationStack
    let qualify [] = UnQual on
        qualify (n:ns) = Qual n (qualify ns)
    return $! qualify (reverse ns)
qualifyName (Qual on rn) = do
    n <- qualifyName (UnQual on)
    let qualify (UnQual on) = Qual on rn
        qualify (Qual mn rn) = Qual mn (qualify rn)
    return $! qualify n

setName :: UnRenamedName -> Name -> RenamerMonad ()
setName rn n = do
    rn' <- qualifyName rn
    modify (\ st -> st { environment = HM.update (environment st) rn' n })

setSrcSpan :: SrcSpan -> RenamerMonad ()
setSrcSpan loc = modify (\ st -> st { srcSpan = loc })

-- | Report a message as an error. This will be raised at the outer monad level.
addErrors :: [ErrorMessage] -> RenamerMonad ()
addErrors msgs = modify (\st -> st { errors = msgs ++ errors st })

addModuleContext :: OccName -> RenamerMonad a -> RenamerMonad a
addModuleContext mName prog = do
    modify (\ st -> st { currentModuleQualificationStack = 
        mName : currentModuleQualificationStack st })
    a <- prog
    modify (\st -> st { currentModuleQualificationStack =
        tail (currentModuleQualificationStack st) })
    return a

resetModuleContext :: RenamerMonad a -> RenamerMonad a
resetModuleContext prog = do
    stk <- gets currentModuleQualificationStack
    modify (\st -> st { currentModuleQualificationStack = [] })
    a <- prog
    modify (\st -> st { currentModuleQualificationStack = stk })
    return a

-- **********************
-- Renaming Class and basic instances

class Renamable e1 e2 | e1 -> e2 where
    rename :: e1 -> RenamerMonad e2

instance Renamable e1 e2 => Renamable [e1] [e2] where
    rename es = mapM rename es

instance Renamable e1 e2 => Renamable (Maybe e1) (Maybe e2) where
    rename (Just x) = return Just $$ rename x
    rename Nothing = return Nothing

instance Renamable e1 e2 => Renamable (Annotated a e1) (Annotated a e2) where
    rename (An a b e) = do
        setSrcSpan a
        rename e >>= return . An a b

instance (Renamable e1 e2, Renamable e1' e2') => Renamable (e1, e1') (e2, e2') where
    rename (a, b) = do
        a' <- rename a
        b' <- rename b
        return (a', b')

type NameMaker = SrcSpan -> UnRenamedName -> RenamerMonad Name

externalNameMaker :: Bool -> NameMaker
externalNameMaker b l unrn = do
    unrn <- qualifyName unrn
    mkExternalName unrn l b

internalNameMaker :: NameMaker
internalNameMaker l unrn = mkInternalName unrn l

reAnnotatePure :: Annotated a e -> e' -> Annotated a e'
reAnnotatePure (An a b _) v = An a b v

-- | Given something annotated and a function that mutates the inner object,
-- returns the inner object annotated with the same outer annotation.
reAnnotate :: Monad m => Annotated a e -> m e' -> m (Annotated a e')
reAnnotate (An a b _) m = do
    v <- m
    return $ An a b v

timedNames :: [String]
timedNames = ["timed_priority", "WAIT"]

-- | Renames the declarations in the current scope.
renameDeclarations :: Bool -> [PDecl] -> RenamerMonad a -> RenamerMonad ([TCDecl], a)
renameDeclarations topLevel ds prog = do
    -- Now, find all the datatype labels and channels and create names for
    -- them in the maps (otherwise renameVarLHS will fail).
    mapM_ insertChannelsAndDataTypes ds

    -- Firstly, check for duplicates amongst the definitions (this can only
    -- be done AFTER insertChannelsAndDatatTypes).
    checkDuplicates ds

    -- Secondly, insert all other names into the environment. We need to have
    -- inserted the channels by here to detect where names are pattern matched.
    mapM_ (insertBoundNames nameMaker) ds

    -- Finally, as all the declarations have been inserted into the map we
    -- can rename the right hand sides (as every variable *should* be in
    -- scope).
    ds' <- mapM renameRightHandSide ds

    -- check the inner thing
    a <- prog

    return (ds', a)

    where
        nameMaker :: NameMaker
        nameMaker = if topLevel then externalNameMaker False else internalNameMaker

        ignoringNameMaker :: NameMaker
        ignoringNameMaker _ rn = do
            Just v <- lookupName rn
            return v

        insertChannelsAndDataTypes :: PDecl -> RenamerMonad ()
        insertChannelsAndDataTypes ad = case unAnnotate ad of
            Channel ns _ -> mapM_ (\ rn -> do
                n' <- externalNameMaker True (loc ad) rn
                setName rn n') ns
            DataType rn cs -> mapM_ (\ cl -> case unAnnotate cl of
                        DataTypeClause rn _ -> do
                            n' <- externalNameMaker True (loc cl) rn
                            setName rn n'
                    ) cs
            Module (UnQual mn) [] privDs pubDs ->
                addModuleContext mn $ mapM_ insertChannelsAndDataTypes pubDs
                -- We can't do the same for the private names because this would
                -- break scoping.
            TimedSection _ _ ds -> mapM_ insertChannelsAndDataTypes ds
            _ -> return ()
    
        insertBoundNames :: NameMaker -> PDecl -> RenamerMonad ()
        insertBoundNames nameMaker pd = case unAnnotate pd of
            Assert _ -> return ()
            Channel _ _ -> return ()
            DataType rn _ -> do
                n <- nameMaker (loc pd) rn
                setName rn n
                setType rn (STDatatype n)
            SubType rn _ -> do
                n <- nameMaker (loc pd) rn
                setName rn n
            External ns ->
                mapM_ (\ rn@(UnQual ocn) -> do
                    case externalFunctionForOccName ocn of
                        Just b -> setName rn (name b)
                        Nothing -> 
                            let err = mkErrorMessage (loc pd) (externalFunctionNotRecognised rn)
                            in addErrors [err]) ns
            FunBind rn ms _ -> do
                n <- nameMaker (loc pd) rn
                setName rn n
            NameType rn _ -> do
                n <- nameMaker (loc pd) rn
                setName rn n
            PatBind p e _ -> renamePattern nameMaker p >> return ()
            Transparent ns -> 
                mapM_ (\ rn@(UnQual ocn) -> do
                    case transparentFunctionForOccName ocn of
                        Just b -> setName rn (name b)
                        Nothing ->
                            let err = mkErrorMessage (loc pd) (transparentFunctionNotRecognised rn)
                            in addErrors [err]) ns
            Module (UnQual mn) [] privDs pubDs -> do
                n <- nameMaker (loc pd) (UnQual mn)
                setName (UnQual mn) n
                -- This adds all of our exported names, and prefixes them with
                -- our module name (i.e. rn), as this is the only name with
                -- which they are available with in this context.

                -- Add the private names, but in a separate scope so they
                -- don't leak.
                rns <- addModuleContext mn $ addScope $ do
                    mapM_ insertChannelsAndDataTypes privDs
                    -- Check that the private and public declarations are
                    -- disjoint (but only after binding the channels).
                    checkDuplicates (pubDs++privDs)
                    mapM_ (insertBoundNames nameMaker) pubDs
                    -- Note that when the above addScope is popped we will
                    -- loose all of the public names that we just bound. Hence,
                    -- we get them out and return them below
                    fvs <- freeVars pubDs
                    mapM (\ rn -> do
                        rn' <- qualifyName rn
                        Just n <- lookupName rn'
                        return (rn, n)) fvs
                mapM_ (\(rn, n) -> setName (Qual mn rn) n) rns
            TimedSection _ _ ds -> do
                rns <- addScope $ do
                    mapM_ (\ n -> setName (UnQual (OccName n)) (builtInName n))
                        timedNames
                    mapM_ (insertBoundNames nameMaker) ds
                    fvs <- freeVars ds
                    mapM (\ rn -> do
                        Just n <- lookupName rn
                        return (rn, n)) fvs
                mapM_ (\(rn, n) -> setName rn n) rns
            _ -> return ()

        -- | resetModuleContext must be called in ALL cases, apart from the
        -- module context, otherwise names are incorrectly qualified within patterns etc.
        renameRightHandSide :: PDecl -> RenamerMonad TCDecl
        renameRightHandSide pd = reAnnotate pd $ case unAnnotate pd of
            Assert e -> resetModuleContext $ do
                e' <- addScope $ rename e
                return $ Assert e'
            Channel rns e -> resetModuleContext $ do
                ns <- mapM renameVarRHS rns
                e' <- addScope $ rename e
                return $ Channel ns e'
            DataType rn cs -> resetModuleContext $ do
                n <- renameVarRHS rn
                cs' <- mapM (\ pc -> addScope $ reAnnotate pc $ case unAnnotate pc of
                                DataTypeClause rn e -> do
                                    n' <- renameVarRHS rn
                                    e' <- rename e
                                    return $ DataTypeClause n' e') cs
                return $ DataType n cs'
            SubType rn cs -> resetModuleContext $ do
                n <- renameVarRHS rn
                cs' <- mapM (\ pc -> addScope $ reAnnotate pc $ case unAnnotate pc of
                                DataTypeClause rn e -> do
                                    n' <- renameVarRHS rn
                                    e' <- rename e
                                    return $ DataTypeClause n' e') cs
                return $ SubType n cs'
            External rns -> resetModuleContext $ do
                ns <- mapM renameVarRHS rns
                return $ External ns
            FunBind rn ms ta -> resetModuleContext $ do
                n <- renameVarRHS rn
                addTypeScope $ do
                    -- Must be done here in types are in scope in expressions
                    ta' <- rename ta
                    ms' <- mapM rename ms
                    return $ FunBind n ms' ta'
            NameType rn e -> resetModuleContext $ do
                n <- renameVarRHS rn
                e' <- addScope $ rename e
                return $ NameType n e'
            PatBind p e ta -> resetModuleContext $ do
                p' <- renamePattern ignoringNameMaker p
                addTypeScope $ do
                    -- Must be done here in types are in scope in expressions
                    ta' <- rename ta
                    e' <- addScope $ rename e
                    return $ PatBind p' e' ta'
            Transparent rns -> resetModuleContext $ do
                ns <- mapM renameVarRHS rns
                return $ Transparent ns
            Module (UnQual mn) [] privDs pubDs -> do
                n' <- renameVarRHS (UnQual mn)
                -- Add a scope for the private declarations.
                -- We now insert bound names in this scope, initially we
                -- fully qualify the names.
                addScope $ addModuleContext mn $ do
                    mapM_ insertChannelsAndDataTypes privDs
                    mapM_ (insertBoundNames nameMaker) privDs
                    -- Now, we need to make the names available without
                    -- prefixing, but we NEED to make sure that the Name s that
                    -- we bound above are the same for the two different
                    -- occurences.
                    fvs <- freeVars (privDs++pubDs)
                    -- Lookup the fully qualified names
                    npairs <- mapM (\ rn -> do
                        rn' <- qualifyName rn
                        Just n' <- lookupName rn'
                        return (rn, n')) fvs
                    -- Insert them (we reset the module context to make sure
                    -- they are not fully qualified).
                    resetModuleContext $ mapM_ (\(rn, n) -> do
                        setName rn n) npairs

                    privDs' <- mapM renameRightHandSide privDs
                    pubDs' <- mapM renameRightHandSide pubDs
                    return $ Module n' [] privDs' pubDs'
            TimedSection Nothing f ds -> do
                f' <- addScope $ rename f
                tock <- renameVarRHS (UnQual (OccName "tock"))
                addScope $ do
                    mapM_ (\ n -> setName (UnQual (OccName n)) (builtInName n))
                        timedNames
                    ds' <- mapM renameRightHandSide ds
                    return $ TimedSection (Just tock) f' ds'

renamePattern :: NameMaker -> PPat -> RenamerMonad TCPat
renamePattern nm ap =
    let
        renamePat :: PPat -> RenamerMonad TCPat
        renamePat ap = reAnnotate ap $ case unAnnotate ap of
            PConcat p1 p2 -> return PConcat $$ renamePat p1 $$ renamePat p2
            PDotApp p1 p2 -> return PDotApp $$ renamePat p1 $$ renamePat p2
            PDoublePattern p1 p2 -> 
                return PDoublePattern $$ renamePat p1 $$ renamePat p2
            PList ps -> return PList $$ mapM renamePat ps
            PLit l -> return $ PLit l
            PParen p -> return PParen $$ renamePat p
            PSet ps -> return PSet $$ mapM renamePat ps
            PTuple ps -> return PTuple $$ mapM renamePat ps
            PVar v -> do
                -- In this case, we should lookup the variable in the map and see if it is
                -- bound. If it is and is a datatype constructor, then we should return
                -- return that, otherwise we create a new internal var.
                mn <- lookupName v
                case mn of
                    Just n | isNameDataConstructor n -> do
                        -- Just return the name
                        return (PVar n)
                    _ ->
                        case v of
                            Qual _ _ -> do
                                loc <- gets srcSpan
                                addErrors [mkErrorMessage loc (qualifiedVarNotAllowedInPat (PVar v))]
                                return $ panic "unknown name evaluated"
                            UnQual _ -> do
                                -- Create the name
                                n <- nm (loc ap) v
                                setName v n
                                return $ PVar n
            PWildCard -> return PWildCard
    in do
        checkDuplicates [ap]
        renamePat ap

checkDuplicates :: FreeVars a => [Annotated b a] -> RenamerMonad ()
checkDuplicates aps = do
    fvss <- mapM freeVars aps
    let 
        fvs = sort (concat fvss)
        gfvs = group fvs
        duped = filter (\l -> length l > 1) gfvs
        nameLocMap = 
            concatMap (\(ns, d) -> [(n, loc d) | n <- ns])  (zip fvss aps)
    
    loc <- gets srcSpan
    if duped /= [] then
        throwSourceError (map (mkErrorMessage loc) (duplicatedDefinitionsMessage nameLocMap))
    else return ()

-- | Rename a variable on the right hand side of a definition.
-- 
-- If the variable does not exist it returns an error thunk and adds an error
-- that will be raised when the monad is evaluated.
renameVarRHS :: UnRenamedName -> RenamerMonad Name
renameVarRHS n = do
    mn <- lookupName n
    case mn of
        Just x -> return x
        Nothing -> do
            msg <- varNotInScopeMessage n
            loc <- gets srcSpan
            addErrors [mkErrorMessage loc msg]
            return $ panic "error name evaluated"

checkFieldsValid :: [PField] -> RenamerMonad ()
checkFieldsValid fs = do
    let
        chk [] = True
        chk (NonDetInput _ _ : fs) = chk fs
        chk (_ : fs) = chk' fs

        chk' [] = True
        chk' (NonDetInput _ _ : fs) = False
        chk' (_ : fs) = chk' fs
    when (not (chk (map unAnnotate fs))) $ do
        loc <- gets srcSpan
        throwSourceError $ [mkErrorMessage loc $ invalidFieldsErrorMessage fs]

renameFields :: [PField] -> RenamerMonad a -> RenamerMonad ([TCField], a)
renameFields fs inner = do
    checkDuplicates fs
    checkFieldsValid fs
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

            fs' :: [TCField]
            fs' = combineJusts fsNonDet fsDet

        -- all fields renamed, now rename the inner thing
        a <- inner
        return (fs', a))

renameStatements :: [PStmt] -> RenamerMonad a -> RenamerMonad ([TCStmt], a)
renameStatements stmts inner = do
    checkDuplicates stmts
    -- No duplicates, so we can just add one scope
    addScope (do
        -- We do the stmts left to right, to ensure that the scoping is correct
        stmts' <- mapM (\ astmt -> reAnnotate astmt $ 
                case unAnnotate astmt of
                    Generator p e -> do
                        -- Rename e first, as it can't depend on p
                        e' <- rename e
                        p' <- renamePattern internalNameMaker p
                        return $ Generator p' e'
                    Qualifier e -> do
                        e' <- rename e
                        return $ Qualifier e'
            ) stmts
        -- all stmts renamed, now rename the inner thing
        a <- inner
        return (stmts', a))

instance Renamable (Match UnRenamedName) (Match Name) where
    rename (Match pss e) = addScope $ do
        checkDuplicates (concat pss)
        pss' <- mapM (mapM (renamePattern internalNameMaker)) pss
        e' <- rename e
        return $ Match pss' e'

instance Renamable (InteractiveStmt UnRenamedName) (InteractiveStmt Name) where
    rename (Bind ds) = do
        (ds',_) <- renameDeclarations True ds (return ())
        return $ Bind ds'
    rename (Evaluate e) = return Evaluate $$ rename e
    rename (RunAssertion a) = return RunAssertion $$ rename a

instance Renamable (CSPMFile UnRenamedName) (CSPMFile Name) where
    rename (CSPMFile ds) = do
        (ds', _) <- renameDeclarations True ds (return ())
        return $ CSPMFile ds'

instance Renamable (Assertion UnRenamedName) (Assertion Name) where
    rename (ASNot e) =
        return ASNot $$ rename e
    rename (Refinement e1 m e2 mopts) = 
        return Refinement $$ rename e1 $$ return m $$ rename e2 $$ rename mopts
    rename (PropertyCheck e1 p m) =
        return PropertyCheck $$ rename e1 $$ return p $$ return m

instance Renamable (ModelOption UnRenamedName) (ModelOption Name) where
    rename (TauPriority e) = return TauPriority $$ rename e

instance Renamable (Exp UnRenamedName) (Exp Name) where
    rename (App e es) = return App $$ rename e $$ rename es
    rename (BooleanBinaryOp op e1 e2) = return 
        (BooleanBinaryOp op) $$ rename e1 $$ rename e2
    rename (BooleanUnaryOp op e) = return
        (BooleanUnaryOp op) $$ rename e
    rename (Concat e1 e2) = return Concat $$ rename e1 $$ rename e2
    rename (DotApp e1 e2) = return DotApp $$ rename e1 $$ rename e2
    rename (If e1 e2 e3) = return If $$ rename e1 $$ rename e2 $$ rename e3
    rename (Lambda p e) = do
        (p', e') <- addScope (do
                p' <- renamePattern internalNameMaker p
                e' <- rename e
                return (p', e'))
        return $ Lambda p' e'
    rename (Let ds e) = do
        (ds', e') <- addScope $ renameDeclarations False ds (rename e)
        return $ Let ds' e'
    rename (Lit l) = return $ Lit l
    rename (List es) = return List $$ rename es
    rename (ListComp es stmts) = do
        (stmts', es') <- renameStatements stmts (rename es)
        return $ ListComp es' stmts'
    rename (ListEnumFrom e) = return ListEnumFrom $$ rename e
    rename (ListEnumFromTo e1 e2) = return ListEnumFromTo $$ rename e1 $$ rename e2
    rename (ListEnumFromComp e stmts) = do
        (stmts', e') <- renameStatements stmts (rename e)
        return $ ListEnumFromComp e' stmts'
    rename (ListEnumFromToComp e1 e2 stmts) = do
        (stmts', (e1', e2')) <- renameStatements stmts $ do
            e1' <- rename e1
            e2' <- rename e2
            return (e1', e2')
        return $ ListEnumFromToComp e1' e2' stmts'
    rename (ListLength e) = return ListLength $$ rename e
    rename (MathsBinaryOp op e1 e2) = return 
        (MathsBinaryOp op) $$ rename e1 $$ rename e2
    rename (MathsUnaryOp op e) = return (MathsUnaryOp op) $$ rename e
    rename (Paren e) = return Paren $$ rename e
    rename (Set es) = return Set $$ rename es
    rename (SetComp es stmts) = do
        (stmts', es') <- renameStatements stmts (rename es)
        return $ SetComp es' stmts'
    rename (SetEnum es) = return SetEnum $$ rename es
    rename (SetEnumComp es stmts) = do
        (stmts', es') <- renameStatements stmts (rename es)
        return $ SetEnumComp es' stmts'
    rename (SetEnumFrom e) = return SetEnumFrom $$ rename e
    rename (SetEnumFromTo e1 e2) = return SetEnumFromTo $$ rename e1 $$ rename e2
    rename (SetEnumFromComp e stmts) = do
        (stmts', e') <- renameStatements stmts (rename e)
        return $ SetEnumFromComp e' stmts'
    rename (SetEnumFromToComp e1 e2 stmts) = do
        (stmts', (e1', e2')) <- renameStatements stmts $ do
            e1' <- rename e1
            e2' <- rename e2
            return (e1', e2')
        return $ SetEnumFromToComp e1' e2' stmts'
    rename (Tuple es) = return Tuple $$ rename es
    rename (Var n) = return Var $$ renameVarRHS n

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
    rename (SynchronisingExternalChoice e1 e2 e3) =
        return SynchronisingExternalChoice $$ rename e1 $$ rename e2 $$ rename e3
    rename (SynchronisingInterrupt e1 e2 e3) =
        return SynchronisingInterrupt $$ rename e1 $$ rename e2 $$ rename e3

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
    rename (ReplicatedSequentialComp stmts e) = do
        (stmts', e') <- renameStatements stmts (rename e)
        return $ ReplicatedSequentialComp stmts' e'
    rename (ReplicatedSynchronisingExternalChoice e1 stmts e2) = do
        e1' <- rename e1
        (stmts', e2') <- renameStatements stmts (rename e2)
        return $ ReplicatedSynchronisingExternalChoice e1' stmts' e2'

instance Renamable (STypeScheme UnRenamedName) (STypeScheme Name) where
    rename (STypeScheme [] cs t) = do
        loc <- gets srcSpan
        -- Inject the type-variables we need
        tvars <- freeTypeVars t >>= return
                . nubBy (\ x y -> fst x == fst y)
                . sortBy (\ x y -> compare (fst x) (fst y))
        ns <- mapM (\ (rn, _) -> do
            n <- internalNameMaker loc rn
            setType rn $ STVar n
            return n) tvars
        return STypeScheme $$ return ns $$ rename cs $$ rename t

instance Renamable (STypeConstraint UnRenamedName) (STypeConstraint Name) where
    rename (STypeConstraint c n) =
        return STypeConstraint $$ return c $$ do
            n' <- renameTypeVar n 
            case n' of
                Nothing -> return renameTypeVarThunk
                Just (STVar n) -> return n
                Just t -> do
                    loc <- gets srcSpan
                    let msg = invalidTypeConstraintVariable t
                    addErrors [mkErrorMessage loc msg]
                    return renameTypeVarThunk

instance Renamable (SType UnRenamedName) (SType Name) where
    rename (STVar n) = renameTypeVar' n
    rename (STExtendable t n) =
        return STExtendable $$ rename t $$ do
            n' <- renameTypeVar n
            case n' of
                Nothing -> return renameTypeVarThunk
                Just (STVar n) -> return n
                Just t -> do
                    loc <- gets srcSpan
                    let msg = invalidExtendableTypeVariable t
                    addErrors [mkErrorMessage loc msg]
                    return $ panic "renameSTExtendable: error thunk"
    rename (STSet t) = return STSet $$ rename t
    rename (STSeq t) = return STSeq $$ rename t
    rename (STDot t1 t2) = return STDot $$ rename t1 $$ rename t2
    rename (STTuple ts) = return STTuple $$ rename ts
    rename (STFunction args rt) = return STFunction $$ rename args $$ rename rt
    rename (STDotable t1 t2) = return STDotable $$ rename t1 $$ rename t2
    rename (STParen t) = return STParen $$ rename t

renameTypeVarThunk :: a
renameTypeVarThunk = panic "renameTypeVar': error thunk evaluated"

renameTypeVar' :: UnRenamedName -> RenamerMonad (SType Name)
renameTypeVar' n = do
    mn <- renameTypeVar n
    case mn of
        Just n -> return n
        Nothing -> return renameTypeVarThunk

renameTypeVar :: UnRenamedName -> RenamerMonad (Maybe (SType Name))
renameTypeVar n = do
    mn <- lookupType n
    case mn of
        Just x -> return $ Just x
        Nothing -> do
            msg <- typeVarNotInScopeMessage n
            loc <- gets srcSpan
            addErrors [mkErrorMessage loc msg]
            return Nothing

class FreeVars a where
    freeVars :: a -> RenamerMonad [UnRenamedName]

instance FreeVars a => FreeVars [a] where
    freeVars xs = concatMapM freeVars xs
    
instance FreeVars a => FreeVars (Annotated b a) where
    freeVars (An _ _ inner) = freeVars inner

instance FreeVars (Pat UnRenamedName) where
    freeVars (PConcat p1 p2) = do
        fvs1 <- freeVars p1
        fvs2 <- freeVars p2
        return $ fvs1++fvs2
    freeVars (PDotApp p1 p2) = freeVars [p1,p2]
    freeVars (PDoublePattern p1 p2) = do
        fvs1 <- freeVars p1
        fvs2 <- freeVars p2
        return $ fvs1++fvs2
    freeVars (PList ps) = freeVars ps
    freeVars (PLit l) = return []
    freeVars (PParen p) = freeVars p
    freeVars (PSet ps) = freeVars ps
    freeVars (PTuple ps) = freeVars ps
    freeVars (PVar n) = do
        rn <- qualifyName n
        mn <- lookupName rn
        case mn of
            Just n | isNameDataConstructor n -> return []
            _ -> case n of
                    Qual _ _ -> return [] -- we pick the error up in renamePattern
                    _ -> return [n]
    freeVars (PWildCard) = return []

instance FreeVars (Stmt UnRenamedName) where
    freeVars (Qualifier e) = return []
    freeVars (Generator p e) = freeVars p

instance FreeVars (Field UnRenamedName) where
    freeVars (Input p e) = freeVars p
    freeVars (NonDetInput p e) = freeVars p
    freeVars (Output e) = return []

instance FreeVars (Decl UnRenamedName) where
    freeVars (Assert _) = return []
    freeVars (External ns) = return ns
    freeVars (Channel ns _) = return ns
    freeVars (DataType n cs) = 
        return $ n:[n' | DataTypeClause n' _ <- map unAnnotate cs]
    freeVars (SubType n _) = return [n]
    freeVars (FunBind n _ _) = return [n]
    freeVars (NameType n _) = return [n]
    freeVars (PatBind p _ _) = freeVars p
    freeVars (Transparent ns) = return ns
    freeVars (Module (UnQual n) ps _ expDs) = do
        ns <- freeVars expDs
        return $ UnQual n : map (Qual n) ns
    freeVars (TimedSection _ _ ds) = freeVars ds

freeTypeVars :: PSType -> RenamerMonad [(UnRenamedName, SrcSpan)]
freeTypeVars st =
    let
        freeTypeVarsL = concatMapM freeTypeVars
        freeVars (STVar n) = do
            mt <- lookupType n
            return $! case mt of
                Just _ -> []
                Nothing -> [(n, loc st)]
        freeVars (STExtendable t n) = do
            ns <- freeTypeVars t
            mt <- lookupType n
            return $! case mt of
                Just _ -> ns
                Nothing -> (n, loc st):ns
        freeVars (STSet t) = freeTypeVars t
        freeVars (STSeq t) = freeTypeVars t
        freeVars (STDot t1 t2) = freeTypeVarsL [t1, t2]
        freeVars (STTuple ts) = freeTypeVarsL ts
        freeVars (STFunction args rt) = freeTypeVarsL (rt:args)
        freeVars (STDotable t1 t2) = freeTypeVarsL [t1, t2]
        freeVars (STParen t) = freeTypeVars t
    in freeVars (unAnnotate st)

-- ********************
-- Error Messages

type Error = Doc

duplicatedDefinitionsMessage :: [(UnRenamedName, SrcSpan)] -> [Error]
duplicatedDefinitionsMessage ns = duplicatedDefinitionsMessage' $
    let
        names = map fst ns
        dupNames = (map head . filter (\ g -> length g > 1) . group . sort) names
    in [(n, applyRelation ns n) | n <- dupNames]

duplicatedDefinitionsMessage' :: [(UnRenamedName, [SrcSpan])] -> [Error]
duplicatedDefinitionsMessage' nlocs = 
    map (\ (n, spans) ->
        hang (text "The variable" <+> prettyPrint n 
                <+> text "has multiple definitions at" <> colon) tabWidth
            (vcat (map prettyPrint spans))) nlocs

transparentFunctionNotRecognised :: UnRenamedName -> Error
transparentFunctionNotRecognised n =
    text "The transparent function" <+> prettyPrint n <+> 
    text "is not recognised."

externalFunctionNotRecognised :: UnRenamedName -> Error
externalFunctionNotRecognised n = 
    text "The external function" <+> prettyPrint n <+> 
    text "is not recognised."

varNotInScopeMessage :: UnRenamedName -> RenamerMonad Error
varNotInScopeMessage n = do
    env <- gets environment
    let availablePp = [(show $ prettyPrint rn, (rn, n)) | (rn, n) <- HM.flatten env]
        suggestions = fuzzyLookup (show $ prettyPrint n) availablePp

        availableTransExtern =
            [(stringName b, b) | b <- builtins True, isTransparent b || isExternal b]
        builtinSuggestions = fuzzyLookup (show $ prettyPrint n) availableTransExtern
    return $ 
        (prettyPrint n <+> text "is not in scope")
        P.$$ case suggestions of
            [] -> case builtinSuggestions of
                    [] -> empty
                    xs -> hang (text "Did you mean:") tabWidth (vcat (
                            map (\ b ->
                                prettyPrint (name b)
                                <+> parens (text "import using" <+> quotes (
                                    (text $ if isTransparent b then "transparent" else "external")
                                    <+> prettyPrint (name b)
                                ))) builtinSuggestions
                        ))
            _ -> hang (text "Did you mean:") tabWidth (vcat (
                    (map (\ (rn, n) -> 
                        prettyPrint rn <+>
                        case nameDefinition n of
                            Unknown -> empty
                            l -> parens (text "defined at" <+> prettyPrint l)
                     ) suggestions)))

typeVarNotInScopeMessage :: UnRenamedName -> RenamerMonad Error
typeVarNotInScopeMessage n = do
    env <- gets typeEnvironment
    let availablePp = [(show $ prettyPrint rn, (rn, n)) | (rn, n) <- HM.flatten env]
        suggestions = fuzzyLookup (show $ prettyPrint n) availablePp
    return $ 
        (text "The type-variable" <+> prettyPrint n <+> text "is not in scope")
        P.$$ case suggestions of
            [] -> empty
            _ -> hang (text "Did you mean:") tabWidth (vcat (
                    (map (\ (rn, t) -> 
                        prettyPrint rn <+>
                        case t of 
                            STVar n | nameDefinition n /= Unknown ->
                                parens (text "defined at" <+> prettyPrint
                                    (nameDefinition n))
                            _ -> empty
                     ) suggestions)))

qualifiedVarNotAllowedInPat :: Pat UnRenamedName -> Error
qualifiedVarNotAllowedInPat p =
    prettyPrint p <+>
    text "is an invalid pattern as non-datatype/channel qualified names are not allowed"

invalidFieldsErrorMessage :: [PField] -> Error
invalidFieldsErrorMessage fs =
    hcat (map prettyPrint fs) <+>
    text "is not a valid field sequence as a $ occurs after a ? or a !."

invalidExtendableTypeVariable :: PrettyPrintable id => SType id -> Error
invalidExtendableTypeVariable t = 
    text "The type" <+> prettyPrint t
    P.$$ text "is not valid as the left hand side of =>*, which must be a type-variable."

invalidTypeConstraintVariable :: PrettyPrintable id => SType id -> Error
invalidTypeConstraintVariable t =
    text "The type" <+> prettyPrint t <+>
        text "cannot be constrainted using a type constraint;"
    P.$$ text "as only type variables may be constrained."
