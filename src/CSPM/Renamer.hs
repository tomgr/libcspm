{-# LANGUAGE FlexibleInstances, FunctionalDependencies,
    ExistentialQuantification, RankNTypes,
    OverloadedStrings, TypeSynonymInstances, MultiParamTypeClasses,
    UndecidableInstances #-}
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
import qualified Data.ByteString.Char8 as B
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust,isNothing)
import qualified Data.Set as S
import Prelude hiding ((<>))

import CSPM.Syntax.Names
import CSPM.Syntax.AST
import CSPM.Prelude
import CSPM.PrettyPrinter
import Util.Annotated
import Util.Exception
import Util.FuzzyLookup
import qualified Util.HierarchicalMap as HM
import Util.Monad
import Util.PrettyPrint hiding (($$))
import qualified Util.PrettyPrint as P

data Visibility =
    Public
    | Private
    | Instance
    deriving (Eq, Ord, Show)

data RenamedName = RenamedName {
        renamedName :: Name,
        renamedNameIsModule :: Bool,
        renamedNameVisibility :: Visibility
    }
    deriving Show

data RenamedType = RenamedType {
        renamedType :: SType Name,
        renamedTypeVisibility :: Visibility
    }
    deriving Show

type RenameEnvironment = HM.HierarchicalMap UnRenamedName RenamedName
type TypeRenameEnvironment = HM.HierarchicalMap UnRenamedName RenamedType

data ModuleInformation =
    ModuleInformation {
        declaration :: PDecl,
        renamedDeclaration :: Maybe TCDecl,
        -- | The visibility of the names in the module.
        nameVisibility :: M.Map UnRenamedName Visibility,
        typeNameVisibility :: M.Map UnRenamedName Visibility,
        publicBoundModules :: [UnRenamedName],
        privateBoundModules :: [UnRenamedName],
        publicBoundLabels :: [UnRenamedName],
        privateBoundLabels :: [UnRenamedName],
        publicBoundNames :: [UnRenamedName],
        privateBoundNames :: [UnRenamedName],
        publicTypeNames :: [UnRenamedName],
        privateTypeNames :: [UnRenamedName]
    }
    | ModuleInstanceInformation {
        resolvedTarget :: Name
    }
    deriving Show

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
        currentModuleQualificationStack :: [OccName],
        -- | The modules that are in scope. Used for module instances.
        foundModules :: M.Map Name ModuleInformation
    }

type RenamerMonad = StateT RenamerState IO

-- | Initialises the renamer.
initRenamer :: IO (RenamerState)
initRenamer = do
    let bs = map (\b -> (UnQual (OccName (stringName b)), RenamedName {
                        renamedName = name b,
                        renamedNameIsModule = False,
                        renamedNameVisibility = Public
                    }))
                (builtins False)
        bts = map (\ (s, t) -> (UnQual (OccName s), RenamedType {
                    renamedType = t,
                    renamedTypeVisibility = Public
                })) [
                    ("Proc", STProc),
                    ("Int", STInt),
                    ("Bool", STBool),
                    ("Char", STChar),
                    ("Event", STEvent)
                ]
    return $ RenamerState {
        -- We insert a new layer to allow builtins to be overridden
        environment = HM.newLayer (HM.updateMulti HM.new  bs),
        typeEnvironment = HM.newLayer (HM.updateMulti HM.new bts),
        srcSpan = Unknown,
        errors = [],
        currentModuleQualificationStack = [],
        foundModules = M.empty
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
    let isInteresting (_, rname) =
            nameType (renamedName rname) `elem` [ExternalName, WiredInName]
            && renamedNameVisibility rname == Public
            && not (renamedNameIsModule rname)
    return $ map (renamedName . snd) $ filter isInteresting $ HM.flatten env

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

lookupType :: UnRenamedName -> RenamerMonad (Maybe RenamedType)
lookupType n = do
    env <- gets typeEnvironment
    return $! HM.maybeLookup env n

setType :: UnRenamedName -> SType Name -> RenamerMonad ()
setType rn t = setTypeWithVisibility rn t Public

setTypeWithVisibility :: UnRenamedName -> SType Name -> Visibility ->
    RenamerMonad ()
setTypeWithVisibility rn t vis = modify (\ st -> st {
        typeEnvironment = HM.update (typeEnvironment st) rn (RenamedType t vis)
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

lookupName :: UnRenamedName -> Bool -> RenamerMonad (Maybe Name)
lookupName n isModule = lookupName' n isModule False

lookupMaybeHiddenName :: UnRenamedName -> Bool -> RenamerMonad (Maybe Name)
lookupMaybeHiddenName n isModule = lookupName' n isModule True

lookupName' :: UnRenamedName -> Bool -> Bool -> RenamerMonad (Maybe Name)
lookupName' n isModule ignoreVisibility = do
    env <- gets environment
    return $!
        case HM.maybeLookup env n of
            Just (RenamedName n isModule' isVisible)
                    | isModule == isModule' &&
                        (isVisible == Public || ignoreVisibility) ->
                Just n
            _ -> Nothing

currentVisibilityOfName :: UnRenamedName -> RenamerMonad Visibility
currentVisibilityOfName rn = do
    env <- gets environment
    return $!
        case HM.maybeLookup env rn of
            Just (RenamedName _ _ v) -> v
            _ -> panic "Cannot find name for visibility checking"

qualify :: OccName -> [OccName] -> UnRenamedName
qualify on [] = UnQual on
qualify on (n:ns) = Qual n (qualify on ns)

qualifyName :: UnRenamedName -> RenamerMonad UnRenamedName
qualifyName (UnQual on) = do
    ns <- gets currentModuleQualificationStack
    return $! qualify on (reverse ns)
qualifyName (Qual on rn) = do
    n <- qualifyName (UnQual on)
    let qualify (UnQual on) = Qual on rn
        qualify (Qual mn rn) = Qual mn (qualify rn)
    return $! qualify n

setName :: UnRenamedName -> Name -> RenamerMonad ()
setName rn n = setRenamedName rn (RenamedName n False Public)

setNameWithVisibility :: UnRenamedName -> Name -> Visibility -> RenamerMonad ()
setNameWithVisibility rn n vis = setRenamedName rn (RenamedName n False vis)

setModuleName :: UnRenamedName -> Name -> Visibility -> RenamerMonad ()
setModuleName rn n vis = setRenamedName rn (RenamedName n True vis)

setRenamedName :: UnRenamedName -> RenamedName -> RenamerMonad ()
setRenamedName rn renamed = do
    rn' <- qualifyName rn
    modify (\ st -> st { environment = HM.update (environment st) rn renamed })

informationForModule :: SrcSpan -> UnRenamedName -> RenamerMonad ModuleInformation
informationForModule loc rn = do
    env <- gets environment
    mn <- lookupMaybeHiddenName rn True
    case mn of
        Just n -> informationForModuleName n
        Nothing -> do
            msg <- varNotInScopeMessage rn True
            throwSourceError [mkErrorMessage loc msg]

informationForModuleName :: Name -> RenamerMonad ModuleInformation
informationForModuleName n = do
    m <- gets foundModules
    case M.lookup n m of
        Just (ModuleInstanceInformation nt) -> informationForModuleName nt
        Just x -> return x
        Nothing -> panic "Found module name but couldn't find module"

setSrcSpan :: SrcSpan -> RenamerMonad ()
setSrcSpan loc = modify (\ st -> st { srcSpan = loc })

-- | Report a message as an error. This will be raised at the outer monad level.
addErrors :: [ErrorMessage] -> RenamerMonad ()
addErrors msgs = do
    modify (\st -> st { errors = msgs ++ errors st })
    errs <- gets errors
    when (length errs > 20) $ do
        let lastError = mkErrorMessage Unknown (text "Too many errors generated. Stopping")
        throwSourceError $! lastError : errs

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

addModule :: Name -> PDecl -> RenamerMonad ()
addModule n d =
    modify (\ st -> st {
        foundModules = M.insert n
            (ModuleInformation d Nothing M.empty M.empty [] [] [] [] [] [] [] [])
            (foundModules st)
    })

addModuleInstance :: Name -> Name -> RenamerMonad ()
addModuleInstance n nt =
    modify (\ st -> st {
        foundModules = M.insert n (ModuleInstanceInformation nt) (foundModules st)
    })

updateModuleInformation :: Name -> (ModuleInformation -> ModuleInformation) ->
    RenamerMonad ()
updateModuleInformation n f = do
    modify (\ st -> st { foundModules = M.adjust f n (foundModules st) })

addBoundModuleModules :: Name -> [UnRenamedName] -> [UnRenamedName] ->
    RenamerMonad ()
addBoundModuleModules mn privNs pubNs = updateModuleInformation mn $
    \ st -> st { privateBoundModules = privNs, publicBoundModules = pubNs }

addBoundModuleLabels :: Name -> [UnRenamedName] -> [UnRenamedName] -> RenamerMonad ()
addBoundModuleLabels mn privNs pubNs = updateModuleInformation mn $
    \ st -> st { privateBoundLabels = privNs, publicBoundLabels = pubNs }

addBoundModuleBoundNames :: Name -> [UnRenamedName] -> [UnRenamedName] -> RenamerMonad ()
addBoundModuleBoundNames mn privNs pubNs = updateModuleInformation mn $
    \ st -> st { privateBoundNames = privNs, publicBoundNames = pubNs }

addBoundModuleTypeNames :: Name -> [UnRenamedName] -> [UnRenamedName] -> RenamerMonad ()
addBoundModuleTypeNames mn privNs pubNs = updateModuleInformation mn $
    \ st -> st { privateTypeNames = privNs, publicTypeNames = pubNs }

addBoundModuleRenamedVersion :: Name -> TCDecl -> RenamerMonad ()
addBoundModuleRenamedVersion mn td = updateModuleInformation mn $
    \ st -> st { renamedDeclaration = Just td }

setNameVisibilityInModule :: Name -> [UnRenamedName] -> [UnRenamedName] ->
    RenamerMonad ()
setNameVisibilityInModule mn ns tns = do
    env <- gets environment
    mapM_ (\ n -> case HM.maybeLookup env n of
        Nothing -> panic "Cannot find name for visibility"
        Just renamed ->
            updateModuleInformation mn $ \ st -> st {
                nameVisibility = M.insert n (renamedNameVisibility renamed)
                        (nameVisibility st)
            }
        ) ns
    tenv <- gets typeEnvironment
    mapM_ (\ n -> case HM.maybeLookup tenv n of
        Nothing -> panic "Cannot find name for visibility"
        Just renamed ->
            updateModuleInformation mn $ \ st -> st {
                typeNameVisibility = M.insert n (renamedTypeVisibility renamed)
                        (typeNameVisibility st)
            }
        ) tns

nameVisibilityInModule :: UnRenamedName -> UnRenamedName -> RenamerMonad Visibility
nameVisibilityInModule umrn n = do
    env <- gets environment
    Just mn <- lookupMaybeHiddenName umrn True
    minfo <- informationForModuleName mn
    case M.lookup n (nameVisibility minfo) of
        Just v -> return v
        Nothing -> panic "Name does not have a visibility in module"

typeNameVisibilityInModule :: UnRenamedName -> UnRenamedName -> RenamerMonad Visibility
typeNameVisibilityInModule umrn n = do
    env <- gets environment
    Just mn <- lookupMaybeHiddenName umrn True
    minfo <- informationForModuleName mn
    case M.lookup n (typeNameVisibility minfo) of
        Just v -> return v
        Nothing -> panic "Type name does not have a visibility in module"

hideBoundNames :: [UnRenamedName] -> [UnRenamedName] ->
    RenamerMonad [UnRenamedName]
hideBoundNames ns tns = do
    let nameSet = S.fromList ns
        change rn x | S.member rn nameSet =
            x { renamedNameVisibility = Private }
        change rn x = x
    modify (\ st -> st { environment = HM.map change (environment st) })
    let nameSet = S.fromList tns
        change rn x | S.member rn nameSet =
            x { renamedTypeVisibility = Private }
        change rn x = x
    modify (\ st -> st { typeEnvironment = HM.map change (typeEnvironment st) })
    return ns

unhideModuleNames :: OccName -> RenamerMonad a -> RenamerMonad a
unhideModuleNames prefix prog = addScope $ do
    minfo <- informationForModule Unknown (UnQual prefix)
    let unhide rn (RenamedName x y _) = RenamedName x y $!
            case M.lookup rn (nameVisibility minfo) of
                Just v -> v
                Nothing -> panic $ "Cannot find visibility of "++show x
                    ++" in "++show (prettyPrint prefix)
        unhideType rn (RenamedType x _) = RenamedType x $!
            case M.lookup rn (typeNameVisibility minfo) of
                Just v -> v
                Nothing -> panic "Cannot find type vis"
    let ns = publicBoundModules minfo ++ privateBoundModules minfo ++
                publicBoundLabels minfo ++ privateBoundLabels minfo ++
                publicBoundNames minfo ++ privateBoundNames minfo
    -- Unhide the names
    modify (\ st -> st {
        environment = HM.updateMulti (environment st)
            [(rn, unhide rn (HM.lookup (environment st) (Qual prefix rn)))
                | rn <- ns],
        typeEnvironment = HM.updateMulti (typeEnvironment st)
            [(rn, unhideType rn (HM.lookup (typeEnvironment st) (Qual prefix rn)))
                | rn <- privateTypeNames minfo ++ publicTypeNames minfo]
        })
    prog

-- | Given a module name, runs the provided program in a new scope in which
-- all variables that were bound with the given module name as a prefix in
-- scope.
prefixNamesFromScope :: Bool -> OccName ->
    RenamerMonad ([UnRenamedName], [UnRenamedName]) ->
    RenamerMonad ([UnRenamedName], [UnRenamedName])
prefixNamesFromScope isParamterised prefix prog = do
    (rns, rnts) <- unhideModuleNames prefix $ do
        (ns, tns) <- prog
        env <- gets environment
        tenv <- gets typeEnvironment
        let rns = map (\ rn -> (rn,
                case HM.maybeLookup env rn of
                    Just r -> r
                    Nothing -> panic "Cannot find name to prefix")) ns
            rnts = map (\ rn -> (rn,
                case HM.maybeLookup tenv rn of
                    Just r -> r
                    Nothing -> panic "Cannot find type name to prefix")) tns
        return (rns, rnts)
    ns <- mapM (\ (rn, renamed) -> do
        let rn' = Qual prefix rn
        setRenamedName rn' $!
            if isParamterised then renamed { renamedNameVisibility = Instance }
            else renamed
        return rn') rns
    tns <- mapM (\ (rn, RenamedType t vis) -> do
        let rn' = Qual prefix rn
        setTypeWithVisibility rn' t (if isParamterised then Instance else vis)
        return rn') rnts
    return (ns, tns)

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

timedNames :: [B.ByteString]
timedNames = ["timed_priority", "WAIT"]

joinName :: UnRenamedName -> UnRenamedName -> UnRenamedName
joinName (UnQual mn) x = Qual mn x
joinName (Qual x y) z = Qual x (joinName y z)

concatMapM' :: Monad m => (a -> m ([b], [c])) -> [a] -> m ([b], [c])
concatMapM' f xs = do
    bscs <- mapM f xs
    let (bs, cs) = unzip bscs
    return (concat bs, concat cs)

-- | Renames the declarations in the current scope.
renameDeclarations :: Bool -> [PDecl] -> RenamerMonad a -> RenamerMonad ([TCDecl], a)
renameDeclarations topLevel ds prog = do
    -- Check to see if any modules are multi-defined (although we check for
    -- duplicates below, because of the preprocessing that modules require this
    -- comes too late and would result in various odd errors being emitted).
    checkModuleNameDuplicates ds
    -- Insert the modules
    mapM_ insertModule ds
    -- Now, find all the datatype labels and channels and create names for
    -- them in the maps (otherwise renameVarLHS will fail).
    mapM_ insertLabels ds
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
        -- | Register all modules in the list of instances. This renames all
        -- modules (but not instances), and does so recursively if the module
        -- is not required to be instantiated.
        --
        -- By default all modules are bound with fully qualified names, but can
        -- have unqualLevel prefixes removed.
        insertModule :: PDecl -> RenamerMonad [UnRenamedName]
        insertModule ad = case unAnnotate ad of
            Module (UnQual nm) args privDs pubDs -> do
                -- Compute the module's actual name
                n <- nameMaker (loc ad) (UnQual nm)
                setModuleName (UnQual nm) n Public
                addModule n ad
                -- Add submodules recursively
                (ns, []) <- prefixNamesFromScope (args /= []) nm $ addModuleContext nm $ do
                    ns1 <- concatMapM insertModule privDs
                    ns2 <- concatMapM insertModule pubDs
                    addBoundModuleModules n ns1 ns2
                    let ns = ns1 ++ ns2
                    setNameVisibilityInModule n ns []
                    hideBoundNames ns1 []
                    return $ (ns, [])
                return $ (UnQual nm):ns
            ModuleInstance (UnQual nm) nt args _ _ -> do
                mntarget <- lookupName nt True
                when (isNothing mntarget) $ do
                    msg <- varNotInScopeMessage nt True
                    throwSourceError [mkErrorMessage (loc ad) msg]
                let Just mtarget = mntarget
                -- Find the module
                minfo <- informationForModule (loc ad) nt
                -- Compute the module's actual name
                n <- nameMaker (loc ad) (UnQual nm)
                setModuleName (UnQual nm) n Public
                addModuleInstance n mtarget
                -- Add any submodules
                (ns, []) <- prefixNamesFromScope False nm $ addModuleContext nm $ do
                    let makeModule rn = do
                            n <- nameMaker (loc ad) rn
                            vis <- nameVisibilityInModule nt rn
                            setModuleName rn n vis
                            Just nt <- lookupMaybeHiddenName (joinName nt rn) True
                            addModuleInstance n nt
                            return rn
                    ns1 <- mapM makeModule (privateBoundModules minfo)
                    ns2 <- mapM makeModule (publicBoundModules minfo)
                    hideBoundNames ns1 []
                    return $ (ns1++ns2, [])
                return $ (UnQual nm):ns
            _ -> return []

        nameMaker :: NameMaker
        nameMaker = if topLevel then externalNameMaker False else internalNameMaker

        ignoringNameMaker :: NameMaker
        ignoringNameMaker _ rn = do
            Just v <- lookupName rn False
            return v

        -- | Inserts channels and datatypes that are present in the top-level
        -- namespace. Returns the set of names that were bound.
        insertLabels :: PDecl -> RenamerMonad [UnRenamedName]
        insertLabels ad = case unAnnotate ad of
            Channel ns _ _ -> mapM (\ rn -> do
                n' <- externalNameMaker True (loc ad) rn
                setName rn n'
                return rn) ns
            DataType rn cs -> mapM (\ cl -> case unAnnotate cl of
                    DataTypeClause rn _ _ -> do
                        n' <- externalNameMaker True (loc cl) rn
                        setName rn n'
                        return rn
                ) cs
            Module (UnQual mn) args privDs pubDs -> do
                Just n <- lookupName (UnQual mn) True
                (ns, []) <- prefixNamesFromScope (args /= []) mn $ addModuleContext mn $ do
                    privLabels <- concatMapM insertLabels privDs
                    pubLabels <- concatMapM insertLabels pubDs
                    addBoundModuleLabels n privLabels pubLabels
                    let ns = privLabels ++ pubLabels
                    setNameVisibilityInModule n ns []
                    hideBoundNames privLabels []
                    return (ns, [])
                return ns
            TimedSection _ _ ds -> concatMapM insertLabels ds
            ModuleInstance (UnQual mn) nt args _ _ -> do
                Just n <- lookupName (UnQual mn) True
                minfo <- informationForModule (loc ad) nt
                (ns, []) <- prefixNamesFromScope False mn $ addModuleContext mn $ do
                    let makeLabel rn = do
                            n' <- externalNameMaker True (loc ad) rn
                            vis <- nameVisibilityInModule nt rn
                            setNameWithVisibility rn n' vis
                            return rn
                    privLabels <- mapM makeLabel (privateBoundLabels minfo)
                    pubLabels <- mapM makeLabel (publicBoundLabels minfo)
                    hideBoundNames privLabels []
                    return $! (privLabels ++ pubLabels, [])
                return ns
            _ -> return []

        insertBoundNames :: NameMaker -> PDecl ->
            RenamerMonad ([UnRenamedName], [UnRenamedName])
        insertBoundNames nameMaker pd = case unAnnotate pd of
            Assert _ -> return ([], [])
            Channel _ _ _ -> return ([], [])
            DataType rn _ -> do
                n <- nameMaker (loc pd) rn
                setName rn n
                setType rn (STDatatype n)
                return ([rn], [rn])
            SubType rn _ -> do
                n <- nameMaker (loc pd) rn
                setName rn n
                return ([rn], [])
            External ns -> concatMapM' (\ rn@(UnQual ocn) -> do
                case externalFunctionForOccName ocn of
                    Just b -> do
                        setName rn (name b)
                        return ([rn], [])
                    Nothing -> do
                        addErrors [mkErrorMessage (loc pd) (externalFunctionNotRecognised rn)]
                        return ([], [])) ns
            FunBind rn ms _ -> do
                n <- nameMaker (loc pd) rn
                setName rn n
                return ([rn], [])
            NameType rn _ _ -> do
                n <- nameMaker (loc pd) rn
                setName rn n
                return ([rn], [])
            PatBind p e _ -> do
                renamePattern nameMaker p
                vs <- freeVars p
                return (vs, [])
            Transparent ns ->
                concatMapM' (\ rn@(UnQual ocn) -> do
                    case transparentFunctionForOccName ocn of
                        Just b -> do
                            setName rn (name b)
                            return ([rn], [])
                        Nothing -> do
                            addErrors [mkErrorMessage (loc pd) (transparentFunctionNotRecognised rn)]
                            return ([], [])) ns
            Module (UnQual mn) args privDs pubDs -> do
                Just n <- lookupName (UnQual mn) True
                prefixNamesFromScope (args /= []) mn $ addModuleContext mn $ do
                    env <- gets environment
                    checkDuplicates' $ map FreeVarElement args
                        ++ map FreeVarElement (pubDs ++ privDs)
                    (privNames, privTypes) <-
                        concatMapM' (insertBoundNames nameMaker) privDs
                    (pubNames, pubTypes) <-
                        concatMapM' (insertBoundNames nameMaker) pubDs
                    addBoundModuleBoundNames n privNames pubNames
                    addBoundModuleTypeNames n privTypes pubTypes
                    let ns = privNames ++ pubNames
                        tns = privTypes ++ pubTypes
                    setNameVisibilityInModule n ns tns
                    hideBoundNames privNames privTypes
                    return (ns, tns)
            ModuleInstance (UnQual mn) nt args nm _ -> do
                Just n <- lookupName (UnQual mn) True
                minfo <- informationForModule (loc pd) nt
                prefixNamesFromScope False mn $ addModuleContext mn $ do
                    let makeName rn = do
                            n' <- nameMaker (loc pd) rn
                            vis <- nameVisibilityInModule nt rn
                            setNameWithVisibility rn n' vis
                            Just oldN <- lookupMaybeHiddenName (joinName nt rn)
                                False
                            return (rn, (oldN, n'))
                    privNameMap <- mapM makeName (privateBoundNames minfo)
                    pubNameMap <- mapM makeName (publicBoundNames minfo)
                    let privNames = map fst privNameMap
                        pubNames = map fst pubNameMap
                        nameMap = map snd $ privNameMap ++ pubNameMap
                        makeType rn = do
                            vis <- typeNameVisibilityInModule nt rn
                            Just (RenamedType rnt _) <-
                                lookupType (joinName nt rn)
                            setTypeWithVisibility rn (substituteType nameMap rnt) vis
                            return rn
                    privTypes <- mapM makeType (privateTypeNames minfo)
                    pubTypes <- mapM makeType (publicTypeNames minfo)
                    hideBoundNames privNames privTypes
                    return $! (privNames ++ pubNames, privTypes ++ pubTypes)
            TimedSection _ _ ds -> do
                (rns, rnts) <- addScope $ do
                    mapM_ (\ n -> setName (UnQual (OccName n)) (builtInName n))
                        timedNames
                    (rns, trns) <- concatMapM' (insertBoundNames nameMaker) ds
                    env <- gets environment
                    let ns = map (\ rn -> (rn, fromJust (HM.maybeLookup env rn))) rns
                    return $! (ns, trns)
                mapM_ (\(rn, n) -> setRenamedName rn n) rns
                return (map fst rns, rnts)
            _ -> return ([], [])

        -- | resetModuleContext must be called in ALL cases, apart from the
        -- module context, otherwise names are incorrectly qualified within
        -- patterns etc.
        renameRightHandSide :: PDecl -> RenamerMonad TCDecl
        renameRightHandSide pd = reAnnotate pd $ case unAnnotate pd of
            Assert e -> resetModuleContext $ do
                e' <- addScope $ rename e
                return $ Assert e'
            Channel rns e ta -> resetModuleContext $ do
                ns <- mapM renameVarRHS rns
                addTypeScope $ addScope $ do
                    e' <- rename e
                    ta' <- rename ta
                    return $ Channel ns e' ta'
            DataType rn cs -> resetModuleContext $ do
                n <- renameVarRHS rn
                cs' <- mapM (\ pc -> addScope $ reAnnotate pc $ case unAnnotate pc of
                                DataTypeClause rn e ta -> addTypeScope $ do
                                    n' <- renameVarRHS rn
                                    e' <- rename e
                                    ta' <- rename ta
                                    return $ DataTypeClause n' e' ta') cs
                return $ DataType n cs'
            SubType rn cs -> resetModuleContext $ do
                n <- renameVarRHS rn
                cs' <- mapM (\ pc -> addScope $ reAnnotate pc $ case unAnnotate pc of
                                DataTypeClause rn e Nothing -> do
                                    n' <- renameVarRHS rn
                                    e' <- rename e
                                    return $ DataTypeClause n' e' Nothing) cs
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
            NameType rn e ta -> resetModuleContext $ do
                n <- renameVarRHS rn
                addTypeScope $ do
                    -- Must be done here in types are in scope in expressions
                    ta' <- rename ta
                    e' <- addScope $ rename e
                    return $ NameType n e' ta'
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
            Module (UnQual mn) args privDs pubDs -> do
                n' <- renameModuleVar (UnQual mn)
                unhideModuleNames mn $ addModuleContext mn $ do
                    -- We also now insert the arguments. We insert the module
                    -- context to ensure they are not qualified.
                    args' <- resetModuleContext $
                        mapM (renamePattern internalNameMaker) args
                    -- Insert the arguments are rename the reset
                    privDs' <- mapM renameRightHandSide privDs
                    pubDs' <- mapM renameRightHandSide pubDs
                    let m = Module n' args' privDs' pubDs'
                    addBoundModuleRenamedVersion n' $ reAnnotatePure pd m
                    return m
            ModuleInstance (UnQual mn) nt args _ _ -> do
                n' <- renameModuleVar (UnQual mn)
                nt' <- renameModuleVar nt
                args' <- resetModuleContext $ mapM (addScope . rename) args
                minfo <- informationForModule (loc pd) nt
                unhideModuleNames mn $ addModuleContext mn $ do
                    nm1 <- mapM (\ rn -> do
                            Just old <- lookupMaybeHiddenName (joinName nt rn) False
                            Just new <- lookupMaybeHiddenName rn False
                            return (old, new)
                        ) (publicBoundNames minfo ++ privateBoundNames minfo ++
                            publicBoundLabels minfo ++ privateBoundLabels minfo)
                    nm2 <- mapM (\ rn -> do
                            Just old <- lookupMaybeHiddenName (joinName nt rn) True
                            Just new <- lookupMaybeHiddenName rn True
                            return (old, new)
                        ) (publicBoundModules minfo ++ privateBoundModules minfo)
                    return $ ModuleInstance n' nt' args'
                        (M.fromList (nm1 ++ nm2)) (renamedDeclaration minfo)
            TimedSection Nothing f ds -> do
                f' <- addScope $ rename f
                tock <- renameVarRHS (UnQual (OccName "tock"))
                addScope $ do
                    mapM_ (\ n -> setName (UnQual (OccName n)) (builtInName n))
                        timedNames
                    ds' <- mapM renameRightHandSide ds
                    return $ TimedSection (Just tock) f' ds'
            PrintStatement s -> return $ PrintStatement s

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
                mn <- lookupName v False
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

data FreeVarElement = forall a b . FreeVars a => FreeVarElement (Annotated b a)

instance FreeVars FreeVarElement where
    freeVars (FreeVarElement a) = freeVars a

newtype ModuleNameCheck = ModuleNameCheck UnRenamedName

instance FreeVars ModuleNameCheck where
    freeVars (ModuleNameCheck n) = return [n]

checkModuleNameDuplicates :: [PDecl] -> RenamerMonad ()
checkModuleNameDuplicates ds =
    let
        extractModuleNames :: PDecl -> [Annotated () ModuleNameCheck]
        extractModuleNames (An loc _ (Module (UnQual mn) _ privDs pubDs)) =
            An loc () (ModuleNameCheck (UnQual mn))
            : map (\ (An a b (ModuleNameCheck n)) ->
                        An a b (ModuleNameCheck (Qual mn n)))
                (concatMap extractModuleNames (privDs ++ pubDs))
        extractModuleNames (An loc _ (ModuleInstance n _ _ _ _ )) =
            [An loc () (ModuleNameCheck n)]
        extractModuleNames (An _ _ (TimedSection _ _ ds)) =
            concatMap extractModuleNames ds
        extractModuleNames (An loc _ (Channel ns _ _)) =
            [An loc () (ModuleNameCheck n) | n <- ns]
        extractModuleNames (An loc _ (DataType n cs)) =
            [An loc () (ModuleNameCheck n') | An loc _ (DataTypeClause n' _ _) <- cs]
        extractModuleNames _ = []
    in checkDuplicates $ concatMap extractModuleNames ds

checkDuplicates :: FreeVars a => [Annotated b a] -> RenamerMonad ()
checkDuplicates aps = checkDuplicates' (map FreeVarElement aps)

checkDuplicates' :: [FreeVarElement] -> RenamerMonad ()
checkDuplicates' aps = do
    fvss <- mapM freeVars aps
    let
        fvs = sort (concat fvss)
        gfvs = group fvs
        duped = filter (\l -> length l > 1) gfvs
        nameLocMap =
            concatMap (\(ns, FreeVarElement d) -> [(n, loc d) | n <- ns]) 
                (zip fvss aps)

    loc <- gets srcSpan
    if duped /= [] then
        throwSourceError (map (mkErrorMessage loc)
            (duplicatedDefinitionsMessage nameLocMap))
    else return ()

-- | Rename a variable on the right hand side of a definition.
-- 
-- If the variable does not exist it returns an error thunk and adds an error
-- that will be raised when the monad is evaluated.
renameVarRHS :: UnRenamedName -> RenamerMonad Name
renameVarRHS n = do
    mn <- lookupName n False
    case mn of
        Just x -> return x
        Nothing -> do
            msg <- varNotInScopeMessage n False
            loc <- gets srcSpan
            addErrors [mkErrorMessage loc msg]
            return $ panic "error name evaluated"

renameModuleVar :: UnRenamedName -> RenamerMonad Name
renameModuleVar n = do
    mn <- lookupName n True
    case mn of
        Just x -> return x
        Nothing -> do
            msg <- varNotInScopeMessage n True
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
    rename (PropertyCheck e1 p m mopts) =
        return PropertyCheck $$ rename e1 $$ rename p $$ return m $$ rename mopts
        
instance Renamable (SemanticProperty UnRenamedName) (SemanticProperty Name) where
    rename (HasTrace e) = return HasTrace $$ rename e
    rename DeadlockFreedom = return DeadlockFreedom
    rename LivelockFreedom = return LivelockFreedom
    rename Deterministic = return Deterministic

instance Renamable (ModelOption UnRenamedName) (ModelOption Name) where
    rename (TauPriority e) = return TauPriority $$ rename e
    rename (PartialOrderReduce m) = return $ PartialOrderReduce m

instance Renamable (Exp UnRenamedName) (Exp Name) where
    rename (App e es) = return App $$ rename e $$ rename es
    rename (BooleanBinaryOp op e1 e2) = return 
        (BooleanBinaryOp op) $$ rename e1 $$ rename e2
    rename (BooleanUnaryOp op e) = return
        (BooleanUnaryOp op) $$ rename e
    rename (Concat e1 e2) = return Concat $$ rename e1 $$ rename e2
    rename (DotApp e1 e2) = return DotApp $$ rename e1 $$ rename e2
    rename (If e1 e2 e3) = return If $$ rename e1 $$ rename e2 $$ rename e3
    rename (Lambda ps e) = do
        (ps', e') <- addScope (do
                ps' <- mapM (renamePattern internalNameMaker) ps
                e' <- rename e
                return (ps', e'))
        return $ Lambda ps' e'
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
    rename (Map kvs) = return Map $$ rename kvs
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
    rename (Project e1 e2) = return Project $$ rename e1 $$ rename e2
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
        mapM_ ( \ (An loc _ (c@(STypeConstraint _ n))) -> do
                n' <- renameTypeVar n
                case n' of
                    Just (STVar n') | not (n' `elem` ns) -> 
                        let msg = invalidTypeConstraintLocation c n'
                        in addErrors [mkErrorMessage loc msg]
                    _ -> return ()
            ) cs
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
    rename (STMap t1 t2) = return STMap $$ rename t1 $$ rename t2
    rename (STTuple ts) = return STTuple $$ rename ts
    rename (STFunction args rt) = return STFunction $$ rename args $$ rename rt
    rename (STDotable t1 t2) = return STDotable $$ rename t1 $$ rename t2
    rename (STParen t) = return STParen $$ rename t

substituteType :: [(Name, Name)] -> SType Name -> SType Name
substituteType nm t =
    let
        m = M.fromList nm
        sub :: TCSType -> TCSType
        sub ad = reAnnotatePure ad (sub' (unAnnotate ad))
        sub' :: SType Name -> SType Name
        sub' (STVar n) = STVar (M.findWithDefault n n m)
        sub' (STExtendable t n) = STExtendable (sub t) (M.findWithDefault n n m)
        sub' (STSet t) = STSet (sub t)
        sub' (STSeq t) = STSeq (sub t)
        sub' (STDot t1 t2) = STDot (sub t1) (sub t2)
        sub' (STMap t1 t2) = STMap (sub t1) (sub t2)
        sub' (STTuple ts) = STTuple (map sub ts)
        sub' (STFunction args rt) = STFunction (map sub args) (sub rt)
        sub' (STDotable t1 t2) = STDotable (sub t1) (sub t2)
        sub' (STParen t) = STParen (sub t)
        sub' (STDatatype n) = STDatatype (M.findWithDefault n n m)
    in sub' t

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
        Just x | renamedTypeVisibility x == Public -> return $ Just $ renamedType x
        _ -> do
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
        mn <- lookupName n False
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
    freeVars (Channel ns _ _) = return ns
    freeVars (DataType n cs) =
        return $ n : [n' | DataTypeClause n' _ _ <- map unAnnotate cs]
    freeVars (SubType n _) = return [n]
    freeVars (FunBind n _ _) = return [n]
    freeVars (NameType n _ _) = return [n]
    freeVars (PatBind p _ _) = freeVars p
    freeVars (Transparent ns) = return ns
    freeVars (Module (UnQual n) _ _ expDs) = do
        ns <- freeVars expDs
        return $ UnQual n : map (Qual n) ns
    freeVars (TimedSection _ _ ds) = freeVars ds
    freeVars (ModuleInstance (UnQual n) nt _ _ _) = do
        return $ [UnQual n]
    freeVars (PrintStatement _) = return []

freeTypeVars :: PSType -> RenamerMonad [(UnRenamedName, SrcSpan)]
freeTypeVars st =
    let
        freeTypeVarsL = concatMapM freeTypeVars
        freeVars (STVar (Qual _ _)) = return []
        freeVars (STVar n) = do
            mt <- lookupType n
            return $! case mt of
                Just x | renamedTypeVisibility x == Public -> []
                Nothing -> [(n, loc st)]
        freeVars (STExtendable t (Qual _ _)) = freeTypeVars t
        freeVars (STExtendable t n) = do
            ns <- freeTypeVars t
            mt <- lookupType n
            return $! case mt of
                Just x | renamedTypeVisibility x == Public -> ns
                Nothing -> (n, loc st):ns
        freeVars (STSet t) = freeTypeVars t
        freeVars (STSeq t) = freeTypeVars t
        freeVars (STDot t1 t2) = freeTypeVarsL [t1, t2]
        freeVars (STMap t1 t2) = freeTypeVarsL [t1, t2]
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
        locationsOf n = [loc | (n', loc) <- ns, n == n']
    in [(n, locationsOf n) | n <- dupNames]

duplicatedDefinitionsMessage' :: [(UnRenamedName, [SrcSpan])] -> [Error]
duplicatedDefinitionsMessage' nlocs = 
    map (\ (n, spans) ->
        hang (text "The variable" <+> prettyPrint n 
                <+> text "has multiple definitions at" <> colon) tabWidth
            (vcat (map prettyPrint (sort spans)))) nlocs

transparentFunctionNotRecognised :: UnRenamedName -> Error
transparentFunctionNotRecognised n =
    text "The transparent function" <+> prettyPrint n <+> 
    text "is not recognised."

externalFunctionNotRecognised :: UnRenamedName -> Error
externalFunctionNotRecognised n = 
    text "The external function" <+> prettyPrint n <+> 
    text "is not recognised."

varNotInScopeMessage :: UnRenamedName -> Bool -> RenamerMonad Error
varNotInScopeMessage n isModule = do
    mv <- lookupMaybeHiddenName n isModule
    if isJust mv then do
        vis <- currentVisibilityOfName n
        let n = fromJust mv
        return $
            (if isModule then text "The module " else empty) <>
            prettyPrint n <+> text "is not in scope"
            P.$$ tabIndent (
                text "Defined"
                <+> case vis of
                        Public -> panic "Cannot not be in scope"
                        Private -> text "privately"
                        Instance -> text "inside a parameterised module"
                <+> case nameDefinition n of
                        Unknown -> empty
                        l -> parens (text "at" <+> prettyPrint l)
                P.$$ case vis of
                        Public -> panic "Cannot not be in scope"
                        Private -> text "Hint: move to the exports section"
                        Instance -> text "Hint: create a module instance")
    else do
    mInverseModuleName <- lookupName' n (not isModule) False
    env <- gets environment
    let availablePp =
            [(show $ prettyPrint rn, (rn, n))
                | (rn, RenamedName n b Public) <- HM.flatten env, b == isModule]
        suggestions = fuzzyLookup (show $ prettyPrint n) availablePp

        availableTransExtern = if isModule then [] else
            [(B.unpack $ stringName b, b) | b <- builtins True,
                isTransparent b || isExternal b]
        builtinSuggestions = fuzzyLookup (show $ prettyPrint n) availableTransExtern
    return $
        (if isModule then text "The module " else empty) <>
        (prettyPrint n <+> text "is not in scope")
        P.$$ (
            case mInverseModuleName of
                Just _ ->
                    if isModule then
                        text "because the in-scope name is not a module, but a module was requested."
                    else
                        text "because the in-scope name is a module, but a normal name was requested."
                Nothing -> empty
        )
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
    let mv = HM.maybeLookup env n
    if isJust mv then do
        let Just rnt = mv
            vis = renamedTypeVisibility rnt
            t = renamedType rnt
        return $
            (text "The type-variable" <+> prettyPrint n
                <+> text "is not in scope")
            P.$$ tabIndent (
                text "Defined"
                <+> case vis of
                        Public -> panic "Cannot not be in scope"
                        Private -> text "privately"
                        Instance -> text "inside a parameterised module"
                <+> case t of
                        STVar n | nameDefinition n /= Unknown ->
                            parens (text "at" <+> prettyPrint (nameDefinition n))
                        _ -> empty
                P.$$ case vis of
                        Public -> panic "Cannot not be in scope"
                        Private -> text "Hint: move to the exports section"
                        Instance -> text "Hint: create a module instance")
    else do
    let availablePp = [(show $ prettyPrint rn, (rn, n)) |
                        (rn, RenamedType n Public) <- HM.flatten env]
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
        text "cannot be constrainted using a type constraint"
    P.$$ text "as only type variables may be constrained."

invalidTypeConstraintLocation :: PrettyPrintable id => STypeConstraint id ->
    Name -> Error
invalidTypeConstraintLocation c typeVar =
    text "The type constraint" <+> prettyPrint c <+>
        text "cannot be specified at this location, since"
    P.$$ text "the type-variable it refers to, i.e." <+> prettyPrint typeVar <>
        text ", was bound in the type annotation at:"
    P.$$ tabIndent (prettyPrint (nameDefinition typeVar))
