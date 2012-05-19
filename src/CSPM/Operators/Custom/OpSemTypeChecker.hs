{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module CSPM.Operators.Custom.OpSemTypeChecker (typeCheckOperators, varsBound) where

import Data.IORef
import Control.Monad.Error
import Control.Monad.State
import CSPM.DataStructures.Names
import CSPM.Renamer hiding (FreeVars, freeVars)
import qualified CSPM.Renamer as RN
import CSPM.Operators.Custom.OpSemPrettyPrinter
import CSPM.Operators.Custom.OpSemDataStructures
import Data.List
import qualified Util.Annotated as An
import Util.Exception
import Util.List
import Util.Monad
import Util.PrettyPrint hiding (($$))
import Util.PartialFunctions
import Text.PrettyPrint.HughesPJ hiding (($$))

identityRule = 
    -- We specify identity's argument as not recursive since it is special
    InputOperator (UnQual (OccName "Identity")) [(UnQual (OccName "P"), NotRecursive)] [
        InductiveRule
            [Performs (Var (UnQual (OccName "P"))) (Event (UnQual (OccName "a"))) (Var (UnQual (OccName "P'")))]
            (Performs
                (OperatorApp (UnQual (OccName "Identity")) [Var (UnQual (OccName "P"))])
                (Event (UnQual (OccName "a")))
                (OperatorApp (UnQual (OccName "Identity")) [Var (UnQual (OccName "P'"))]))
            [SCGenerator (PVar (UnQual (OccName "a"))) SigmaPrime]
    ] Nothing Nothing

data TypeCheckError =
    ErrorWithOperator Name TypeCheckError
    | ErrorWithRule (InductiveRule Name) TypeCheckError
    | ErrorWithExpression (Exp Name) TypeCheckError
    
    -- ** Specific Errors (i.e. errors that indicate their cause)
    -- Name appears multiple times in some context
    | DuplicatedNameError [Name]
    -- e.g. P -> P 
    | ArgumentsUsedOnRHSOfProcessRelation [Name]
    | OnProcessClonedError [Name]
    -- E.g. P -tau-> P' => op(P) -a-> op(P')
    | TauPromotedError
    | ResultingProcessDiscarded [Name]
        
    -- ** General Errors
    -- Error whilst unifying the two types
    | UnificationError Type Type
    -- Name is an unknown variable
    | VariableNotInScope Name
    | InfiniteUnificationError TypeVar Type
    
    | UnknownError String

prettyPrintType = show

showList' :: Show a => [a] -> String
showList' xs = show $ hsep (punctuate comma (map text (map show xs)))

indentEveryLine = unlines . map ("    "++) . lines

instance Show TypeCheckError where
    show (ErrorWithOperator n err) =
        show err++
        "in the operator "++show n++"."
    show (ErrorWithRule rule err) = 
        show err++
        "in the rule:\n"++
        indentEveryLine (show (prettyPrint rule))
    show (ErrorWithExpression exp err) = 
        show err++
        "in the expression:\n"++
        indentEveryLine (show (prettyPrint exp))
    show (DuplicatedNameError ns) = 
        "The names "++showList' ns++" are duplicated.\n"
    show (ArgumentsUsedOnRHSOfProcessRelation ns) =
        "The arguments "++showList' ns++" are used on the right hand side of"++
        "a process relation.\n"
    show (OnProcessClonedError ns) = 
        "The on processes "++showList' ns++" are cloned.\n"
    show (TauPromotedError) = 
        "A tau was promoted to a non-tau event.\n"
    show (ResultingProcessDiscarded ns) = 
        "The processes resulting from "++showList' ns++" are discarded.\n"
    show (UnificationError t1 t2) =
        "Could not match the types:\n"++
        "\t"++show (prettyPrintType t1)++
        "\nand\n"++
        "\t"++show (prettyPrintType t2)++"\n"       
    show (VariableNotInScope n) = "Name "++show n++" is not in scope.\n"
    show (InfiniteUnificationError tv t) =
        "Cannot construct the infinite type: "++
        show tv++" = "++show (prettyPrintType t)++"\n"
    show (UnknownError s) = 
        "An unknown error occured: "++s++"\n"
    
instance Error TypeCheckError where
    strMsg = UnknownError
    
addOperatorToError :: Name -> TypeCheckMonad a -> TypeCheckMonad a
addOperatorToError n m = 
    m `catchError` (\ e -> throwError $ ErrorWithOperator n e)

addRuleToError :: InductiveRule Name -> TypeCheckMonad a -> TypeCheckMonad a
addRuleToError n m = 
    m `catchError` (\ e -> throwError $ ErrorWithRule n e)

addExpToError :: Exp Name -> TypeCheckMonad a -> TypeCheckMonad a
addExpToError exp m = 
    m `catchError` (\ e -> throwError $ ErrorWithExpression exp e)

-- ***************************************************************************
-- Type Check Monad
-- ***************************************************************************
data TypeInferenceState = TypeInferenceState {
        -- map from names to types
        environment :: [PartialFunction Name Type],
        -- Next TypeVar to be allocated
        nextTypeId :: Int,
        -- If true then 
        -- unify (TOnProcess) (TOffProcess) = TOnProcess
        -- NB: this is not symmetric and is used only in operator application
        allowGeneralUnification :: Bool
    }   
    deriving Show
    
type TypeCheckMonad = 
    ErrorT TypeCheckError (StateT TypeInferenceState IO)

errorIfFalse :: Bool -> TypeCheckError -> TypeCheckMonad ()
errorIfFalse True e = return ()
errorIfFalse False e = throwError e

errorIfFalseM :: TypeCheckMonad Bool -> TypeCheckError -> TypeCheckMonad ()
errorIfFalseM m e = 
    do
        res <- m
        errorIfFalse res e

unifyTypeWithName :: Name -> Type -> TypeCheckMonad ()
unifyTypeWithName n t =
    do
        typ <- getType n
        unify t typ
        return ()
        

readTypeRef :: TypeVarRef -> TypeCheckMonad (Either TypeVar Type)
readTypeRef (TypeVarRef tv ioref) = 
    do
        mtyp <- liftIO $ readIORef ioref
        case mtyp of
            Just t  -> return (Right t)
            Nothing -> return (Left tv)

writeTypeRef :: TypeVarRef -> Type -> TypeCheckMonad ()
writeTypeRef (TypeVarRef tv ioref) t = liftIO $ writeIORef ioref (Just t)

freshTypeVar :: TypeCheckMonad Type
freshTypeVar = 
    do
        nextId <- gets nextTypeId
        modify (\s -> s { nextTypeId = nextId+1 })
        ioRef <- liftIO $ newIORef Nothing
        return $ TVar (TypeVarRef (TypeVar nextId) ioRef)


safeGetType_ :: [PartialFunction Name Type] -> Name -> Maybe Type
safeGetType_ [] n = Nothing
safeGetType_ (pf:pfs) n = 
    case safeApply pf n of
        Just t  -> Just t
        Nothing -> safeGetType_ pfs n

getType :: Name -> TypeCheckMonad Type
getType name =
    do
        envs <- gets environment
        case safeGetType_ envs name of
            Just t  -> return t
            Nothing -> throwError $ VariableNotInScope name

safeGetType :: Name -> TypeCheckMonad (Maybe Type)
safeGetType n =
    do
        envs <- gets environment
        return $ safeGetType_ envs n

-- Sets the type of n to be t in the current scope only. No unification is 
-- performed.
setType :: Name -> Type -> TypeCheckMonad ()
setType n t =
    do
        res <- safeGetType n
        (env:envs) <- gets environment
        let env' = updatePF env n t
        modify (\ s -> s { environment = env':envs })

local :: [Name] -> TypeCheckMonad a -> TypeCheckMonad a
local ns m = 
    do
        env <- gets environment
        nextId <- gets nextTypeId
        newArgs <- replicateM (length ns) freshTypeVar
        modify (\s -> s { environment = (zip ns newArgs):env })
        
        res <- m
        
        env <- gets environment
        modify (\ s -> s { environment = tail env })
        
        return res

generalUnificationAllowed :: TypeCheckMonad a -> TypeCheckMonad a
generalUnificationAllowed m =
    do
        r <- gets allowGeneralUnification
        modify (\ s -> s { allowGeneralUnification = True})
        res <- m
        modify (\ s -> s { allowGeneralUnification = r})
        return res

compress :: Type -> TypeCheckMonad Type
compress (tr @ (TVar typeRef)) = 
    do
        res <- readTypeRef typeRef
        case res of
            Left tv -> return tr
            Right t -> compress t
compress (TSet t) =
    do
        t' <- compress t
        return $ TSet t'
compress (TTuple ts) =
    do
        ts' <- mapM compress ts
        return $ TTuple ts'
compress (TChannel ts) = 
    do
        ts' <- mapM compress ts
        return $ TChannel ts'
compress (TOperator ts) =
    do
        ts' <- mapM compress ts
        return $ TOperator ts'  
compress t = return t

-- ***************************************************************************
-- Dependency Analyisis
-- ***************************************************************************
class VarsBound a id where
    -- There should never be any duplicates
    varsBound :: a -> [id]
instance VarsBound a id => VarsBound [a] id where
    varsBound = concatMap varsBound
instance VarsBound (Pat id) id where
    varsBound (PVar n) = [n]
    varsBound (PTuple ps) = varsBound ps
    varsBound (PSet ps) = varsBound ps
instance VarsBound (SideCondition id) id where
    varsBound (SCGenerator p e) = varsBound p
    varsBound (Formula p) = []

class Eq id => FreeVars a id where
    freeVars :: a -> [id]
    freeVars = nub . freeVars'
    freeVars' :: a -> [id]
instance (Eq id, FreeVars a id) => FreeVars [a] id where
    freeVars' = concatMap freeVars
instance Eq id => FreeVars (Exp id) id where
    freeVars' (Tuple es) = freeVars' es
    freeVars' (Var n) = [n]
    freeVars' (Sigma) = []
    freeVars' (SigmaPrime) = []
    freeVars' (Powerset e) = freeVars' e
    freeVars' (SetMinus e1 e2) = freeVars' e1 ++ freeVars' e2
    freeVars' (Set es) = freeVars' es
    freeVars' (Union e1 e2) = freeVars' e1 ++ freeVars' e2
    freeVars' (Intersection e1 e2) = freeVars' e1 ++ freeVars' e2
    freeVars' (OperatorApp n es) = freeVars' es
    freeVars' (InductiveCase) = []
    freeVars' (ReplicatedUnion e) = freeVars' e
    freeVars' (SetComprehension es scs) = 
        (freeVars' es++freeVars' scs) \\ varsBound scs
instance Eq id => FreeVars (SideCondition id) id where
    freeVars' (SCGenerator p e) = freeVars' e
    freeVars' (Formula p) = freeVars p
instance Eq id => FreeVars (Formula id) id where
    freeVars' (Subset e1 e2) = freeVars' [e1,e2]
    freeVars' (Member p e) = freeVars' e++freeVars' p
    freeVars' (Equals e1 e2) = freeVars' [e1,e2]
    freeVars' (And e1 e2) = freeVars' [e1,e2]
    freeVars' (Or e1 e2) = freeVars' [e1,e2]
    freeVars' (Not e1) = freeVars' e1
    freeVars' PFalse = []
    freeVars' PTrue = []
instance Eq id => FreeVars (Pat id) id where
    freeVars' (PVar n) = [n]
    freeVars' (PTuple ns) = freeVars' ns
    freeVars' (PSet ps) = freeVars' ps
instance Eq id => FreeVars (Event id) id where
    freeVars' (Tau) = []
    freeVars' (Event n) = [n]
    freeVars' (ChanEvent n es) = n:(concatMap freeVars' es)

class TypeCheckable a b | a -> b where
    errorConstructor :: a -> (TypeCheckError -> TypeCheckError)
    typeCheck :: a -> TypeCheckMonad b
    typeCheck e =
        typeCheck' e `catchError` (throwError . errorConstructor e)
    typeCheck' :: a -> TypeCheckMonad b
    
instance TypeCheckable (SideCondition Name) () where
    errorConstructor x = id
    typeCheck' (SCGenerator p generator) =
        do
            tgen <- typeCheck generator
            tpat <- typeCheck p
            unify tgen (TSet tpat)
            return ()
    typeCheck' (Formula f) = typeCheck f
instance TypeCheckable (Formula Name) () where
    errorConstructor x = id
    typeCheck' (Subset e1 e2) =
        do
            t1 <- typeCheck e1
            t2 <- typeCheck e2
            ensureIsSet t1
            ensureIsSet t2
            unify t1 t2
            return ()
    typeCheck' (Member p e) =
        do
            t1 <- typeCheck p
            t2 <- typeCheck e
            unify (TSet t1) t2
            return ()
    typeCheck' (Equals e1 e2) =
        do
            t1 <- typeCheck e1
            t2 <- typeCheck e2
            unify t1 t2
            return ()
    typeCheck' (Not sc) = typeCheck sc
    typeCheck' (And sc1 sc2) = typeCheck sc1 >> typeCheck sc2
    typeCheck' (Or sc1 sc2) = typeCheck sc1 >> typeCheck sc2
    typeCheck' PTrue = return ()
    typeCheck' PFalse = return ()
instance TypeCheckable (Exp Name) Type where
    errorConstructor = ErrorWithExpression
    typeCheck' Sigma = return $ TSet TEvent
    typeCheck' SigmaPrime = return $ TSet TEvent
    typeCheck' (Powerset e) = 
        do
            t <- typeCheck e
            ensureIsSet t
            return $ TSet t
    typeCheck' (Tuple es) =
        do
            ts <- mapM typeCheck es
            return $ TTuple ts
    typeCheck' (Var n) = getType n
    typeCheck' (Set es) =
        do
            ts <- mapM typeCheck es
            t <- unifyAll ts
            return $ TSet t
    typeCheck' (SetMinus s1 s2) =
        do
            t1 <- typeCheck s1
            t2 <- typeCheck s2
            ensureIsSet t1
            ensureIsSet t2
            unify t1 t2
    typeCheck' (Union s1 s2) =
        do
            t1 <- typeCheck s1
            t2 <- typeCheck s2
            ensureIsSet t1
            ensureIsSet t2
            unify t1 t2
    typeCheck' (Intersection s1 s2) =
        do
            t1 <- typeCheck s1
            t2 <- typeCheck s2
            ensureIsSet t1
            ensureIsSet t2
            unify t1 t2
    typeCheck' (InductiveCase) = 
        -- We return TOnProcess here - this is fine since we assume everything
        -- in the inductive case is On.
        return $ TOffProcess Unknown
    typeCheck' (SetComprehension es scs) =
        do
            let varsToBind = varsBound scs
            errorIfFalse (noDups varsToBind) (DuplicatedNameError varsToBind)
            local varsToBind (
                do
                    mapM typeCheck scs
                    ts <- mapM typeCheck es
                    t <- unifyAll ts
                    return $ TSet t)
    typeCheck' (ReplicatedUnion e) =
        do
            t <- typeCheck e
            fv <- freshTypeVar
            unify t (TSet (TSet fv))
            return $ TSet fv
    typeCheck' (OperatorApp n es) =
        do
            -- TODO: remove compression since we don't use it?
            ts <- mapM typeCheck es
            opType <- getType n
            
            -- IMPORTANT: order is important here as the unification may not
            -- be symmetric (see allowGeneralUnification)
            t <- unify opType (TOperator ts)
            
            return $ TOnProcess Unknown

instance TypeCheckable (Pat Name) Type where
    errorConstructor x = id
    typeCheck' (PVar n) = getType n
    typeCheck' (PTuple ps) =
        do
            ts <- mapM typeCheck ps
            return $ TTuple ts
    typeCheck' (PSet ps) =
        do
            -- TODO: check set is <= 1
            ts <- mapM typeCheck ps
            t <- unifyAll ts
            return $ TSet t

instance Renamable (InputOperator UnRenamedName) (InputOperator Name) where
    rename (InputOperator name args rules repOp parseInfo) = do
        let renameProcPair (n, p) = do
                n' <- internalNameMaker An.Unknown n
                setName n n'
                return (n', p)
        return InputOperator $$ renameVarRHS name $$ mapM renameProcPair args 
            $$ rename rules $$ rename repOp $$ return parseInfo

instance Renamable (InputReplicatedOperator UnRenamedName) (InputReplicatedOperator Name) where
    rename (InputReplicatedOperator name base inductive parseInfo) = do
        let renameBase (ps, e) = addScope $ do
                ps' <- mapM renameOpSemPattern ps
                e' <- rename e
                return (ps', e')
            renameInductive (ns, e) = addScope $ do
                pns' <- mapM (renameOpSemPattern . PVar) ns
                e' <- rename e
                return (map (\ (PVar n) -> n) pns', e')
        return InputReplicatedOperator $$ mapM renameVarRHS name 
            $$ renameBase base $$ renameInductive inductive $$ return parseInfo

instance Renamable (InductiveRule UnRenamedName) (InductiveRule Name) where
    rename (InductiveRule rs rs' ss) = do
        let renamePres :: [ProcessRelation UnRenamedName] -> RenamerMonad a ->
                RenamerMonad ([ProcessRelation Name], a)
            renamePres [] prog = do
                v <- prog
                return ([], v)
            renamePres (Performs (Var n) ev (Var n') : prs) prog = do
                n <- renameVarRHS n
                ev <- rename ev
                n'' <- internalNameMaker An.Unknown n'
                setName n' n''
                (rest, a) <- renamePres prs prog
                return $ (Performs (Var n) ev (Var n'') : rest, a)
        (scs', (rs, rs')) <- renameOpSemStatements ss $ addScope $ renamePres rs (rename rs')
        return $ InductiveRule rs rs' scs'

instance Renamable (ProcessRelation UnRenamedName) (ProcessRelation Name) where
    rename (Performs e1 ev e2) =
        return Performs $$ rename e1 $$ rename ev $$ rename e2

instance Renamable (Event UnRenamedName) (Event Name) where
    rename (Event n) = return Event $$ renameVarRHS n
    rename (ChanEvent n es) = return ChanEvent $$ renameVarRHS n $$ rename es
    rename Tau = return Tau

instance Renamable (Formula UnRenamedName) (Formula Name) where
    rename (Member e1 e2) = return Member $$ rename e1 $$ rename e2
    rename (Equals e1 e2) = return Equals $$ rename e1 $$ rename e2
    rename (Subset e1 e2) = return Subset $$ rename e1 $$ rename e2
    rename (Not f) = return Not $$ rename f
    rename (And f1 f2) = return And $$ rename f1 $$ rename f2
    rename (Or f1 f2) = return Or $$ rename f1 $$ rename f2
    rename PFalse = return PFalse
    rename PTrue = return PTrue

instance Renamable (Exp UnRenamedName) (Exp Name) where
    rename (OperatorApp n es) = return OperatorApp $$ renameVarRHS n $$ rename es
    rename InductiveCase = return InductiveCase
    rename (Tuple es) = return Tuple $$ rename es
    rename (Var n) = return Var $$ renameVarRHS n
    rename SigmaPrime = return SigmaPrime
    rename Sigma = return Sigma
    rename (SetComprehension es cs) = do
        (stmts', cs') <- renameOpSemStatements cs (rename es)
        return $ SetComprehension cs' stmts'
    rename (Set es) = return Set $$ rename es
    rename (SetMinus e1 e2) = return SetMinus $$ rename e1 $$ rename e2
    rename (Union e1 e2) = return Union $$ rename e1 $$ rename e2
    rename (Intersection e1 e2) = return Intersection $$ rename e1 $$ rename e2
    rename (Powerset e1) = return Powerset $$ rename e1
    rename (ReplicatedUnion e) = return ReplicatedUnion $$ rename e

instance Renamable (Channel UnRenamedName) (Channel Name) where
    rename (Channel n fs) = return Channel $$ renameVarRHS n $$ rename fs

instance RN.FreeVars (Pat UnRenamedName) where
    freeVars (PTuple ps) = RN.freeVars ps
    freeVars (PSet ps) = RN.freeVars ps
    freeVars (PVar n) = do
        mn <- lookupName n
        case mn of
            Just n | isNameDataConstructor n -> return []
            _ -> return [n]
instance RN.FreeVars (SideCondition UnRenamedName) where
    freeVars (SCGenerator p e) = RN.freeVars p
    freeVars _ = return []

renameOpSemPattern :: Pat UnRenamedName -> RenamerMonad (Pat Name)
renameOpSemPattern pn = do
    let
        renamePat :: Pat UnRenamedName -> RenamerMonad (Pat Name)
        renamePat (PTuple ps) = return PTuple $$ mapM renamePat ps
        renamePat (PSet ps) = return PSet $$ mapM renamePat ps
        renamePat (PVar v) = do
            -- In this case, we should lookup the variable in the map and see if it is
            -- bound. If it is and is a datatype constructor, then we should return
            -- return that, otherwise we create a new internal var.
            mn <- lookupName v
            case mn of
                Just n | isNameDataConstructor n ->
                    -- Just return the name
                    return $ PVar n
                _ -> do
                    -- Create the name
                    n <- internalNameMaker An.Unknown v
                    setName v n
                    return $ PVar n
    checkDuplicates' [pn]
    renamePat pn

checkDuplicates' :: (Show a, RN.FreeVars a) => [a] -> RenamerMonad ()
checkDuplicates' xs = do
    vss <- mapM RN.freeVars xs
    when (not (noDups (sort (concat vss)))) $ panic ("Duplicate variables "++show xs)

renameOpSemStatements :: [SideCondition UnRenamedName] -> RenamerMonad a -> 
    RenamerMonad ([SideCondition Name], a)
renameOpSemStatements stmts prog = do
    checkDuplicates' stmts
    -- No duplicates, so we can just add one scope
    addScope $ do
        stmts' <- mapM (\ astmt -> 
                case astmt of
                    SCGenerator p e -> do
                        -- Rename e first, as it can't depend on p
                        e' <- rename e
                        p' <- renameOpSemPattern p
                        return $ SCGenerator p' e'
                    Formula e -> do
                        e' <- rename e
                        return $ Formula e'
            ) stmts
        a <- prog
        return (stmts', a)

typeCheckOperators :: InputOpSemDefinition UnRenamedName -> IO OpSemDefinition
typeCheckOperators (InputOpSemDefinition ops chans) = do
    let chanNames = [n | Channel n _ <- chans] 
    when (not (noDups chanNames)) $ panic (show "Dup chan names")

    renamerSt <- initRenamerNoBuiltins
    ((identityRule, chans, ops), _) <- runFromStateToState renamerSt $ do
        -- Inject the channels
        mapM (\ ocn -> do
            rcn <- externalNameMaker True An.Unknown ocn
            setName ocn rcn) chanNames
        -- Inject the operators
        mapM (\ op -> do
            rcn <- externalNameMaker False An.Unknown (iopFriendlyName op)
            setName (iopFriendlyName op) rcn) (identityRule:ops)
        -- Rename everyt
        identityRule <- rename identityRule
        ops <- rename ops
        chans <- rename chans
        return $ (identityRule, chans, ops)

    let
        chanNames = [n | Channel n _ <- chans]
        typeCheckOps =
            do
                errorIfFalse (noDups chanNames) (DuplicatedNameError chanNames)
                mapM injectChannelType chans
                ops <- concatMapM typeCheckOperator (identityRule:ops)
                return $ OpSemDefinition ops chans
        injectChannelType (Channel n es) =
            do
                ts <- mapM typeCheck es
                setType n (TChannel ts)

    (lh, st) <- 
        runStateT (runErrorT typeCheckOps) (TypeInferenceState [[]] 0 False)
    case lh of
        Left err -> panic (show err)
        Right ops -> return ops     

-- TODO: below, enforcing left application is operator, need to check this
-- and in general, need to check the syntatic conditions we enforce via pattern
-- matching
typeCheckOperator :: InputOperator Name -> TypeCheckMonad [Operator]
typeCheckOperator (InputOperator n argsSt rules replicatedOp syntax) = 
    let
        args = map fst argsSt
        onArgs = nub [n | InductiveRule pres _ _ <- rules, 
                            Performs (Var n) _ _<- pres]
        isOnArg arg = arg `elem` onArgs
        mergeType (name, st) (TOnProcess _) = TOnProcess st
        mergeType (name, st) (TOffProcess _) = TOffProcess st
        mergeType (name, Unknown) t = t
    in  
        addOperatorToError n (do
            errorIfFalse (noDups args) (DuplicatedNameError args)
            
            operatorTypeArgs <- replicateM (length args) freshTypeVar
            setType n (TOperator operatorTypeArgs)
            
            -- Type check all the rules: returns monads that will type check
            -- the operator calls each rule makes           
            local args (mapM_ (typeCheckInductiveRule args) rules)
            ts' <- mapM compress operatorTypeArgs
            let ts = map (\ t -> case t of
                                TVar _          -> TOffProcess Unknown
                                _               -> t) ts'
            
            let argsTypes = zipWith (\ (arg, st) t ->
                    case t of
                        -- Check that it actually is an on process
                        -- e.g it might infer that it is on because of a
                        -- recursive call that makes it on
                        TOnProcess st'  -> 
                            if isOnArg arg then mergeType (arg, st) t
                            else mergeType (arg, st) (TOffProcess st')
                        t               -> mergeType (arg, st) t) argsSt ts
            setType n (TOperator argsTypes)
            let op = Operator n (zip args argsTypes) rules syntax
            case replicatedOp of
                    Just repOp  -> 
                        do
                            repOp <- typeCheckReplicatedOperator n repOp
                            return [op, repOp]
                    Nothing     -> return [op])

typeCheckReplicatedOperator 
    :: Name -> InputReplicatedOperator Name -> TypeCheckMonad Operator
typeCheckReplicatedOperator n (InputReplicatedOperator args 
        (basePats, baseCase)
        (inductiveVars, inductiveCase) syntax) =
    do
        (tBaseCase, tPats) <- local (freeVars basePats) (do
            tPats <- mapM typeCheck basePats
            mapM ensureIsSet tPats
            t <- typeCheck baseCase
            ensureIsProc t
            return (t, tPats))

        -- TODO: need to check arg lengths match up (including the base case
        -- patterns)
        errorIfFalse (noDups (args++inductiveVars))
                    (DuplicatedNameError (args++inductiveVars))
        
        -- TODO: should check InductiveCase is used
        operatorArgTypes <- local (args++inductiveVars) (do
            t <- generalUnificationAllowed (typeCheck inductiveCase)
            ensureIsProc t
            tArgs <- mapM getType args
            generalUnificationAllowed (zipWithM unify tPats (map TSet tArgs))
            tArgs' <- mapM getType args
            return tArgs')
        
        ts <- mapM compress operatorArgTypes

        return $ ReplicatedOperator n
                    (zip args ts) (basePats, baseCase)
                    (inductiveVars, inductiveCase) syntax

-- Returns a type inference monad that, when called, will typecheck the
-- operator calls
typeCheckInductiveRule 
    :: [Name] -> InductiveRule Name -> TypeCheckMonad ()
typeCheckInductiveRule args
        (rule @ (InductiveRule pres (post @ (Performs opApp1 ev opApp2)) sc)) =
    addRuleToError rule (
    let
        onPreProcs = [(n, TOnProcess Unknown) | Performs (Var n) _ _ <- pres]
        onPostProcs = [(n, TOnProcess Unknown) | Performs _ _ (Var n) <- pres]
        argPostProcs = intersect args (map (\ (n, t) -> n) onPostProcs)
        -- We use freeVars' here as it does not remove duplicates
        procsCloned = [head xs | xs <- group (freeVars' opApp2),length xs > 1]
        isOnProc (TOnProcess _) = True
        isOnProc _ = False
    in do
        errorIfFalse (length argPostProcs == 0)
            (ArgumentsUsedOnRHSOfProcessRelation argPostProcs)
        -- TODO: this may be a valid Tau promotion rule - check
        errorIfFalse (length [r | r @ (Performs _ Tau _) <- pres] == 0) 
            TauPromotedError
            
        let varsToBind = varsBound sc
        errorIfFalse (noDups varsToBind) (DuplicatedNameError varsToBind)
        local varsToBind (do
            mapM_ (\ ev -> 
                case ev of
                    Event n         -> unifyTypeWithName n TEvent
                    ChanEvent n es  ->  do
                        t <- getType n
                        tsArgs <- mapM typeCheck es
                        unify t (TChannel $ map TSet tsArgs)
                        return ())
                [ev | Performs _ ev _ <- pres]
            
            mapM_ typeCheck sc
        
            case ev of
                Event n                 -> unifyTypeWithName n TEvent
                -- See comment in DataTypes: a channel may only have event 
                -- components
                ChanEvent chanName es   -> do
                        t <- getType chanName
                        tsArgs <- mapM typeCheck es
                        unify t (TChannel $ map TSet tsArgs)
                        return ()
                Tau                     -> return ()
                        
            -- TODO: check opApp1 is our operator
            mapM_ (uncurry unifyTypeWithName) onPreProcs
            typeCheck opApp1
            
            let discardedProcResults = 
                    intersect (freeVars opApp2) (map fst onPreProcs)
            errorIfFalse (length discardedProcResults == 0)
                (ResultingProcessDiscarded discardedProcResults)
            -- We use set type as they cannot be in scope
            mapM_ (uncurry setType) onPostProcs
            generalUnificationAllowed (typeCheck opApp2)
            
            onProcsCloned <- 
                filterM (\ n -> getType n >>= (\ t -> return $ isOnProc t)) 
                procsCloned
            errorIfFalse (length onProcsCloned == 0) 
                (OnProcessClonedError onProcsCloned)))
    

-- ***************************************************************************
-- Type Unification
-- ***************************************************************************
ensureIsSet :: Type -> TypeCheckMonad ()
ensureIsSet t =
    do
        fv <- freshTypeVar
        unify t (TSet fv)
        return ()

ensureIsProc :: Type -> TypeCheckMonad ()
ensureIsProc t =
    do
        t' <- compress t
        case t of
            TOnProcess _    -> return ()
            TOffProcess _   -> return ()
            _               -> unify t (TOnProcess Unknown) >> return ()

ensureIsChannel :: Name -> [Exp Name] -> TypeCheckMonad ()
ensureIsChannel n es =
    do
        freshVars <- replicateM (length es) freshTypeVar
        unifyTypeWithName n (TChannel (map TSet freshVars))
        return ()

unifyAll :: [Type] -> TypeCheckMonad Type
unifyAll [] = freshTypeVar
unifyAll [t] = return t
unifyAll (t1:ts) =
    do
        t2 <- unifyAll ts
        unify t1 t2

-- Unifies the two types, updating all types in the name map and returns the 
-- unified type
-- Important: unification may not necessarily be symmetric - 
-- see allowGeneralUnification
unify :: Type -> Type -> TypeCheckMonad Type
unify (TVar t1) (TVar t2) | t1 == t2 = 
    return (TVar t1)
unify (TVar t1) (TVar t2) = 
    do
        res1 <- readTypeRef t1
        res2 <- readTypeRef t2
        case (res1, res2) of
            (Left tv1, Left tv2)    -> applySubstitution t1 (TVar t2)
            (Left _, Right t)       -> unify (TVar t1) t
            (Right t, Left _)       -> unify t (TVar t2)
            (Right t1, Right t2)    -> unify t1 t2
unify (TVar a) b = 
    do
        res <- readTypeRef a
        case res of
            Left tva    -> applySubstitution a b
            Right t     -> unify t b
unify b (TVar a) = 
    do
        res <- readTypeRef a
        case res of
            Left tva    -> applySubstitution a b
            Right t     -> unify b t
unify (TSet a) (TSet b) = 
    do
        tr <- unify a b
        return (TSet tr)
unify (TTuple ts1) (TTuple ts2) | length ts1 == length ts2 =
    do
        ts' <- zipWithM unify ts1 ts2
        return $ TTuple ts'
unify (TChannel ts1) (TChannel ts2) | length ts1 == length ts2 =
    do
        ts' <- zipWithM unify ts1 ts2
        return $ TChannel ts'
unify (TOperator ts1) (TOperator ts2) | length ts1 == length ts2 =
    do
        ts' <- zipWithM unify ts1 ts2
        return $ TOperator ts'
unify TEvent TEvent = return TEvent
unify TProcArg TProcArg = return TProcArg
unify (TOnProcess _) (TOnProcess _) = return $ TOnProcess Unknown
unify (TOffProcess _) (TOffProcess _) = return $ TOffProcess Unknown
unify (TOnProcess st1) (TOffProcess st2) = 
    do
        r <- gets allowGeneralUnification
        if r then return $ TOnProcess Unknown else 
            throwError $ UnificationError (TOnProcess st1) (TOffProcess st2)
unify t1 t2 = 
    do 
        t1' <- compress t1
        t2' <- compress t2
        throwError $ UnificationError t1' t2'


-- Returns the type that we substitute for
applySubstitution :: TypeVarRef -> Type -> TypeCheckMonad Type
applySubstitution (tvref @ (TypeVarRef tv _)) typ =
    do
        t' <- compress typ
        errorIfFalseM (liftM not (occurs tv typ)) (InfiniteUnificationError tv t')
        writeTypeRef tvref typ
        return typ

-- Apply the substitution type for tv to the type
{-substituteType :: (TypeVar, Type) -> Type -> TypeCheckMonad Type
substituteType (tv, t) (TVar a) = if a == tv then t else TVar a
substituteType _ (TEvent) = return TEvent
substituteType _ (TOnProcess) = return TOnProcess
substituteType _ (TOffProcess) = return TOffProcess
substituteType (tv, t) (TSet t1) = TSet (substituteType (tv,t) t1)
substituteType (tv, t) (TTuple ts) = 
    TTuple (map (substituteType (tv,t)) ts)
-}

occurs :: TypeVar -> Type -> TypeCheckMonad Bool
occurs a (TVar (tvref @ (TypeVarRef tv _))) = 
    do
        res <- readTypeRef tvref
        case res of 
            Left tv -> return $ a == tv
            Right t -> occurs a t
occurs a (TSet t) = occurs a t
occurs a (TTuple ts) = liftM or (mapM (occurs a) ts)
occurs a (TChannel ts) = liftM or (mapM (occurs a) ts)
occurs a TEvent = return False
occurs a (TOnProcess _) = return False
occurs a (TOffProcess _) = return False
occurs a TProcArg = return False
