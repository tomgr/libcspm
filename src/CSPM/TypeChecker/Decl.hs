{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module CSPM.TypeChecker.Decl (typeCheckDecls) where

import Control.Monad
import Control.Monad.Trans
import Data.Graph.Wrapper
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub, intersect, (\\), sortBy)

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax hiding (getType)
import CSPM.DataStructures.Types
import CSPM.PrettyPrinter
import CSPM.TypeChecker.BuiltInFunctions
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Dependencies
import CSPM.TypeChecker.Exceptions
import CSPM.TypeChecker.Expr
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Pat
import CSPM.TypeChecker.Unification
import Util.Annotated
import Util.List
import Util.Monad
import Util.PartialFunctions
import Util.PrettyPrint

-- | Type check a list of possibly mutually recursive functions
typeCheckDecls :: [PDecl] -> TypeCheckMonad ()
typeCheckDecls decls = do
    namesBoundByDecls <- mapM (\ decl -> do
        namesBound <- namesBoundByDecl decl
        return (decl, namesBound)) decls
        
    let 
        -- | Map from declarations to integer identifiers
        declMap = zip decls [0..]
        invDeclMap = invert declMap
        -- | Map from names to the identifier of the declaration that it is
        -- defined by.
        varToDeclIdMap = 
            [(n, apply declMap d) | (d, ns) <- namesBoundByDecls, n <- ns]
        boundVars = map fst varToDeclIdMap
        namesToLocations = [(n, loc d) | (d, ns) <- namesBoundByDecls, n <- ns]

    -- Throw an error if a name is defined multiple times
    manyErrorsIfFalse (noDups boundVars) 
        (duplicatedDefinitionsMessage namesToLocations)

    -- We prebind the datatypes and channels as they can be matched on in 
    -- patterns (and thus, given a var in a pattern we can't decide if it
    -- is free or a dependency otherwise).
    mapM_ registerChannelsAndDataTypes (map unAnnotate decls)

    -- Map from decl id -> [decl id] meaning decl id depends on the list of
    -- ids
    declDeps <- mapM (\ decl -> do
            deps <- dependencies decl
            let depsInThisGroup = intersect deps boundVars
            return (apply declMap decl, mapPF varToDeclIdMap depsInThisGroup)
        ) decls

    let 
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
        typeInferenceGroup = mapPF invDeclMap

        -- When an error occurs continue type checking, but only
        -- type check groups that do not depend on the failed group.
        -- failM is called at the end if any error has occured.
        typeCheckGroups [] b = if b then failM else return ()
        typeCheckGroups (g:gs) b = do
            err <- tryAndRecover (do
                typeCheckMutualyRecursiveGroup (typeInferenceGroup (S.toList g))
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

-- This method heavily affects the DataType clause of typeCheckDecl.
-- If any changes are made here changes will need to be made to typeCheckDecl
-- too

-- We have to prebind all datatype clauses and channel names so
-- that we can identify when a particular pattern uses these clauses and
-- channels. We do this by injecting them into the symbol table earlier
-- than normal.
registerChannelsAndDataTypes :: Decl -> TypeCheckMonad ()
registerChannelsAndDataTypes (DataType n cs) = do
    mapM_ (\ c -> case unAnnotate c of
            DataTypeClause n' _ -> addDataTypeOrChannel n'
        ) cs
registerChannelsAndDataTypes (Channel ns _) = 
    mapM_ addDataTypeOrChannel ns
registerChannelsAndDataTypes _ = return ()

-- | Type checks a group of certainly mutually recursive functions. Only 
-- functions that are mutually recursive should be included otherwise the
-- types could end up being less general.
typeCheckMutualyRecursiveGroup :: [PDecl] -> TypeCheckMonad ()
typeCheckMutualyRecursiveGroup ds' = do
    -- TODO: fix temporary hack
    let 
        cmp x y = case (unAnnotate x, unAnnotate y) of
            (DataType _ _, DataType _ _) -> EQ
            (DataType _ _, _) -> LT
            (_, DataType _ _) -> GT
            (_, _) -> EQ
        ds = sortBy cmp ds'
    fvs <- liftM nub (concatMapM namesBoundByDecl ds)

    ftvs <- replicateM (length fvs) freshTypeVar
    zipWithM setType fvs (map (ForAll []) ftvs)
    
    -- The list of all variables bound by these declaration
    fvs <- liftM nub (concatMapM namesBoundByDecl ds)

    -- Type check each declaration then generalise the types
    nts <- generaliseGroup fvs (map typeCheck ds)
    -- Add the type of each declaration (if one exists to each declaration)
    zipWithM annotate nts ds

    -- Compress all the types we have inferred here (they should never be 
    -- touched again)
    mapM_ (\ n -> do
        t <- getType n
        t' <- compressTypeScheme t
        setType n t') fvs
    where        
        annotate nts (An _ psymbtable _) = setPSymbolTable (snd psymbtable) nts

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
    fv <- freshTypeVar
    unify t (TSet fv)
    return fv

instance TypeCheckable PDecl [(Name, Type)] where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)

instance TypeCheckable Decl [(Name, Type)] where
    errorContext (FunBind n ms) = Just $ 
        -- This will only be helpful if the equations don't match in
        -- type
        text "In the declaration of:" <+> prettyPrint n
    errorContext (p@(PatBind pat exp)) = Just $
        hang (text "In a pattern binding:") tabWidth (prettyPrint p)
    errorContext (DataType n cs) = Just $
        text "In the declaration of:" <+> prettyPrint n
    errorContext (NameType n e) = Just $
        text "In the declaration of:" <+> prettyPrint n
    errorContext (Channel ns es) = Just $
        text "In the declaration of:" <+> list (map prettyPrint ns)
    errorContext (Assert a) = Just $
        text "In the assertion:" <+> prettyPrint a
    errorContext (Transparent ns) = Nothing
    errorContext (External ns) = Nothing
    
    typeCheck' (FunBind n ms) = do
        ts <- mapM (\ m -> addErrorContext (matchCtxt m) $ typeCheck m) ms
        ForAll [] t <- getType n
        -- This unification also ensures that each equation has the same number
        -- of arguments.
        (t' @ (TFunction tsargs _)) <- unifyAll (t:ts)
        return [(n, t')]
        where
            matchCtxt an = 
                hang (text "In an equation for" <+> prettyPrint n <> colon) 
                    tabWidth (prettyPrintMatch n an)
    typeCheck' (PatBind pat exp) = do
        tpat <- typeCheck pat
        texp <- typeCheck exp
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
        ns <- namesBoundByDecl' (PatBind pat exp)
        ts <- mapM getType ns
        return $ zip ns [t | ForAll _ t <- ts]
    -- The following two clauses rely on the fact that they have been 
    -- prebound.
    typeCheck' (Channel ns Nothing) = do
        -- We now unify the each type to be a TEvent
        mapM (\ n -> do
            ForAll [] t <- getType n
            unify TEvent t) ns
        -- (Now getType n for any n in ns will return TEvent)
        return [(n, TEvent) | n <- ns]
    typeCheck' (Channel ns (Just e)) = do
        t <- typeCheck e
        valueType <- evalTypeExpression t
        dotList <- typeToDotList valueType
        let t = foldr TDotable TEvent dotList
        mapM (\ n -> do
            ForAll [] t' <- getType n
            unify t' t) ns
        return $ [(n, t) | n <- ns]
    typeCheck' (DataType n clauses) = do
        nts <- mapM (\ clause -> do
            let 
                n' = case unAnnotate clause of
                        DataTypeClause x _ -> x
            ForAll [] t <- getType n'
            (n', ts) <- typeCheck clause
            let texp = foldr TDotable (TDatatype n) ts
            t <- unify texp t
            return (n', t)
            ) clauses
        ForAll [] t <- getType n
        t' <- unify t (TSet (TDatatype n))
        return $ (n, t'):nts
    typeCheck' (NameType n e) = do
        t <- typeCheck e
        valueType <- evalTypeExpression t
        return [(n, TSet valueType)]
    typeCheck' (Transparent ns) = do
        mapM_ (\ (n@(Name s)) -> do
            texp <- applyPFOrError (transparentFunctionNotRecognised n) 
                                transparentFunctions s
            ForAll [] t <- getType n
            unify texp t) ns
        return []
    typeCheck' (External ns) = do
        mapM_ (\ (n@(Name s)) -> do
            texp <- applyPFOrError (externalFunctionNotRecognised n) 
                                externalFunctions s
            ForAll [] t <- getType n
            unify texp t) ns
        return []
    typeCheck' (Assert a) = typeCheck a >> return []

instance TypeCheckable Assertion () where
    errorContext a = Just $ 
        hang (text "In the assertion" <> colon) tabWidth (prettyPrint a)
    typeCheck' (PropertyCheck e1 p m) = do
        ensureIsProc e1
        return ()
    typeCheck' (Refinement e1 m e2 opts) = do
        ensureIsProc e1
        ensureIsProc e2
        mapM_ typeCheck opts

instance TypeCheckable ModelOption () where
    errorContext a = Nothing
    typeCheck' (TauPriority e) = do
        typeCheckExpect e (TSet TEvent)
        return ()

instance TypeCheckable PDataTypeClause (Name, [Type]) where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)

instance TypeCheckable DataTypeClause (Name, [Type]) where
    errorContext c = Just $
        hang (text "In the data type clause" <> colon) tabWidth 
            (prettyPrint c)
    typeCheck' (DataTypeClause n' Nothing) = do
        return (n', [])
    typeCheck' (DataTypeClause n' (Just e)) = do
        t <- typeCheck e
        valueType <- evalTypeExpression t
        dotList <- typeToDotList valueType
        return (n', dotList)

instance TypeCheckable PMatch Type where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)
instance TypeCheckable Match Type where
    -- We create the error context in FunBind as that has access
    -- to the name
    errorContext (Match groups exp) = Nothing
    typeCheck' (Match groups exp) = do
        -- Introduce free variables for all the parameters
        fvs <- liftM concat (mapM freeVars groups)
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

applyPFOrError 
    :: Eq a => Error -> PartialFunction a b -> a -> TypeCheckMonad b
applyPFOrError err pf a = 
    case safeApply pf a of
        Just a -> return a
        Nothing -> raiseMessageAsError err
