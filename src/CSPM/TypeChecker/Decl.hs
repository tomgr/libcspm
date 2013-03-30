{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module CSPM.TypeChecker.Decl (typeCheckDecls) where

import Control.Monad
import Data.Graph.Wrapper
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intersect, (\\), sortBy)

import CSPM.DataStructures.FreeVars
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax hiding (getType)
import CSPM.DataStructures.Types
import CSPM.PrettyPrinter
import CSPM.TypeChecker.Common
import CSPM.TypeChecker.Expr()
import CSPM.TypeChecker.Monad
import CSPM.TypeChecker.Pat()
import CSPM.TypeChecker.Unification
import Util.Annotated
import Util.List
import Util.PartialFunctions
import Util.PrettyPrint

-- | Type check a list of possibly mutually recursive functions
typeCheckDecls :: [TCDecl] -> TypeCheckMonad ()
typeCheckDecls decls = do

    -- Flatten the decls so that definitions in modules are also type-checked
    -- in the correct order.
    let flattenDecl :: TCDecl -> [TCDecl]
        flattenDecl (An _ _ (Module _ _ ds1 ds2)) =
            concatMap flattenDecl ds1++concatMap flattenDecl ds2
        flattenDecl (d@(An _ _ (TimedSection _ _ ds))) =
            -- We need to type-check the function in the timed section, but we
            -- flatten the decls so that dependencies work out ok.
            d : concatMap flattenDecl ds
        flattenDecl x = [x]
        flatDecls = concatMap flattenDecl decls

        -- | Map from declarations to integer identifiers
        declMap = zip flatDecls [0..]
        invDeclMap = invert declMap

    let
        namesBoundByDecls = concatMap (\ (decl, declId) ->
            case decl of
                An _ _ (TimedSection _ _ _) -> []
                _ -> [(declId, boundNames decl)]) declMap

        -- | Map from names to the identifier of the declaration that it is
        -- defined by.
        varToDeclIdMap = 
            [(n, declId) | (declId, ns) <- namesBoundByDecls, n <- ns]
        boundVars = map fst varToDeclIdMap

    -- Throw an error if a name is defined multiple times
    when (not (noDups boundVars)) $ panic "Duplicates found after renaming."

    -- Map from decl id -> [decl id] meaning decl id depends on the list of
    -- ids
    declDeps <- mapM (\ (decl, declId) -> do
            let deps = freeVars decl
            let depsInThisGroup = intersect deps boundVars
            return (declId, mapPF varToDeclIdMap depsInThisGroup)
        ) declMap

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
            err <- tryAndRecover True (do
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
typeCheckMutualyRecursiveGroup :: [TCDecl] -> TypeCheckMonad ()
typeCheckMutualyRecursiveGroup ds' = do
    -- TODO: fix temporary hack
    let 
        cmp x y = case (unAnnotate x, unAnnotate y) of
            (DataType _ _, DataType _ _) -> EQ
            (DataType _ _, _) -> LT
            (_, DataType _ _) -> GT
            (_, _) -> EQ
        ds = sortBy cmp ds'
        fvs = boundNames ds

    ftvs <- replicateM (length fvs) freshTypeVar
    zipWithM setType fvs (map (ForAll []) ftvs)

    -- Type check each declaration then generalise the types
    nts <- generaliseGroup fvs (map typeCheck ds)

    -- Compress all the types we have inferred here (they should never be 
    -- touched again)
    mapM_ (\ n -> do
        t <- getType n
        t' <- compressTypeScheme t
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
    fv <- freshTypeVar
    unify t (TSet fv)
    return fv

instance TypeCheckable TCDecl [(Name, Type)] where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)

instance TypeCheckable (Decl Name) [(Name, Type)] where
    errorContext (FunBind n ms _) = Just $ 
        -- This will only be helpful if the equations don't match in
        -- type
        text "In the declaration of:" <+> prettyPrint n
    errorContext (p@(PatBind pat exp _)) = Just $
        hang (text "In a pattern binding:") tabWidth (prettyPrint p)
    errorContext (DataType n cs) = Just $
        text "In the declaration of:" <+> prettyPrint n
    errorContext (SubType n cs) = Just $
        text "In the declaration of:" <+> prettyPrint n
    errorContext (NameType n e) = Just $
        text "In the declaration of:" <+> prettyPrint n
    errorContext (Channel ns es) = Just $
        text "In the declaration of:" <+> list (map prettyPrint ns)
    errorContext (Assert a) = Just $
        text "In the assertion:" <+> prettyPrint a
    errorContext (TimedSection _ _ _) = Nothing
    errorContext (Transparent ns) = Nothing
    errorContext (External ns) = Nothing
    
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
                hang (text "In an equation for" <+> prettyPrint n <> colon) 
                    tabWidth (prettyPrintMatch n an)
    typeCheck' (p@(PatBind pat exp mta)) = do
        (texp, tpat) <-
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
    typeCheck' (Channel ns Nothing) = do
        -- We now unify the each type to be a TEvent
        mapM (\ n -> do
            ForAll [] t <- getType n
            unify TEvent t) ns
        -- (Now getType n for any n in ns will return TEvent)
        return [(n, TEvent) | n <- ns]
    typeCheck' (Channel ns (Just e)) = do
        t <- typeCheck e
        -- Events must be comparable for equality.
        ensureHasConstraint CEq t
        valueType <- evalTypeExpression t
        dotList <- typeToDotList valueType
        let t = foldr TDotable TEvent dotList
        mapM (\ n -> do
            ForAll [] t' <- getType n
            unify t' t) ns
        return $ [(n, t) | n <- ns]
    typeCheck' (SubType n clauses) = do
        -- Get the type fromthe first clause
        parentType <- freshTypeVar
        mapM_ (\ clause -> do
                let nclause = case unAnnotate clause of
                            DataTypeClause x _ -> x
                (_, tsFields) <- typeCheck clause
                ForAll [] typeCon <- getType nclause
                (actFields, dataType) <- dotableToDotList typeCon
                -- Check that the datatype is the correct subtype.
                tvref' <- freshTypeVarRef []
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
                        DataTypeClause x _ -> x
            ForAll [] t <- getType n'
            (n', ts) <- typeCheck clause
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

        -- We mark the type for equality, as if the type depends only on itself
        -- (i.e. it is recursive), then it should be comparable for equality.
        markDatatypeAsComparableForEquality n
        b <- tryAndRecover False 
                (mapM_ (ensureHasConstraint CEq) tclauses >> return True)
                (return False)
        when (not b) $ unmarkDatatypeAsComparableForEquality n
        ForAll [] t <- getType n
        t' <- unify t (TSet (TDatatype n))
        return $ (n, t'):nts
    typeCheck' (NameType n e) = do
        t <- typeCheck e
        valueType <- evalTypeExpression t
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

instance TypeCheckable TCAssertion () where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)

instance TypeCheckable (Assertion Name) () where
    errorContext a = Just $ 
        hang (text "In the assertion" <> colon) tabWidth (prettyPrint a)
    typeCheck' (PropertyCheck e1 p m) = do
        ensureIsProc e1
        return ()
    typeCheck' (Refinement e1 m e2 opts) = do
        ensureIsProc e1
        ensureIsProc e2
        mapM_ typeCheck opts
    typeCheck' (ASNot a) = typeCheck a

instance TypeCheckable (ModelOption Name) () where
    errorContext a = Nothing
    typeCheck' (TauPriority e) = do
        typeCheckExpect e (TSet TEvent)
        return ()

instance TypeCheckable TCDataTypeClause (Name, [Type]) where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)

instance TypeCheckable (DataTypeClause Name) (Name, [Type]) where
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
            rt <- freshTypeVar
            argts <- mapM (flip replicateM freshTypeVar) (map length groups)
            unify tsig $ foldr (\ targs tr -> TFunction targs tr) rt argts

            -- The rest of the code is as before (comments before also apply)
            tgroups <- zipWithM (\ pats argts -> zipWithM (\ pat argt -> 
                    typeCheckExpect pat argt >>= evaluateDots
                ) pats argts) groups argts    
            tr <- typeCheckExpect exp rt >>= evaluateDots
            tgroups <- mapM (\ pats -> mapM (\ pat -> 
                    typeCheck pat >>= evaluateDots
                ) pats) groups
            return $ foldr (\ targs tr -> TFunction targs tr) tr tgroups

instance TypeCheckable TCSTypeScheme TypeScheme where
    errorContext an = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)
instance TypeCheckable (STypeScheme Name) TypeScheme where
    errorContext _ = Nothing
    typeCheck' (STypeScheme boundNs cs t) = do
        tvs <- mapM (\ n -> do
            let ncs = map (\ (STypeConstraint c _) -> c) $
                        filter (\ (STypeConstraint _ n') -> n == n') $
                        (map unAnnotate cs)
            t@(TVar tvref) <- freshRigidTypeVarWithConstraints n ncs
            setType n (ForAll [] t)
            return (typeVar tvref, constraints tvref)
            ) boundNs
        t' <- typeCheck t
        return $ ForAll tvs t'

instance TypeCheckable TCSType Type where
    errorContext _ = Nothing
    typeCheck' an = setSrcSpan (loc an) $ typeCheck (inner an)
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
