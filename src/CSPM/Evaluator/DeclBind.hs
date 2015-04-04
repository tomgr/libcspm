{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CSPM.Evaluator.DeclBind (
    bindDecls, 
) where

import Data.List (partition)

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.Evaluator.AnalyserMonad
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.DataTypeAnalyser
import CSPM.Evaluator.Dot
import CSPM.Evaluator.Exceptions
import {-# SOURCE #-} CSPM.Evaluator.Expr
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Profiler
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import Util.Annotated
import Util.Monad

-- | Given a list of declarations, returns a sequence of names bounds to
-- values that can be passed to 'addScopeAndBind' in order to bind them in
-- the current scope.
bindDecls :: [TCDecl] -> AnalyserMonad (EvaluationMonad [(Name, EvaluationMonad Value)])
bindDecls ds = do
    registerDataTypes ds
    ds' <- mapM bindDecl ds
    return $ do
        registerCall <- maybeRegisterCall
        nvs' <- sequence ds'
        let nvs = concat nvs'
            eventsName = builtInName "Events"
            (eventNvs, normalNvs) = partition (\x -> fst x == eventsName) nvs
            computeEvents vs = registerCall eventsName $ do
                vss <- sequence (map snd eventNvs)
                return $ VSet $ infiniteUnions $ vs : map (\ (VSet s) -> s) vss

            isChannelDecl (Channel _ _ _) = True
            isChannelDecl _ = False

        if or (map (isChannelDecl . unAnnotate) ds) then do
            -- Lookup the existing value of events and add to it
            VSet vs <- lookupVar eventsName
            return $ (eventsName, computeEvents vs)  : normalNvs
        else return nvs

bindDecl :: TCDecl -> AnalyserMonad (EvaluationMonad [(Name, EvaluationMonad Value)])
bindDecl (an @(An loc _ (FunBind n ms _))) = do
    let
        matches = map unAnnotate ms
        argGroupCount = head (map (\ (Match pss e) -> length pss) matches)

        collectArgs :: Int -> AnalyserMonad ([[Value]] -> EvaluationMonad Value)
        collectArgs 0 = do
            let
                tryMatches [] = return $! \ argGroups -> throwError $ 
                    funBindPatternMatchFailureMessage loc n argGroups
                tryMatches (Match pss exp : ms) = do
                    e <- eval exp
                    rest <- tryMatches ms
                    binder <- bindAll (concat pss)
                    return $! \ argGroups -> do
-- TODO: optimise away from argGroups
                        case binder (concat argGroups) of
                            Nothing -> rest argGroups
                            Just binds -> do
                                parentScope <- getParentScopeIdentifier
                                registerCall <- maybeRegisterCall
                                let scopeName = scopeId n argGroups parentScope
                                addScopeAndBind binds $ updateParentScopeIdentifier scopeName $ do
                                    v <- registerCall n e
                                    case getType exp of
                                        TProc ->
                                            let VProc p = v
                                            in return $ VProc $ PProcCall (procName scopeName) p
                                        _ -> return $ v
            ms <- tryMatches matches
            return $! \ ass -> do
                let argGroups = reverse ass
                ms argGroups
        collectArgs number = do
            rest <- collectArgs (number-1)
            return $! \ ass -> do
                -- Make sure we save the enclosing environment (as it contains
                -- variables that we need).
                st <- gets id
                parentScope <- getParentScopeIdentifier
                let fid = matchBindFunction n (reverse ass) parentScope
                return $ VFunction fid $ \ vs ->
                    (if profilerActive st then
                        modify (\ st' -> st {
                            -- Don't reset the profiler state
                            profilerState = profilerState st'
                        }) 
                    else modify (\ _ -> st)) $ rest (vs:ass)
    fn <- collectArgs argGroupCount
    return $ return $ [(n, fn [])]
bindDecl (an@(An _ _ (PatBind p e _))) = do
    let [(n, ForAll _ t)] = getSymbolTable an
    e <- eval e
    binder <- bind p
    return $! return [(n, do
        parentScope <- getParentScopeIdentifier
        registerCall <- maybeRegisterCall
        let scopeName = scopeId n [] parentScope
        maybeSave t $ updateParentScopeIdentifier scopeName $ do
            v <- registerCall n e
            case binder v of
                Just [(_, val)] -> return $!
                    case t of
                        TProc ->
                            let VProc p = val
                            in VProc $ PProcCall (procName scopeName) p
                        _ ->  val
                _ -> throwError $ patternMatchFailureMessage (loc an) p v
        )]
bindDecl (an@(An _ _ (Channel ns me _))) = do
    let
        mkChan :: Name -> AnalyserMonad (Name, EvaluationMonad Value)
        mkChan n = do
            fs <- case me of
                Just e -> do
                    e <- eval e
                    return $! e >>= evalTypeExprToList n
                Nothing -> return $! return []
            return $! (n, do
                registerCall <- maybeRegisterCall
                registerCall n $ do
                    fields <- fs
                    let arity = fromIntegral (length fields)
                    return $! tupleFromList [
                        VDot [VChannel n],
                        VInt arity,
                        tupleFromList (map VSet fields)]
                        )
        eventSetValue :: EvaluationMonad Value
        eventSetValue = do
            ss <- mapM (\ n -> do
                (_, _, fields) <- dataTypeInfo n
                let fs = fromList [VChannel n] : elems fields
                return $ cartesianProduct CartDot fs) ns
            return $ VSet (infiniteUnions ss)
    cs <- mapM mkChan ns
    return $ do
        -- We bind to events here, and this is picked up in bindDecls
        return $ (builtInName "Events", eventSetValue) : cs
bindDecl (an@(An _ _ (DataType n cs))) = do
    let
        mkDataTypeClause :: TCDataTypeClause -> AnalyserMonad (Name, EvaluationMonad Value)
        mkDataTypeClause (An _ _ (DataTypeClause nc me _)) = do
            fs <- case me of
                Just e -> do
                    e <- eval e
                    return $! e >>= evalTypeExprToList nc
                Nothing -> return $! return []
            return (nc, do
                vss <- fs
                let arity = fromIntegral (length vss)
                return $! tupleFromList [
                    VDot [VDataType nc],
                    VInt arity,
                    tupleFromList (map VSet vss)])

        computeSetOfValues :: EvaluationMonad Value
        computeSetOfValues = 
            let 
                mkSet nc = do
                    (_, _, fields) <- dataTypeInfo nc
                    let fs = fromList [VDataType nc] : elems fields
                    return $ cartesianProduct CartDot fs
            in do
                registerCall <- maybeRegisterCall
                registerCall n $ do
                    vs <- mapM mkSet [nc | DataTypeClause nc _ _ <- map unAnnotate cs]
                    return $ VSet (infiniteUnions vs)
    cs <- mapM mkDataTypeClause cs
    return $! return $! (n, computeSetOfValues):cs
bindDecl (an@(An _ _ (SubType n cs))) = do
    let
        analyseSetOfValues :: TCDataTypeClause -> AnalyserMonad (EvaluationMonad ValueSet)
        analyseSetOfValues (An _ _ (DataTypeClause nc me _)) = do
            fs <- case me of
                Just e -> do
                    e <- eval e
                    return $! e >>= evalTypeExprToList nc
                Nothing -> return $! return []
            return $! do
                fs <- fs
                let s = cartesianProduct CartDot (fromList [VDataType nc] : fs)
                vs <- mapM productionsSet (toList s)
                return (infiniteUnions vs)

    cs <- mapM analyseSetOfValues cs
    let calculateSet = do
            registerCall <- maybeRegisterCall
            vs <- sequence cs
            registerCall n $ return $ VSet (infiniteUnions vs)
    return $! return $! [(n, calculateSet)]
bindDecl (an@(An _ _ (NameType n e _))) = do
    e <- eval e
    let calculateSet = do
        registerCall <- maybeRegisterCall
        v <- e
        sets <- evalTypeExprToList n v
        -- If we only have one set then this is not a cartesian product, this is
        -- just introducing another name (see TPC P543 and
        -- evaluator/should_pass/nametypes.csp).
        registerCall n $ case sets of
            [s] -> return $ VSet s
            _ -> return $ VSet $ cartesianProduct CartDot sets
    return $ return $ [(n, calculateSet)]
bindDecl (an@(An _ _ (TimedSection (Just tn) f ds))) = do
    fnName <- mkFreshInternalName
    f <- case f of
            Nothing -> return Nothing
            Just f -> do
                f <- eval f
                return $! Just f
    withinTimedSection tn fnName $! do
        nds <- mapM bindDecl ds
        let 
            tockFn :: EvaluationMonad Value
            tockFn = 
                case f of
                    Nothing -> return $! VFunction 
                        (builtInFunction fnName [])
                        (\ _ -> return $! VInt 1)
                    Just f -> f
        return $! do
            nds <- sequence nds
            return $! (fnName, tockFn) : concat nds

bindDecl (an@(An _ _ (Assert _))) = return $! return []
bindDecl (an@(An _ _ (PrintStatement _))) = return $! return []

evalTypeExpr :: Value -> ValueSet
evalTypeExpr (VSet s) = s
evalTypeExpr (VTuple vs) = cartesianProduct CartTuple (map evalTypeExpr (elems vs))

evalTypeExprToList :: Name -> Value -> EvaluationMonad [ValueSet]
evalTypeExprToList n (VDot vs) = concatMapM (evalTypeExprToList n) vs
evalTypeExprToList n v = splitIntoFields False n (evalTypeExpr v)
