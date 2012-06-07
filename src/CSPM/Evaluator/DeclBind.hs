{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CSPM.Evaluator.DeclBind (
    bindDecls, 
) where

import Control.Monad
import Data.List (partition)
import Data.Maybe
import System.IO.Unsafe

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.Dot
import CSPM.Evaluator.Exceptions
import {-# SOURCE #-} CSPM.Evaluator.Expr
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import Util.Annotated
import Util.Exception
import Util.Monad

-- | Given a list of declarations, returns a sequence of names bounds to
-- values that can be passed to 'addScopeAndBind' in order to bind them in
-- the current scope.
bindDecls :: [TCDecl] -> EvaluationMonad [(Name, EvaluationMonad Value)]
bindDecls ds = do
    nvs <- concatMapM bindDecl ds 
    let eventsName = builtInName "Events"
        (eventNvs, normalNvs) = partition (\x -> fst x == eventsName) nvs
        computeEvents vs = do
            vss <- sequence (map snd eventNvs)
            return $ VSet $ unions $ vs : map (\ (VSet s) -> s) vss

    -- Lookup the existing value of events and add to it
    VSet vs <- lookupVar eventsName
    return $ (eventsName, computeEvents vs)  : normalNvs

bindDecl :: TCDecl -> EvaluationMonad [(Name, EvaluationMonad Value)]
bindDecl (an@(An _ _ (FunBind n ms))) = do
    parentPid <- getParentProcName
    let
        matches = map unAnnotate ms
        argGroupCount = head (map (\ (Match pss e) -> length pss) matches)
        collectArgs :: Int -> [[Value]] -> EvaluationMonad Value
        collectArgs 0 ass_ =
            let
                procName = procId n argGroups parentPid
                argGroups = reverse ass_
                tryMatches [] = throwError $ 
                    funBindPatternMatchFailureMessage (loc an) n argGroups
                tryMatches (Match pss e : ms) =
                    let 
                        bindResult = zipWith bindAll pss argGroups
                        (bindResults, boundValuess) = unzip bindResult
                        binds = concat boundValuess
                    in 
                        if not (and bindResults) then tryMatches ms
                        else
                            addScopeAndBind binds $ updateParentProcName procName $ do
                                v <- eval e
                                case v of
                                    VProc p -> return $ VProc $ PProcCall procName p
                                    _ -> return $ v
            in tryMatches matches
        collectArgs n ass =
            return $ VFunction $ \ vs -> collectArgs (n-1) (vs:ass)
    return $ [(n, collectArgs argGroupCount [])]
bindDecl (an@(An _ _ (PatBind (p@(An _ _ (PVar n))) e))) | not (nameIsConstructor n) = do
    -- Optimise for this case
    parentPid <- getParentProcName
    let ev = maybeSave (getType e) $ do
            val <- eval e
            case val of
                VProc p -> return $ VProc $ PProcCall (procId n [] parentPid) p
                _ -> return $ val
    return $ [(n, ev)]
bindDecl (an@(An _ _ (PatBind p e))) = do
    parentPid <- getParentProcName
    let
        -- We put p inside the unsafePerformIO simply to prevent it being
        -- lifed out of the lambda (and thus only being performed once).
        nV = unsafePerformIO (p `seq` mkFreshInternalName)
        {-# NOINLINE nV #-}

        typeOf :: Name -> Type
        typeOf n = head [t | (n', ForAll _ t) <- getSymbolTable an, n' == n]

        shouldSaveValue = [n | (n, ForAll [] TProc) <- getSymbolTable an] == []

        value = if shouldSaveValue then eval e else noSave (eval e)

        extractor :: Name -> EvaluationMonad Value
        extractor n = maybeSave (typeOf n) $ do
            v <- lookupVar nV
            let 
                nvs = case bind p v of
                        (True, nvs) -> nvs
                        (False, _) -> throwError $ 
                            patternMatchFailureMessage (loc an) p v
                val = head [v | (n', v) <- nvs, n' == n]
            -- We don't need to update the parent proc name as the renamer
            -- disambiguates things for us
            case val of
                VProc p -> return $ VProc $ PProcCall (procId n [] parentPid) p
                _ -> return $ val
    return $ (nV, value):[(fv, extractor fv) | fv <- freeVars p]
bindDecl (an@(An _ _ (Channel ns me))) =
    let
        mkChan :: Name -> EvaluationMonad Value
        mkChan n = do
            vss <- case me of
                Just e -> eval e >>= evalTypeExprToList
                Nothing -> return []
            
            let arity = fromIntegral (length vss)
            return $ VTuple [VDot [VChannel n], VInt arity, VList (map VSet vss)]
        eventSetValue :: EvaluationMonad Value
        eventSetValue = do
            ss <- mapM (\ n -> completeEvent (VDot [VChannel n])) ns
            return $ VSet (unions ss)
    -- We bind to events here, and this is picked up in bindDecls
    in return $ (builtInName "Events", eventSetValue) : [(n, mkChan n) | n <- ns]
bindDecl (an@(An _ _ (DataType n cs))) =
    let
        mkDataTypeClause :: DataTypeClause Name -> (Name, EvaluationMonad Value)
        mkDataTypeClause (DataTypeClause nc me) = (nc, do
            vss <- case me of
                Just e -> eval e >>= evalTypeExprToList
                Nothing -> return []
            let arity = fromIntegral (length vss)
            return $ VTuple [VDot [VDataType nc], VInt arity, VList (map VSet vss)])
        computeSetOfValues :: EvaluationMonad Value
        computeSetOfValues =
            let 
                mkSet nc = do
                    VTuple [_, _, VList fields] <- lookupVar nc
                    let fs = fromList [VDataType nc] : map (\(VSet vs) -> vs) fields
                    return $ cartesianProduct CartDot fs
            in do
                vs <- mapM mkSet [nc | DataTypeClause nc _ <- map unAnnotate cs]
                return $ VSet (unions vs)
    in return $ (n, computeSetOfValues):(map mkDataTypeClause (map unAnnotate cs))
bindDecl (an@(An _ _ (NameType n e))) = return $
    [(n, do
        v <- eval e
        sets <- evalTypeExprToList v
        return $ VSet $ cartesianProduct CartDot sets)]

bindDecl (an@(An _ _ (Assert _))) = return []
bindDecl (an@(An _ _ (External ns))) = return []
bindDecl (an@(An _ _ (Transparent ns))) = return []

evalTypeExpr :: Value -> ValueSet
evalTypeExpr (VSet s) = s
evalTypeExpr (VTuple vs) = cartesianProduct CartTuple (map evalTypeExpr vs)

evalTypeExprToList :: Value -> EvaluationMonad [ValueSet]
evalTypeExprToList (VDot vs) = concatMapM evalTypeExprToList vs
evalTypeExprToList v = splitIntoFields (evalTypeExpr v)

class FreeVars a where
    freeVars :: a -> [Name]

instance FreeVars a => FreeVars (Annotated b a) where
    freeVars (An _ _ inner) = freeVars inner

instance FreeVars (Pat Name) where
    freeVars (PConcat p1 p2) = freeVars p1++freeVars p2
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
