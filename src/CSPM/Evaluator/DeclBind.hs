{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CSPM.Evaluator.DeclBind (
    bindDecls, 
    --valuesForChannel, valuesForDataTypeClause,
) where

import Control.Monad
import Data.Maybe
import System.IO.Unsafe

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Exceptions
import {-# SOURCE #-} CSPM.Evaluator.Expr
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import Util.Annotated

-- | Given a list of declarations, returns a sequence of names bounds to
-- values that can be passed to 'addScopeAndBind' in order to bind them in
-- the current scope.
bindDecls :: [TCDecl] -> [(Name, EvaluationMonad Value)]
bindDecls ds = concatMap bindDecl ds

bindDecl :: TCDecl -> [(Name, EvaluationMonad Value)]
bindDecl (an@(An _ _ (FunBind n ms))) = [(n, collectArgs argGroupCount [])]
    where
        mss = map unAnnotate ms
        argGroupCount = head (map (\ (Match pss e) -> length pss) mss)
        collectArgs :: Int -> [[Value]] -> EvaluationMonad Value
        collectArgs 0 ass_ = do
            bss <- mapM (\ (Match pss e) -> do
                    let 
                        r = zipWith bindAll pss ass
                        b = and (map fst r)
                        binds = concatMap snd r
                    return ((b, binds), e)
                ) mss
            let
                rs :: [([(Name, Value)], TCExp)]
                rs = [(bs, e) | ((True, bs), e) <- bss]
            case rs of
                ((binds, exp):_) ->
                    addScopeAndBind binds (do
                        v <- eval exp
                        case v of
                            VProc p -> 
                                return $ VProc $ PProcCall (procId n ass) (Just p)
                            _ -> return v)
                _       -> -- throwError $ 
                    -- funBindPatternMatchFailureMessage (loc an) n ass
                        do
                            x <- mapM (\ (Match pss e) -> do
                                        let 
                                            r = zipWith bindAll pss ass
                                            b = and (map fst r)
                                            binds = concatMap snd r
                                        return (pss, ass, b, binds)
                                    ) mss
                            error $ show (ass) ++ "\n" ++ show x
            where
                ass = reverse ass_
        collectArgs n ass =
            return $ VFunction $ \ vs -> collectArgs (n-1) (vs:ass)
bindDecl (an@(An _ _ (PatBind p e))) =
    let
        nV = unsafePerformIO mkFreshInternalName
        extractor :: Name -> EvaluationMonad Value
        extractor n = do
            v <- lookupVar nV
            let 
                nvs = case bind p v of
                        (True, nvs) -> nvs
                        (False, _) -> (unsafePerformIO $ putStrLn $ "My pattern failed\n"++show n++"\n"++show nV++"\n\n"++show v) `seq` []
            return $ head [v | (n', v) <- nvs, n' == n]
    in (nV, eval e):[(fv, extractor fv) | fv <- freeVars p]
bindDecl (an@(An _ _ (Channel ns me))) =
    --let
        -- TODO: check channel values are in es
        --vs = case me of 
        --    Nothing -> []
        --    Just e -> do
        --        v <- eval e
        --        return $ evalTypeExprToList v
        -- ++[(internalNameForChannel n, VTuple (map VSet vs)) | n <- ns]
    [(n, return $ VChannel n) | n <- ns]
bindDecl (an@(An _ _ (DataType n cs))) =
    -- TODO: check data values are in e
    let
        bindClause (DataTypeClause nc _) =
            --(fromList [VDataType nc], [(nc, VDataType nc)]) -- (internalNameForDataTypeClause nc, VTuple [])
          [(nc, return $ VDataType nc)]  
        --bindClause (DataTypeClause nc (Just e)) =
        --    [(nc, VDataType nc)]
        --    --v <- eval e
            --let 
            --    sets = evalTypeExprToList v
            --    setOfValues = cartesianProduct (\vs -> VDot ((VDataType nc):vs)) sets
            --    binds = [(nc, VDataType nc)] -- (internalNameForDataTypeClause nc, VTuple (map VSet sets))
            --return (setOfValues, binds)
    in 
        --(sets, binds) <- mapAndUnzipM (bindClause . unAnnotate) cs
        concatMap bindClause (map unAnnotate cs)
        --let dt = (n, VSet (unions sets))
        --return $ dt:concat binds
bindDecl (an@(An _ _ (NameType n e))) =
    [(n, do
        v <- eval e
        return $ VSet $ evalTypeExpr v)]

bindDecl (an@(An _ _ (Assert _))) = []
bindDecl (an@(An _ _ (External ns))) = []
bindDecl (an@(An _ _ (Transparent ns))) = []

--internalNameForChannel, internalNameForDataTypeClause :: Name -> Name
--internalNameForChannel n = 
--    mkInternalName ("VALUE_TUPLE_CHANNEL_"++n)
--internalNameForDataTypeClause (Name n) = 
--    mkInternalName ("VALUE_TUPLE_DT_CLAUSE_"++n)

-- | Given a channel name returns a list of sets of values, one for each 
-- component of the channel. For example, if the channel took two components
-- A and {0..1} then this would return [A, {0..1}].
--valuesForChannel :: Name -> EvaluationMonad [ValueSet]
--valuesForChannel n = do
--    VTuple vs <- lookupVar (internalNameForChannel n)
--    return $ map (\(VSet s) -> s) vs

-- | Given a datatype clause name returns the sets of values, like 
-- 'valuesForChannel' does, but for channels.
--valuesForDataTypeClause :: Name -> EvaluationMonad [ValueSet]
--valuesForDataTypeClause n = do
--    VTuple vs <- lookupVar (internalNameForDataTypeClause n)
--    return $ map (\(VSet s) -> s) vs

evalTypeExpr :: Value -> ValueSet
evalTypeExpr (VSet s) = s
evalTypeExpr (VDot vs) = cartesianProduct VDot (map evalTypeExpr vs)
evalTypeExpr (VTuple vs) = cartesianProduct VTuple (map evalTypeExpr vs)

evalTypeExprToList :: Value -> [ValueSet]
evalTypeExprToList (VDot vs) = concatMap evalTypeExprToList vs
evalTypeExprToList v = [evalTypeExpr v]

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
