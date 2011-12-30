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
import Util.Exception
import qualified Util.List as UL
import Util.Monad

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
                _       -> throwError $ 
                    funBindPatternMatchFailureMessage (loc an) n ass
            where
                ass = reverse ass_
        collectArgs n ass =
            return $ VFunction $ \ vs -> collectArgs (n-1) (vs:ass)
bindDecl (an@(An _ _ (PatBind p e))) =
    let
        -- We put p inside the unsafePerformIO simply to prevent it being
        -- lifed out of the lambda (and thus only being performed once).
        nV = unsafePerformIO (p `seq` mkFreshInternalName)
        {-# NOINLINE nV #-}
        extractor :: Name -> EvaluationMonad Value
        extractor n = do
            v <- lookupVar nV
            let 
                nvs = case bind p v of
                        (True, nvs) -> nvs
                        (False, _) -> throwError $ 
                            patternMatchFailureMessage (loc an) p v
            return $ head [v | (n', v) <- nvs, n' == n]
    in (nV, eval e):[(fv, extractor fv) | fv <- freeVars p]
bindDecl (an@(An _ _ (Channel ns me))) =
    let
        -- TODO: check channel values are in es
        mkChan :: Name -> EvaluationMonad Value
        mkChan n = do
            vss <- case me of
                Just e -> eval e >>= return . evalTypeExprToList
                Nothing -> return []
            
            let arity = fromIntegral (length vss)
            return $ VTuple [VDot [VChannel n], VInt arity, VList (map VList vss)]
    in [(n, mkChan n) | n <- ns]
bindDecl (an@(An _ _ (DataType n cs))) =
    -- TODO: check data values are in e
    let
        mkDataTypeClause :: DataTypeClause Name -> (Name, EvaluationMonad Value)
        mkDataTypeClause (DataTypeClause nc me) = (nc, do
            vss <- case me of
                Just e -> eval e >>= return . evalTypeExprToList
                Nothing -> return []
            let arity = fromIntegral (length vss)
            return $ VTuple [VDot [VDataType nc], VInt arity, VList (map VList vss)])
        computeSetOfValues :: EvaluationMonad Value
        computeSetOfValues =
            let 
                mkSet nc = do
                    VTuple [_, _, VList fields] <- lookupVar nc
                    let cp = UL.cartesianProduct (map (\(VList vs) -> vs) fields)
                    return $ map (\vs -> VDot ((VDataType nc):vs)) cp
            in do
                vs <- concatMapM mkSet [nc | DataTypeClause nc _ <- map unAnnotate cs]
                return $ VSet (fromList vs)
    in (n, computeSetOfValues):(map mkDataTypeClause (map unAnnotate cs))
bindDecl (an@(An _ _ (NameType n e))) =
    [(n, do
        v <- eval e
        return $ VSet $ evalTypeExpr v)]

bindDecl (an@(An _ _ (Assert _))) = []
bindDecl (an@(An _ _ (External ns))) = []
bindDecl (an@(An _ _ (Transparent ns))) = []

evalTypeExpr :: Value -> ValueSet
evalTypeExpr (VSet s) = s
evalTypeExpr (VDot vs) = cartesianProduct VDot (map evalTypeExpr vs)
evalTypeExpr (VTuple vs) = cartesianProduct VTuple (map evalTypeExpr vs)

evalTypeExprToList :: Value -> [[Value]]
evalTypeExprToList (VDot vs) = concatMap evalTypeExprToList vs
evalTypeExprToList v = [toList (evalTypeExpr v)]

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
