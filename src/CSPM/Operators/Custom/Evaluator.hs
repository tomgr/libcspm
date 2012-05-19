{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables,
    TypeSynonymInstances #-}
module CSPM.Operators.Custom.Evaluator (
    UnCompiledOperator(..),
) where

import Control.Monad.Reader
import Control.Monad.Trans
import CSPM.Compiler.Processes
import qualified CSPM.DataStructures.Names as CSPM
import qualified CSPM.DataStructures.Syntax as CSPM
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.Expr
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import qualified CSPM.Operators.Custom.OpSemDataStructures as OpSem
import qualified CSPM.Operators.Custom.Syntax as CSPM
import qualified Data.Sequence as Sq
import Util.List
import Util.PrettyPrint

data UnCompiledOperator =
    ProcOperator ProcOperator
    | EvUserOperator CSPM.Name [Value UnCompiledOperator] CSPM.CustomParserContext

type UnCompiledProc = Proc Sq.Seq UnCompiledOperator (ProcName UnCompiledOperator)

instance PrettyPrintable UnCompiledProc where
    prettyPrint (PUnaryOp (ProcOperator pop) e) =
        prettyPrint pop <> parens (prettyPrint e)
    prettyPrint (POp (EvUserOperator n vs _) s) | Sq.null s =
        text (show n) <> parens (list (fmap prettyPrint vs))

instance Evaluatable (CSPM.Process CSPM.Name) UnCompiledOperator where
    eval (CSPM.ReplicatedUserOperator opName opArgs opStmts opDefns) = do
        argGroups <- evalStmts (\ (VSet s) -> S.toList s) opStmts $ do
            vs <- mapM eval opArgs
            return [vs]
        let
            args = cartesianProduct argGroups
            OpSem.ReplicatedOperator op opArgs (basePats, baseCase) (inductiveVars, inductiveCase) _ =
                head [op | op@(OpSem.ReplicatedOperator opn _ _ _ _) <- 
                                OpSem.operators (CSPM.uncompiledOperators opDefns), 
                                opn == opName]
            patMatch (OpSem.PSet ps) vs = bindAll ps vs
            patMatch p v = bind p (VList v)
            evalRepOp :: [[Value UnCompiledOperator]] -> 
                EvaluationMonad UnCompiledOperator (Value UnCompiledOperator)
            evalRepOp args = do
                -- Check to see if the base case matches
                let bps = zipWith patMatch basePats args
                    (b, bs) = (and (map fst bps), concatMap snd bps)
                if b then
                    runOpSemEvaluator opDefns Nothing $ addToEnv bs $ opSemEval baseCase
                else do
                    let args' = map tail args
                        thisArgs = map head args
                        bs = zip inductiveVars thisArgs
                    VProc inductiveProc <- evalRepOp args'
                    runOpSemEvaluator opDefns (Just inductiveProc) $ 
                        addToEnv bs $ opSemEval inductiveCase
        evalRepOp args
    eval (CSPM.UserOperator opName opArgs opDefns) = do
        vs <- mapM eval opArgs
        return $ VProc $
            POp (EvUserOperator opName [v | v <- vs, not (isProc v)] opDefns) 
                (Sq.fromList [p | VProc p <- vs])

isProc (VProc p) = True
isProc _ = False

data OpSemState ops = OpSemState {
        operators :: CSPM.CustomParserContext,
        currentInductiveCase :: Maybe UnCompiledProc
    }

type OpSemEvalMonad ops = ReaderT (OpSemState ops) (EvaluationMonad ops)

runOpSemEvaluator :: CSPM.CustomParserContext -> Maybe UnCompiledProc ->
    OpSemEvalMonad UnCompiledOperator a -> EvaluationMonad UnCompiledOperator a
runOpSemEvaluator opSemDefn inductiveCase prog = do
    let chans = OpSem.channels (CSPM.uncompiledOperators opSemDefn)
        evalChan (OpSem.Channel n fs) = do
            fieldSets <- mapM (\ f -> runReaderT (opSemEval f) (OpSemState opSemDefn Nothing)) fs
            return $ [(n, VTuple [VChannel n, VInt $ length fieldSets, VList fieldSets])]
    binds <- mapM evalChan chans
    addScopeAndBind (concat binds) $ runReaderT prog (OpSemState opSemDefn inductiveCase)

opSemEval :: OpSem.Exp CSPM.Name -> OpSemEvalMonad UnCompiledOperator (Value UnCompiledOperator)
opSemEval (OpSem.OperatorApp opName es) = do
    vs <- mapM opSemEval es
    opDefns <- reader operators
    return $ VProc $
        POp (EvUserOperator opName [v | v <- vs, not (isProc v)] opDefns) 
                (Sq.fromList [p | VProc p <- vs])
opSemEval OpSem.InductiveCase = do
    Just p <- asks currentInductiveCase
    return $ VProc p
opSemEval (OpSem.Tuple es) = mapM opSemEval es >>= return . VTuple
opSemEval (OpSem.Var n) | CSPM.isNameDataConstructor n = do
    VTuple [dc, _, _] <- lift $ lookupVar n
    return dc
opSemEval (OpSem.Var n) = lift $ lookupVar n
opSemEval OpSem.Sigma = lift $ lookupVar $ builtInName "Events"
opSemEval OpSem.SigmaPrime = do
    events <- opSemEval OpSem.Sigma
    context <- asks operators
    let chans = OpSem.channels (CSPM.uncompiledOperators context)
        evalChan (OpSem.Channel n _) = do
            VTuple [_, _, VList fieldSets] <- lift $ lookupVar n
            return $ S.cartesianProduct (\ vs -> VDot (VChannel n : vs)) (map (\(VSet s) -> s) fieldSets)
    cs <- mapM evalChan chans
    return $ VSet $ S.unions cs
opSemEval (OpSem.SetComprehension es stmts) = do
    vs <- opSemStmtEval stmts $ mapM opSemEval es
    return $ VSet $ S.fromList vs
opSemEval (OpSem.Set es) = do
    vs <- mapM opSemEval es
    return $ VSet $ S.fromList vs
opSemEval (OpSem.SetMinus e1 e2) = do
    VSet s1 <- opSemEval e1
    VSet s2 <- opSemEval e2
    return $ VSet $ S.difference s1 s2
opSemEval (OpSem.Union e1 e2) = do
    VSet s1 <- opSemEval e1
    VSet s2 <- opSemEval e2
    return $ VSet $ S.union s1 s2
opSemEval (OpSem.Intersection e1 e2) = do
    VSet s1 <- opSemEval e1
    VSet s2 <- opSemEval e2
    return $ VSet $ S.intersection s1 s2
opSemEval (OpSem.Powerset e) = do
    VSet s <- opSemEval e
    return $ VSet $ S.powerset s
opSemEval (OpSem.ReplicatedUnion es) = do
    VSet ss <- opSemEval es
    return $ VSet $ S.unions $ map (\(VSet s) -> s) $ S.toList ss

addToEnv :: [(CSPM.Name, Value ops)] -> OpSemEvalMonad ops a -> OpSemEvalMonad ops a
addToEnv bs prog = do
    env <- ask
    lift $ addScopeAndBind bs (runReaderT prog env)

opSemStmtEval :: [OpSem.SideCondition CSPM.Name] ->
    OpSemEvalMonad UnCompiledOperator [Value UnCompiledOperator] -> 
    OpSemEvalMonad UnCompiledOperator [Value UnCompiledOperator]
opSemStmtEval cs inner = do
    let
        evalSideCondition [] = inner
        evalSideCondition (OpSem.SCGenerator p e : cs) = do
            VSet s <- opSemEval e
            vss <- mapM (\ v -> 
                        case bind p v of
                            (True, bs) -> addToEnv bs $ evalSideCondition cs
                            (False, _) -> return []
                    ) (S.toList s)
            return $ concat vss
        evalSideCondition (OpSem.Formula pf : cs) = do
            b <- opSemPropFormulaCheck pf
            if b then evalSideCondition cs else return []

    evalSideCondition cs
    
opSemPropFormulaCheck :: OpSem.Formula CSPM.Name -> OpSemEvalMonad UnCompiledOperator Bool
opSemPropFormulaCheck (OpSem.Member e1 e2) = do
    v1 <- opSemEval e1
    VSet v2 <- opSemEval e2
    return $ S.member v1 v2
opSemPropFormulaCheck (OpSem.Equals e1 e2) = do
    v1 <- opSemEval e1
    v2 <- opSemEval e2
    return $ compareValues v1 v2 == Just EQ
opSemPropFormulaCheck (OpSem.Subset e1 e2) = do
    VSet v1 <- opSemEval e1
    VSet v2 <- opSemEval e2
    return $ S.compareValueSets v1 v2 == Just LT
opSemPropFormulaCheck (OpSem.Not f1) = do
    b1 <- opSemPropFormulaCheck f1
    return $ not b1
opSemPropFormulaCheck (OpSem.And f1 f2) = do
    b1 <- opSemPropFormulaCheck f1
    b2 <- opSemPropFormulaCheck f2
    return $ b1 && b2
opSemPropFormulaCheck (OpSem.Or f1 f2) = do
    b1 <- opSemPropFormulaCheck f1
    b2 <- opSemPropFormulaCheck f2
    return $ b1 || b2
opSemPropFormulaCheck OpSem.PFalse = return False
opSemPropFormulaCheck OpSem.PTrue = return True

instance Bindable (OpSem.Pat CSPM.Name) ops where
    bind (OpSem.PVar n) v | CSPM.isNameDataConstructor n = 
        case v of
            VChannel n' -> (n == n', [])
            VDataType n' -> (n == n', [])
    bind (OpSem.PVar n) v = (True, [(n, v)])
    bind (OpSem.PTuple ps) (VTuple vs) = bindAll ps vs
    bind (OpSem.PSet ps) (VTuple vs) = bindAll ps vs
    bind _ _ = (False, [])
