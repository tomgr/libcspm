{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances #-}
module CSPM.Operators.Custom.Evaluator (
    UnCompiledOperator(..), UnCompiledProc, UnCompiledOperatorRules,
    EvaluatedOperator(..), ProcState(..), OperatorRules(..), OperatorFormat(..),
    OperatorRule(..), UnCompiledEvent,
) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import CSPM.Compiler.Processes
import qualified CSPM.DataStructures.Names as CSPM
import qualified CSPM.DataStructures.Syntax as CSPM
import CSPM.Evaluator.BuiltInFunctions
import CSPM.Evaluator.Expr
import CSPM.Evaluator.Monad hiding (gets, modify)
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import qualified CSPM.Operators.Custom.OpSemDataStructures as OpSem
import qualified CSPM.Operators.Custom.Syntax as CSPM
import Data.Array
import qualified Data.Map as M
import qualified Data.Sequence as Sq
import Util.Exception
import Util.List
import qualified Util.PartialFunctions as PF
import Util.PrettyPrint

-- Aliases, for ease
type UnCompiledEvent = Event UnCompiledOperator
newtype UnCompiledOperator = UnCOp {
        unCompiledOperator :: EvaluatedOperator UnCompiledOperator (Maybe UnCompiledEvent)
    }

type UnCompiledProc = Proc Sq.Seq UnCompiledOperator (ProcName UnCompiledOperator)
type UnCompiledOperatorRules = OperatorRules (Maybe UnCompiledEvent)
type PartiallyEvaluatedOperator = (CSPM.Name, [Value UnCompiledOperator])

data EvaluatedOperator op ev =
    ProcOperator ProcOperator
    | EvUserOperator {
        evOperatorParserContext :: CSPM.CustomParserContext,
        evOperatorName :: CSPM.Name,
        evOperatorArguments :: [Value op],
        evOperatorRules :: OperatorRules ev
    }

data ProcState = On | Off | Terminated
    deriving (Eq, Show)

data OperatorRules ev = OperatorRules {
        formats :: Array Int (OperatorFormat ev),
        initialFormat :: Int
    }
    deriving Show
data OperatorFormat ev =
    OperatorFormat {
        operatorRules :: [OperatorRule ev],
        operatorProcStates :: Array Int ProcState
    }
    | IdentityFormat {
        -- | The identifier of the process to do the identity mapping over.
        processId :: Int,
        operatorProcStates :: Array Int ProcState
    }
    deriving Show
data OperatorRule ev = OperatorRule {
        operatorRulePres :: Array Int ev,
        operatorRuleResult :: ev,
        operatorRuleResultFormat :: Int
    }
    deriving Show

instance PrettyPrintable UnCompiledProc where
    prettyPrint (PUnaryOp (UnCOp (ProcOperator pop)) e) =
        prettyPrint pop <> parens (prettyPrint e)
    prettyPrint (POp (UnCOp (EvUserOperator _ n vs _)) s) =
        text (show n) <> parens (list (fmap prettyPrint vs))
    prettyPrint (PProcCall pn p) = prettyPrint pn
instance Evaluatable (CSPM.Process CSPM.Name) UnCompiledOperator where
    eval (CSPM.ReplicatedUserOperator opName opArgs opStmts opDefns) = do
        argGroups <- evalStmts (\ (VSet s) -> S.toList s) opStmts $ do
            vs <- mapM eval opArgs
            return [vs]
        let
            makeRows [] = [[] | i <- [0..length opArgs-1]]
            makeRows (xs:xss) = zipWith (\x ys -> x:ys) xs (makeRows xss)
            args = makeRows argGroups
            OpSem.ReplicatedOperator op opArgs (basePats, baseCase) (inductiveVars, inductiveCase) _ =
                head [op | op@(OpSem.ReplicatedOperator opn _ _ _ _) <- 
                                OpSem.operators (CSPM.uncompiledOperators opDefns), 
                                opn == opName]
            patMatch (OpSem.PSet [p]) [v] = bind p v
            patMatch (OpSem.PSet []) [] = (True, [])
            patMatch (OpSem.PSet _) _ = (False, [])
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
                        makeInductiveArg name valueList =
                            case valueList of
                                [] -> [(name, VSet (S.fromList []))]
                                (VProc p :_) -> []
                                vs -> [(name, VSet (S.fromList vs))]
                        bs = zip (map fst opArgs) thisArgs ++ 
                                concat (zipWith makeInductiveArg inductiveVars args')
                    VProc inductiveProc <- evalRepOp args'
                    runOpSemEvaluator opDefns (Just inductiveProc) $ 
                        addToEnv bs $ opSemEval inductiveCase
        evalRepOp args
    eval (CSPM.UserOperator opName opArgs opDefns) = do
        vs <- mapM eval opArgs
        let pop = (opName, vs)
        op <- runOpSemEvaluator opDefns Nothing $ opSemEvalPartialOp pop
        let p = VProc $ POp (UnCOp $ EvUserOperator opDefns opName vs op) 
                            (Sq.fromList [p | VProc p <- vs])
        return p

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
    let pop = (opName, vs)
    op <- opSemEvalPartialOp pop
    return $ VProc $ POp (UnCOp $ EvUserOperator opDefns opName vs op) 
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

opSemEvalEvent :: OpSem.Event CSPM.Name -> 
    OpSemEvalMonad UnCompiledOperator (Event UnCompiledOperator)
opSemEvalEvent (OpSem.Event n) | CSPM.isNameDataConstructor n = do
    VTuple [dc, _, _] <- lift $ lookupVar n
    return $ UserEvent dc
opSemEvalEvent (OpSem.Event n) = do
    v <- lift $ lookupVar n
    return $ UserEvent v
opSemEvalEvent (OpSem.ChanEvent n es) = do
    VTuple [dc, _, _] <- lift $ lookupVar n
    vs <- mapM opSemEval es
    return $ UserEvent $ VDot (dc:vs)
opSemEvalEvent OpSem.Tau = return Tau

addToEnv :: [(CSPM.Name, Value ops)] -> OpSemEvalMonad ops a -> OpSemEvalMonad ops a
addToEnv bs prog = do
    env <- ask
    lift $ addScopeAndBind bs (runReaderT prog env)

opSemStmtEval :: [OpSem.SideCondition CSPM.Name] ->
    OpSemEvalMonad UnCompiledOperator [a] -> 
    OpSemEvalMonad UnCompiledOperator [a]
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
    bind (OpSem.PSet []) (VSet s) | S.empty s = (True, [])
    bind (OpSem.PSet [p]) (VSet s) =
        case S.singletonValue s of
            Just v -> bind p v
            Nothing -> (False, [])
    bind _ _ = (False, [])

opSemEvalPartialOp :: PartiallyEvaluatedOperator -> 
    OpSemEvalMonad UnCompiledOperator UnCompiledOperatorRules
opSemEvalPartialOp (opName, opActArgs) = do
    CSPM.CustomParserContext opDefns _ <- asks operators
    let op = head [op | op@(OpSem.Operator opN _ _ _) <- OpSem.operators opDefns, opName == opN]
        isProc (VProc p) = True
        isProc _ = False
        opNonProcArgs = [v | v <- opActArgs, not (isProc v)]
    (thisId, st) <- 
        runStateT (opSemEvalCompiledOperator opName opNonProcArgs)
            (initialEvaluatorState (map snd (OpSem.opArgs op)))
    return $ OperatorRules (array (0, nextFormatId st-1) (compiledFormats st)) thisId

data OperatorEvaluatorState = OperatorEvaluatorState {
        nextFormatId :: Int,
        compiledOperators :: M.Map (PF.PartialFunction Int Int, CSPM.Name, [Value UnCompiledOperator]) Int,
        compiledFormats :: [(Int, OperatorFormat (Maybe UnCompiledEvent))],
        -- | Map from process id of the current operator to the actual process
        -- id of the original operator.
        currentProcessMap :: PF.PartialFunction Int Int,
        currentProcessStates :: PF.PartialFunction Int ProcState,
        currentOffProcessMap :: PF.PartialFunction Int Int
    }

initialEvaluatorState :: [OpSem.Type] -> OperatorEvaluatorState
initialEvaluatorState opArgs =
    let procArgs = [t | t <- opArgs, isProcType t]
        onProcArgs = [0 | OpSem.TOnProcess _ <- procArgs]
        isProcType (OpSem.TOnProcess _) = True
        isProcType (OpSem.TOffProcess _) = True
        isProcType _ = False
        procState (OpSem.TOnProcess _) = On
        procState (OpSem.TOffProcess _) = Off
    in OperatorEvaluatorState 0 M.empty []
            (PF.identityFunction [0..length onProcArgs-1])
            (zip [0..] (map procState procArgs))
            (zip [(-1),(-2)..] [pid | (pid, OpSem.TOffProcess _) <- zip [0..] procArgs])

type OperatorCompilerMonad a =
    StateT OperatorEvaluatorState (OpSemEvalMonad UnCompiledOperator) a

opSemEvalCompiledOperator :: 
    CSPM.Name ->
    [Value UnCompiledOperator] ->
    OperatorCompilerMonad Int
opSemEvalCompiledOperator opName opArgs = do
    seenOps <- gets compiledOperators
    pmap <- gets currentProcessMap
    CSPM.CustomParserContext opDefns copDefns <- lift $ asks operators
    case M.lookup (pmap, opName, opArgs) seenOps of
        Just fmtId -> return fmtId
        Nothing -> do
            let compiledOp = head [cop | cop <- copDefns, OpSem.copFriendlyName cop == opName]
                unCompOp = head [op | op@(OpSem.Operator opn _ _ _) <- OpSem.operators opDefns, opn == opName]
                isProc (OpSem.TOnProcess _) = True
                isProc (OpSem.TOffProcess _) = True
                isProc _ = False
                binds = zip [n | (n, t) <- OpSem.opArgs unCompOp, not (isProc t)] opArgs
            thisFmtId <- gets nextFormatId
            modify (\ st -> st { 
                    compiledOperators =
                        M.insert (pmap, opName, opArgs) thisFmtId (compiledOperators st),
                    nextFormatId = nextFormatId st + 1
                })

            thisFmt' <- 
                if show opName == "Identity" then
                    return $ IdentityFormat (PF.apply pmap 0)
                else do
                    -- Evaluate our compiled rules
                    rs <- mapM (opSemEvalRule thisFmtId binds) (OpSem.copRules compiledOp)
                    procStates <- gets currentProcessStates
                    let pcount = length procStates
                        makeTauPromotionRule (pid, On) =
                            let pres = (array (0, pcount-1)) $ 
                                    map (\pid' -> (pid', if pid == pid' then Just Tau else Nothing)) [0..pcount-1]
                            in [OperatorRule pres (Just Tau) thisFmtId]
                        makeTauPromotionRule _ = []
                    return $ OperatorFormat (concat rs ++ concatMap makeTauPromotionRule procStates)

            procStates <- gets currentProcessStates
            let thisFmt = thisFmt' (array (0, length procStates-1) procStates)

            modify (\st -> st { compiledFormats = (thisFmtId, thisFmt) : compiledFormats st})
            return thisFmtId

opSemEvalRule ::
    Int ->
    [(CSPM.Name, Value UnCompiledOperator)] ->
    OpSem.CompiledRule ->
    OperatorCompilerMonad [OperatorRule (Maybe UnCompiledEvent)]
opSemEvalRule thisFmtId binds compiledRule = do
    unCompRules <- 
        lift $ addToEnv binds $ opSemStmtEval (OpSem.crGenerators compiledRule) $ do
                preEvents <- mapM (\ (pid, evexp) -> do
                    ev <- opSemEvalEvent evexp
                    return (pid, ev)) (OpSem.crComponentEvents compiledRule)
                resultEvent <- opSemEvalEvent (OpSem.crResultEvent compiledRule)
                let (resultName, resultArgExps) =
                        OpSem.crResultingOperatorName compiledRule
                resultOperatorArgs <- mapM opSemEval resultArgExps
                return [(preEvents, resultEvent, resultName, resultOperatorArgs)]

    pstates <- gets currentProcessStates
    pmap <- gets currentProcessMap
    offpmap <- gets currentOffProcessMap

    let pcount = length pmap
        compile (preEvents, resultEvent, resultName, resultOpArgs) = do
            let pres = array (0, pcount-1) $ map (\pid -> (pid, PF.safeApply preEvents pid)) [0..pcount-1]
                currentOnProcs = [pid | (pid, On) <- pstates]
                currentOnProcCount = length currentOnProcs
                discardedProcs = OpSem.crDiscards compiledRule
                newProcCount = currentOnProcCount-length discardedProcs+length turnedOnProcs
                -- | The process id (relative to the CURRENT operator) of all
                -- the turned on procs.
                turnedOnProcs = map (PF.apply offpmap . snd) (OpSem.crF compiledRule)
                -- | Returns the process id for the resulting operator. That is
                -- getProc pid returns the process id of the component that is
                -- the pid^th component of the new operator.
                getProc :: Int -> Int
                getProc pid = 
                    let v = PF.apply (OpSem.crPsi compiledRule) pid
                    in if v < currentOnProcCount then v
                        else PF.apply offpmap (PF.apply (OpSem.crF compiledRule) (v - currentOnProcCount))
                pmap' = map (\pid -> (pid, getProc pid)) [0..newProcCount-1]
                pstates' :: PF.PartialFunction Int ProcState
                pstates' = map (\ (cid, st) ->
                                    if cid `elem` discardedProcs then
                                        (cid , Terminated)
                                    else if cid `elem` turnedOnProcs then
                                        (cid, On)
                                    else (cid, st)) pstates
                offpmap' = PF.composeFunctions offpmap (OpSem.crChi compiledRule)
            resultFmtId <- updateProcessMap offpmap' pmap' pstates' $
                opSemEvalCompiledOperator resultName resultOpArgs
            return $ OperatorRule pres (Just resultEvent) resultFmtId
    
    mapM compile unCompRules

updateProcessMap :: 
    PF.PartialFunction Int Int ->
    PF.PartialFunction Int Int -> 
    PF.PartialFunction Int ProcState ->
    OperatorCompilerMonad a -> OperatorCompilerMonad a
updateProcessMap offpmap pmap pstates prog = do
    pmap0 <- gets currentProcessMap
    pstates0 <- gets currentProcessStates
    offpmap0 <- gets currentOffProcessMap
    modify (\st -> st { 
        currentProcessMap = pmap,
        currentProcessStates = pstates,
        currentOffProcessMap = offpmap }) 
    a <- prog
    modify (\st -> st {
        currentProcessMap = pmap0,
        currentProcessStates = pstates0,
        currentOffProcessMap = offpmap0 }) 
    return a
