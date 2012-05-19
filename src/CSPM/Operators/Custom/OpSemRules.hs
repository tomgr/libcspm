module CSPM.Operators.Custom.OpSemRules (
    compileOperators
) where
import Data.List
import Text.PrettyPrint.HughesPJ

import CSPM.DataStructures.Names
import CSPM.Operators.Custom.OpSemDataStructures
import CSPM.Operators.Custom.OpSemTypeChecker
import Util.PartialFunctions

-- *************************************************************************
-- Transformer
-- *************************************************************************
compileOperators :: OpSemDefinition -> [CompiledOp]
compileOperators opSemDefn = 
    let
        ops = operators opSemDefn
        transformOperator (op @ (Operator name args rules _)) =
            let
                cargs = args
                crules = map (transformInductiveRule ops op) rules
            in
                CompiledOp name cargs crules (discardableArguments crules)
    in
        map transformOperator [op | op @ (Operator _ _ _ _) <- ops]

transformInductiveRule :: [Operator] -> Operator -> InductiveRule Name -> CompiledRule
transformInductiveRule ops op (InductiveRule pre post conditions) =
    let     
        (Performs (OperatorApp name currentOperatorArgs) resultingEvent 
                        (OperatorApp newOperator newArgs)) = post
        -- All arguments of the operator of type proc
        procArgs = [pr | Var pr <- currentOperatorArgs, 
                            typeIsProc (getArgType op pr)]
        -- Map from proc |-> proc; if resultingProcs p' = p then it means
        -- p -> p' by some rule
        resultingProcs = 
            [(pr', pr) | Performs (Var pr) _ (Var pr') <- pre]
        -- Map from proc to proc; resulting proc is an argument of the current
        -- operator (and thus has an index)
        procMap = resultingProcs 
            ++ (identityFunction (procArgs \\ functionImage resultingProcs))
        
        currentOnProcs = map (getId op) (onProcs op)
        currentOffProcs = map (getId op) (offProcs op)
        
        resultingOperator @ (Operator _ resultingOperatorArgs _ _) = 
            getOp ops newOperator
        resultingOperatorName = (name,
                [exp | (exp, (_, t)) <- zip newArgs resultingOperatorArgs, 
                        not (typeIsProc t)])
            where name = newOperator
        resultingOperatorProcessArgs = 
            zip [0..] [arg | (Var arg, (_, TOnProcess _)) 
                                <- zip newArgs resultingOperatorArgs]
            ++ zip [(-length (offProcs resultingOperator))..(-1)] 
                            [arg | (Var arg, (_, TOffProcess _)) 
                                <- zip newArgs resultingOperatorArgs]
        -- PartialFunction from arg id of resulting op to argument value
        resultingOnProcs = 
            map (\ pid -> (pid, apply resultingOperatorProcessArgs pid))
                (map (getId resultingOperator) (onProcs resultingOperator))
        -- PartialFunction from arg id of resulting op to argument value
        resultingOffProcs =
            map (\ pid -> (pid, apply resultingOperatorProcessArgs pid))
                (map (getId resultingOperator) (offProcs resultingOperator))
            
        -- If procMap pid = pid' then it means the pid of the new operator
        -- is pid' of the old process
        onProcMap =
            [(pid, getId op (apply procMap pr')) | (pid,pr') <- resultingOnProcs]
        -- Map from pid to [0..]; if newProcMap pid = n it means that the 
        -- nth turned on argument of the resulting operator is the pid off
        -- argument of the current operator
        newProcMap = 
            zip [pid' | (pid, pid') <- onProcMap, elem pid' currentOffProcs] [0..]
        
        phi = [(getId op n, ev) | Performs (Var n) ev (Var n') <- pre]
        psi = [(pid, if pid' < 0 then apply newProcMap pid' else pid')
                | (pid, pid') <- onProcMap]
        f = invert newProcMap
        chi = 
            [(pid, getId op (apply procMap pr')) | (pid,pr') <- resultingOffProcs]
        discards = 
            currentOnProcs
            \\ (map (getId op) (mapPF procMap (functionImage resultingOnProcs)))
        generators = map transformSideCondition conditions
        boundVars = varsBound conditions
    in
{-      error (
            show resultingOperator ++"\n\n"
            ++ show resultingOperatorProcessArgs ++"\n\n"
            ++ show resultingOnProcs ++"\n\n"
            ++ show resultingOffProcs ++"\n\n"
            ++ show procMap++"\n\n"
            ++ show phi ++"\n\n"
            ++ show psi ++"\n\n"
            
            ++show resultingOperatorName++"\n\n"
            ++show resultingEvent++"\n\n"           
            ++show f++"\n\n"
            ++show chi++"\n\n"
            ++show discards++"\n\n"
            ++show generators++"\n\n"
        )
-}
        CompiledRule phi resultingEvent resultingOperatorName f psi 
            chi discards boundVars generators
            
transformSideCondition :: SideCondition Name -> Stmt Name
transformSideCondition (SCGenerator p e) = Generator p e
transformSideCondition (Formula f) = PropFormula f

getOp :: [Operator] -> Name -> Operator
getOp ops n =
    head [op | (op @ (Operator n' _ _ _)) <- ops, n==n']
    
getArgType :: Operator -> Name -> Type
getArgType (Operator _ args _ _) n = 
    head [t | (n', t) <- args, n==n']

-- A process is off iff there exists no rule that allows it to perform any
-- visible event
offProcs :: Operator -> [Name]
offProcs (op @ (Operator n args rules _)) = 
    [pr | (pr, TOffProcess _) <- args]

onProcs :: Operator -> [Name]
onProcs (op @ (Operator _ args rules _)) = 
    [pr | (pr, TOnProcess _) <- args]

getId :: Operator -> Name -> ProcId
getId op n =
    head ([id | (n', id) <- zip (onProcs op) [0..], n==n']
            ++[id | (n', id) <- zip (offProcs op) [-(length (offProcs op))..(-1)], 
                    n==n']
    )

-- *********************************************************************
-- Discardable Arguments
-- *********************************************************************
discardableArguments :: [CompiledRule] -> [ProcId]
discardableArguments rules = 
    (nub . concat) 
        [discards | CompiledRule _ _ _ _ _ _ discards _ _ <- rules]
