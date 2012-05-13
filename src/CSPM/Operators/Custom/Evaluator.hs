{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module CSPM.Operators.Custom.Evaluator (
    UnCompiledOperator(..),
) where

import CSPM.Compiler.Processes
import qualified CSPM.DataStructures.Names as CSPM
import CSPM.Evaluator.Expr
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import qualified CSPM.Operators.Custom.OpSemDataStructures as OpSem
import qualified CSPM.Operators.Custom.Syntax as CSPM
import qualified Data.Sequence as Sq
import Util.PrettyPrint

data UnCompiledOperator =
    ProcOperator ProcOperator
    | UserOperator OpSem.Name [Value UnCompiledOperator] OpSem.OpSemDefinition

type UnCompiledProc = Proc Sq.Seq UnCompiledOperator (ProcName UnCompiledOperator)

instance PrettyPrintable UnCompiledProc where
    prettyPrint (PUnaryOp (ProcOperator pop) e) =
        prettyPrint pop <> parens (prettyPrint e)
    prettyPrint (POp (UserOperator n vs _) s) | Sq.null s =
        text (show n) <> parens (list (fmap prettyPrint vs))

instance Evaluatable (CSPM.Process CSPM.Name) UnCompiledOperator where
    --eval (CSPM.ReplicatedUserOperator opName opArgs opStmts opDefns) = do
    --    ps <- evalStmts (\ (VSet s) -> S.toList s) opStmts $ do
    --        vs <- mapM eval opArgs
    --        return [UserOperator opName vs opDefns]
    --    return $ map (\ op -> VProc op []) ps
    eval (CSPM.UserOperator opName opArgs opDefns) = do
        vs <- mapM eval opArgs
        return $ VProc $ POp (UserOperator opName vs opDefns) Sq.empty
