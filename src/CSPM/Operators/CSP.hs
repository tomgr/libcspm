{-# LANGUAGE MultiParamTypeClasses #-}
module CSPM.Operators.CSP (
    module CSPM.Operators.CSP.Desugar,
    module CSPM.Operators.CSP.Evaluator,
    module CSPM.Operators.CSP.Parser,
    module CSPM.Operators.CSP.PrettyPrinter,
    module CSPM.Operators.CSP.Processes,
    module CSPM.Operators.CSP.Renamer,
    module CSPM.Operators.CSP.Syntax,
    module CSPM.Operators.CSP.TypeChecker,
) where

import CSPM
import CSPM.Operators.CSP.Desugar
import CSPM.Operators.CSP.Evaluator
import CSPM.Operators.CSP.PrettyPrinter
import CSPM.Operators.CSP.Renamer
import CSPM.Operators.CSP.Parser
import CSPM.Operators.CSP.Processes
import CSPM.Operators.CSP.Syntax
import CSPM.Operators.CSP.TypeChecker

instance CSPLike CSPProcess UnCompiledCSPOp where
