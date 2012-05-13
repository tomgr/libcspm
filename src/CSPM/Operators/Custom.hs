{-# LANGUAGE MultiParamTypeClasses #-}
module CSPM.Operators.Custom ( ) where

import CSPM
import CSPM.Operators.Custom.CSPMParser
import CSPM.Operators.Custom.Evaluator
import CSPM.Operators.Custom.OpSemDataStructures
import CSPM.Operators.Custom.OpSemParser
import CSPM.Operators.Custom.OpSemTypeChecker
import CSPM.Operators.Custom.Syntax
import CSPM.Parser
import CSPM.Prelude
import Data.IORef

-- TODO: remove this hack

instance BuiltInFunctions UnCompiledOperator where
    compressionOperator = ProcOperator
    extraBuiltInsDefinitions = []

instance Parseable OpSemDefinition Process where
    parseInteractiveStmt = error "unsupported"
    parseExpression = error "unsupported"
    parseFile opSemDefn dirName fileName = parseCSPMFile fileName opSemDefn
    parseStringAsFile = error "unsupported"

instance CSPLike OpSemDefinition Process UnCompiledOperator where

newParserContext :: String -> IO OpSemDefinition
newParserContext opSemFile = parseOpSemFile opSemFile >>= typeCheckOperators
