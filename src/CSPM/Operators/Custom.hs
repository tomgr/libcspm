{-# LANGUAGE MultiParamTypeClasses #-}
module CSPM.Operators.Custom (
    module CSPM.Operators.Custom.CSPMParser,
    module CSPM.Operators.Custom.Evaluator,
    module CSPM.Operators.Custom.OpSemDataStructures,
    module CSPM.Operators.Custom.OpSemParser,
    module CSPM.Operators.Custom.OpSemTypeChecker,
    module CSPM.Operators.Custom.Syntax,
    newParserContext,
) where

import CSPM
import CSPM.Operators.Custom.CSPMParser
import CSPM.Operators.Custom.Evaluator
import CSPM.Operators.Custom.OpSemDataStructures
import CSPM.Operators.Custom.OpSemParser
import CSPM.Operators.Custom.OpSemTypeChecker
import CSPM.Operators.Custom.OpSemRules
import CSPM.Operators.Custom.Syntax
import CSPM.Parser
import CSPM.Prelude
import System.FilePath

instance BuiltInFunctions UnCompiledOperator where
    compressionOperator = UnCOp . ProcOperator
    extraBuiltInsDefinitions = []

instance Parseable CustomParserContext Process where
    parseInteractiveStmt = error "unsupported"
    parseExpression = error "unsupported"
    parseFile opSemDefn dirName fileName =
        parseCSPMFile (joinPath [dirName, fileName]) opSemDefn
    parseStringAsFile opSemDefn dirName string = stringParser string opSemDefn

instance CSPLike CustomParserContext Process UnCompiledOperator where

newParserContext :: String -> IO CustomParserContext
newParserContext opSemFile = do
    ops <- parseOpSemFile opSemFile >>= typeCheckOperators
    let cops = compileOperators ops
    return $ CustomParserContext ops cops
