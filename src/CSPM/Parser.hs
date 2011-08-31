module CSPM.Parser (
    parseFile, parseInteractiveStmt, parseExpression, parseStringAsFile,
    
    ParseMonad, runParser,
) 
where

import CSPM.DataStructures.Syntax
import CSPM.Parser.Monad
import CSPM.Parser.Parser
import Util.Annotated

-- External Interface
parseInteractiveStmt :: String -> ParseMonad PInteractiveStmt
parseInteractiveStmt str =
    pushFileContents "<interactive>" str >> parseInteractiveStmt_

parseExpression :: String -> ParseMonad PExp
parseExpression str = 
    pushFileContents "<interactive>" str >> parseExpression_

parseFile :: String -> ParseMonad [PModule]
parseFile fname = do
    decls <- pushFile fname parseFile_
    return [An Unknown dummyAnnotation (GlobalModule decls)]

parseStringAsFile :: String -> ParseMonad [PModule]
parseStringAsFile str = do
    decls <- pushFileContents "<interactive>" str >> parseFile_
    return [An Unknown dummyAnnotation (GlobalModule decls)]
