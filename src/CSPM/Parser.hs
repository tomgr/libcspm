module CSPM.Parser (
    parseFile, parseInteractiveStmt, parseExpression, parseStringAsFile,
    
    ParseMonad, runParser,
) 
where

import CSPM.DataStructures.Syntax
import CSPM.Parser.Monad
import CSPM.Parser.Parser
import Util.Annotated

-- | Parse as string as an 'PInteractiveStmt'.
parseInteractiveStmt :: String -> ParseMonad PInteractiveStmt
parseInteractiveStmt str =
    pushFileContents "<interactive>" str >> parseInteractiveStmt_

-- | Parses a string as an 'PExp'.
parseExpression :: String -> ParseMonad PExp
parseExpression str = 
    pushFileContents "<interactive>" str >> parseExpression_

-- | Parse the given file, returning the parsed 'PModule's.
parseFile :: String -> ParseMonad [PModule]
parseFile fname = do
    decls <- pushFile fname parseFile_
    return [An Unknown dummyAnnotation (GlobalModule decls)]

-- | Parse a string, as though it were an entire file, returning the parsed
-- 'PModule's.
parseStringAsFile :: String -> ParseMonad [PModule]
parseStringAsFile str = do
    decls <- pushFileContents "<interactive>" str >> parseFile_
    return [An Unknown dummyAnnotation (GlobalModule decls)]
