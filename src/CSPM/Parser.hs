-- | A module that parses CSPM modules.
-- 
-- The biggest problem with parsing CSPM is that > means both greater than
-- and end sequence. For example, consider @\<x | x > 2@ and @\<x | x > 2 >@.
-- Both of these are syntactically valid, but the parser has no way of deducing
-- whether or not the first @>@ it sees is an end sequence or a greater than.
--
-- Clearly, without an arbitrary lookahead it is not possible, in general, to
-- solve this. Hence, we go for the 'shortest sequence' approach, in which
-- whenever a @>@ is seen whilst a list is currently open, we assume that it
-- closes the list.
--
-- FDR has a slightly more sophisticated scheme, but this depends on the fact
-- that Bison happens to generate a lazy lookahead in this case, whereas Happy
-- is never lazy in its lookahead token. In particular, FDR has a sequence
-- stack of integers. The top value means the number of open sequence brackets
-- it has seen so far. It lexes @\<@ as normal, but whenever it decides that a
-- @\<@ token is a open sequence token, adds one to the current top of the
-- sequence stack. Then, whenever a @>@ token is discovered it checks to see 
-- if the top value is non-zero, and if so lexes endseq, otherwise lexes gt. 
-- It then decrements the value of the top of the sequence stack in the 
-- parser. It uses a stack to allow it to open new contexts within parenthesis.
--
-- Laziness is required when scanning something like @\<1> > \<1>@ as, if it is
-- not lazy then the second @>@ will be lexed just before the first @>@ is dealt
-- with.
--
-- Instead, we decrement the top of the sequence stack within the lexer instead.
-- I don't think this will cause a change in behaviour, as if the lexer lexes
-- a _endseq token then it will definitely decrement the counter later (and
-- it has already checked to make sure it is non-zero). However, we do keep
-- the decision about @LT@ being @openseq@ or @\<@ in the parser as this can 
-- decide when this is the case (e.g. @x \<@ means it must be a @LT@). 
--
-- The above is a problem in conjunction with nested brackets. For example,
-- consider @\<(0,1) | x>@; when we parse this we would take the @(@ into the 
-- lookahead before we actually parse @\<@, meaning that when we push the @1@
-- onto the sequence stack it actually goes onto the new top entry that is 
-- popped off after @)@ is processed. Therefore, the @>@ will be parsed as a 
-- @GT@. To solve this we make sure that whenever we pop off the sequence 
-- stack we add any remaining open sequences onto the new top element. Clearly 
-- this would be unsafe if we were relying on this for parsing, but as we are 
-- not this should be fine and should cause no further ambiguities.
--
-- It should be noted the above is a big hacky workaround, but I see no 
-- alternative without resorting to an arbitrary lookahead parser (like parsec)
-- and its obvious inefficiency.
module CSPM.Parser (
    parseFile, parseInteractiveStmt, parseExpression, parseStringAsFile,
    parseStringsAsFile, filesRequiredByFile,
    
    ParseMonad, runParser,
) 
where

import qualified Data.ByteString.Char8 as B
import System.FilePath

import CSPM.DataStructures.Syntax
import CSPM.Parser.Monad
import CSPM.Parser.Parser

-- | Parse as string as an 'PInteractiveStmt'.
parseInteractiveStmt :: String -> ParseMonad PInteractiveStmt
parseInteractiveStmt str =
    pushFileContents "<interactive>" (B.pack str) >> parseInteractiveStmt_

-- | Parses a string as an 'PExp'.
parseExpression :: String -> ParseMonad PExp
parseExpression str = 
    pushFileContents "<interactive>" (B.pack str) >> parseExpression_

-- | Parse the given file, returning the parsed 'PModule's.
parseFile :: String -> ParseMonad PCSPMFile
parseFile fname = pushFile fname parseFile_

-- | Parse a string, as though it were an entire file, returning the parsed
-- 'PModule's.
parseStringAsFile :: String -> ParseMonad PCSPMFile
parseStringAsFile str =
    pushFileContents "<interactive>" (B.pack str) >> parseFile_

parseStringsAsFile :: String -> [(String, B.ByteString)] -> ParseMonad PCSPMFile
parseStringsAsFile rootFile files = do
    mapM_ (\ (f, c) -> addFileContents f c) files
    parseFile rootFile

-- | Returns the list of files that have been loaded so far.
filesRequiredByFile :: String -> IO [String]
filesRequiredByFile fp = 
    let (dir, fname) = splitFileName fp
    in runParser (do
        parseFile fname
        st <- getParserState
        return $! loadedFiles st) dir
