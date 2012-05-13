{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module CSPM.Parser (
    Parseable(..)
) 
where

import Control.Monad.Trans
import CSPM.DataStructures.Syntax

class Parseable c p | p -> c where
    -- | Parse as string as an 'PInteractiveStmt'.
    parseInteractiveStmt :: c -> String -> String -> IO (PInteractiveStmt p)
    -- | Parses a string as an 'PExp'.
    parseExpression :: c -> String -> String -> IO (PExp p)
    -- | Parse the given file, returning the parsed 'PModule's.
    parseFile :: c -> String -> String -> IO [PModule p]
    -- | Parse a string, as though it were an entire file, returning the parsed
    -- 'PModule's.
    parseStringAsFile :: c -> String -> String -> IO [PModule p]
