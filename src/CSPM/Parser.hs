{-# LANGUAGE MultiParamTypeClasses #-}
module CSPM.Parser (
    Parseable(..)
) 
where

import Control.Monad.Trans
import CSPM.DataStructures.Syntax

class Parseable p where
    -- | Parse as string as an 'PInteractiveStmt'.
    parseInteractiveStmt :: String -> String -> IO (PInteractiveStmt p)
    -- | Parses a string as an 'PExp'.
    parseExpression :: String -> String -> IO (PExp p)
    -- | Parse the given file, returning the parsed 'PModule's.
    parseFile :: String -> String -> IO [PModule p]
    -- | Parse a string, as though it were an entire file, returning the parsed
    -- 'PModule's.
    parseStringAsFile :: String -> String -> IO [PModule p]
