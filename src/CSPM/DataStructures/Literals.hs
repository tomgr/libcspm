module CSPM.DataStructures.Literals (
    Literal(..)
) where

import Util.PrettyPrint

data Literal = 
    -- | An integer. This is finite size, as per the FDR spec.
    Int Int
    -- | A boolean.
    | Bool Bool
    -- | A character.
    | Char Char
    -- | A string.
    | String String
    deriving (Eq, Ord, Show)

instance PrettyPrintable Literal where
    prettyPrint (Int n) = int n
    prettyPrint (Bool True) = text "true"
    prettyPrint (Bool False) = text "false"
    prettyPrint (Char c) = quotes (char c)
    prettyPrint (String s) = doubleQuotes (text s)
