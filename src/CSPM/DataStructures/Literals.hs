module CSPM.DataStructures.Literals (
    Literal(..)
) where

import Util.PrettyPrint

data Literal = 
    -- | An integer. This is finite size, as per the FDR spec.
    Int Int
    -- | A boolean (TODO: remove).
    | Bool Bool
    deriving (Eq, Show)

instance PrettyPrintable Literal where
    prettyPrint (Int n) = int n
    prettyPrint (Bool True) = text "true"
    prettyPrint (Bool False) = text "false"
