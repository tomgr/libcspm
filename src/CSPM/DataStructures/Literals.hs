module CSPM.DataStructures.Literals (
    Literal(..)
) where

import Util.PrettyPrint

data Literal = 
    -- | An integer (arbitrarily sized).
    Int Integer
    -- | A boolean (TODO: remove).
    | Bool Bool
    deriving (Eq, Show)

instance PrettyPrintable Literal where
    prettyPrint (Int n) = integer n
    prettyPrint (Bool True) = text "true"
    prettyPrint (Bool False) = text "false"
