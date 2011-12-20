module CSPM.DataStructures.Literals (
    Literal(..)
) where

data Literal = 
    -- | An integer (arbitrarily sized).
    Int Integer
    -- | A boolean (TODO: remove).
    | Bool Bool
    deriving (Eq, Show)
