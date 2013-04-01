module Util.Precedence (
    Precedence(..),
    Associativity(..),
) where

-- | The associativity of an operator
data Associativity = AssocNone | AssocLeft | AssocRight
    deriving (Eq, Ord, Show)

class Precedence a where
    -- | Returns the binding strength of the operator. Higher numbers bind more
    -- loosely.
    precedence :: a -> Int
    precedence _ = 0

    -- | Returns the associativity of the operator.
    associativity :: a -> Associativity
    associativity _ = AssocNone

    -- | True if the two items are the same operator. This is used to decide
    -- when to paranthesise.
    sameOperator :: a -> a -> Bool
    sameOperator _ _ = False
