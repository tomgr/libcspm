module Util.Precedence (
    Precedence(..)
) where

class Precedence a where
    precedence :: a -> Int
    precedence _ = 0
