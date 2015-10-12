module CSPM.Syntax.Literals (
    Literal(..),
    typeOfLiteral,
) where

import qualified Data.ByteString as B

import CSPM.Syntax.Types
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

data Literal = 
    -- | An integer. This is finite size, as per the FDR spec.
    Int Int
    -- | A boolean.
    | Bool Bool
    -- | A character.
    | Char Char
    -- | A string.
    | String B.ByteString
    -- | A source code location.
    | Loc SrcSpan
    deriving (Eq, Ord, Show)

instance PrettyPrintable Literal where
    prettyPrint (Int n) = int n
    prettyPrint (Bool True) = text "true"
    prettyPrint (Bool False) = text "false"
    prettyPrint (Char c) = quotes (char c)
    prettyPrint (String s) = doubleQuotes (bytestring s)
    prettyPrint (Loc l) = prettyPrint l

typeOfLiteral :: Literal -> Type
typeOfLiteral (Int _) = TInt
typeOfLiteral (Bool _) = TBool
typeOfLiteral (Char _) = TChar
typeOfLiteral (String _) = TSeq TChar
typeOfLiteral (Loc _) = panic "Cannot represent the type of a Loc"
