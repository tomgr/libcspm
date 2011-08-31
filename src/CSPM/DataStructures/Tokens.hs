module CSPM.DataStructures.Tokens (
    Token(..), LToken, Model(..),
)
where

import CSPM.DataStructures.Syntax (Model(..))
import Util.Annotated
import Util.PrettyPrint

data Token = 
    TInteger Integer
    | TFalse
    | TTrue
    | TIdent String

    | TRefines Model
    | TModel Model
    | TTauPriority
    | TDeadlockFree
    | TDivergenceFree
    | TLivelockFree
    | TDeterministic

    | TNewLine
    | TDefineEqual

    | TComma
    | TDot
    | TExclamationMark
    | TQuestionMark
    | TDollar
    | TPipe
    | TDoubleDot
    | TColon
    | TDrawnFrom
    | TTie -- "<->"

    | TDoubleAt
    | TWildCard

    | TIf
    | TThen
    | TElse
    | TLet
    | TWithin
    | TBackSlash
    | TLambdaDot
    | TChannel
    | TAssert
    | TDataType
    | TExternal
    | TTransparent
    | TNameType

    | TSemiColon
    | TGuard

    | TNot
    | TAnd
    | TOr
    | TEq
    | TNotEq
    | TLtEq
    | TGtEq
    | TLt
    | TGt
    | TPlus
    | TMinus
    | TTimes
    | TDivide
    | TMod
    | TCloseSeq
    | TEmptySeq

    | TConcat
    | THash

    | TLParen
    | TRParen
    | TLBrace
    | TRBrace
    | TLPipeBrace
    | TRPipeBrace
    | TLDoubleSqBracket
    | TRDoubleSqBracket
    | TLPipeSqBracket
    | TRPipeSqBracket
    | TLSqBracket
    | TRSqBracket 
    
    | TExtChoice
    | TIntChoice
    | TInterleave
    | TPrefix
    | TInterrupt
    | TSlidingChoice
    | TRException
    | TParallel

    | TEOF
    deriving (Eq, Show)
    
type LToken = Located Token

instance PrettyPrintable Token where
    prettyPrint e = text (show e)
