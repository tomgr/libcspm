module CSPM.Parser.Tokens (
    Token(..), LToken, Model(..),
)
where

import CSPM.DataStructures.Syntax (Model(..))
import CSPM.PrettyPrinter
import Util.Annotated
import Util.PrettyPrint

data Token = 
    TInteger Int
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

    | TModule
    | TExports
    | TEndModule
    | TScope

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
    | TAssertNot
    | TDataType
    | TSubType
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
    deriving Eq
    
type LToken = Located Token

instance Show Token where
    show t = show (prettyPrint t)

instance PrettyPrintable Token where
    prettyPrint (TInteger i) = int i
    prettyPrint TFalse = text "false"
    prettyPrint TTrue = text "true"
    prettyPrint (TIdent s) = text s

    prettyPrint (TRefines m) = char '[' <> prettyPrint m <> char '='
    prettyPrint (TModel m) = char '[' <> prettyPrint m <> char ']'
    prettyPrint TTauPriority = text "tau priority"
    prettyPrint TDeadlockFree = text "deadlock free"
    prettyPrint TDivergenceFree = text "divergence free"
    prettyPrint TLivelockFree = text "livelock free"
    prettyPrint TDeterministic = text "deterministic"

    prettyPrint TNewLine = text "<newline>"
    prettyPrint TDefineEqual = char '='

    prettyPrint TModule = text "module"
    prettyPrint TExports = text "exports"
    prettyPrint TEndModule = text "endmodule"
    prettyPrint TScope = text "::"

    prettyPrint TComma = char ','
    prettyPrint TDot = char '.'
    prettyPrint TExclamationMark = char '!'
    prettyPrint TQuestionMark = char '?'
    prettyPrint TDollar = char '$'
    prettyPrint TPipe = char '|'
    prettyPrint TDoubleDot = text ".."
    prettyPrint TColon = char ':'
    prettyPrint TDrawnFrom = text "<-"
    prettyPrint TTie = text "<->"

    prettyPrint TDoubleAt = text "@@"
    prettyPrint TWildCard = char '_'

    prettyPrint TIf = text "if"
    prettyPrint TThen = text "then"
    prettyPrint TElse = text "else"
    prettyPrint TLet = text "let"
    prettyPrint TWithin = text "within"
    prettyPrint TBackSlash = char '\\'
    prettyPrint TLambdaDot = char '@'
    prettyPrint TChannel = text "channel"
    prettyPrint TAssert = text "assert"
    prettyPrint TAssertNot = text "not"
    prettyPrint TDataType = text "datatype"
    prettyPrint TSubType = text "subtype"
    prettyPrint TExternal = text "external"
    prettyPrint TTransparent = text "transparent"
    prettyPrint TNameType = text "nametype"

    prettyPrint TSemiColon = char ';'
    prettyPrint TGuard = char '&'

    prettyPrint TNot = text "not"
    prettyPrint TAnd = text "and"
    prettyPrint TOr = text "or"
    prettyPrint TEq = text "=="
    prettyPrint TNotEq = text "!="
    prettyPrint TLtEq = text "<="
    prettyPrint TGtEq = text ">="
    prettyPrint TLt = char '<'
    prettyPrint TGt = char '>'
    prettyPrint TPlus = char '+'
    prettyPrint TMinus = char '-'
    prettyPrint TTimes = char '*'
    prettyPrint TDivide = char '/'
    prettyPrint TMod = char '%'
    prettyPrint TCloseSeq = char '<'
    prettyPrint TEmptySeq = text "<>"

    prettyPrint TConcat = char '^'
    prettyPrint THash = char '#'

    prettyPrint TLParen = char '('
    prettyPrint TRParen = char ')'
    prettyPrint TLBrace = char '{'
    prettyPrint TRBrace = char '}'
    prettyPrint TLPipeBrace = text "{|"
    prettyPrint TRPipeBrace = text "|}"
    prettyPrint TLDoubleSqBracket = text "[["
    prettyPrint TRDoubleSqBracket = text "]]"
    prettyPrint TLPipeSqBracket = text "[|"
    prettyPrint TRPipeSqBracket = text "|]"
    prettyPrint TLSqBracket = text "["
    prettyPrint TRSqBracket = text "]"
    
    prettyPrint TExtChoice = text "[]"
    prettyPrint TIntChoice = text "|~|"
    prettyPrint TInterleave = text "|||"
    prettyPrint TPrefix = text "->"
    prettyPrint TInterrupt = text "/\\"
    prettyPrint TSlidingChoice = text "[>"
    prettyPrint TRException = text "|>"
    prettyPrint TParallel = text "||"

    prettyPrint TEOF = text "EOF"
