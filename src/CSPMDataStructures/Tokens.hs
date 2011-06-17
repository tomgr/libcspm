module CSPMDataStructures.Tokens where

import Util.Annotated

data Token = 
	TInteger Integer
	| TFalse
	| TTrue
	| TIdent String

	| TTraceRefines
	| TFailuresRefines
	| TFailuresDivergencesRefines
	| TTracesModel
	| TFailuresModel
	| TFailuresDivergencesModel
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
