{
{-# OPTIONS_GHC -fno-warn-lazy-unlifted-bindings #-}
module CSPM.Parser.Lexer where

import Data.ByteString.Internal (c2w)
import Data.List
import Data.Word

import Control.Monad.Trans
import CSPM.DataStructures.Tokens
import CSPM.Parser.Exceptions
import CSPM.Parser.Monad
import Util.Annotated
import Util.Exception

}

$digit      = [0-9]
$whitechar   = [\ \n\r\f\v\t]
$white_no_nl = $whitechar # \n
$not_white = [^$whitechar]
$alpha = [A-Za-z]
$upperalpha = [A-Z]
$alphaspace = [ $alpha]
$alphanum = [A-Za-z0-9_]
$propertychars = [A-Za-z0-9\ _\-]
$prime = '
$notid = [[^0-9a-zA-Z_]\(\[$whitechar]

@property = $propertychars+(\[$upperalpha+\])?

@white_no_nl = ((\-\-.*)|$white_no_nl)+
@nl = ((\-\-.*\n)|$whitechar)*
@comment = (\-\-.*) 
@nltok = (@comment|())\n@nl

-- Note that we allow newlines to preceed all tokens, except for those that
-- may possibly be at the start of a new expression. Therefore, for example,
-- as a + may never be at the start of an expression we allow newlines before
-- them. However, - and < may either appear in the middle of an expression or
-- the start of one and thus we do not allow newlines to come between them.
tokens :-
    <0>@nl:\[                   { begin sem_prop }
    <sem_prop>"tau priority"    { tok TTauPriority }
    <sem_prop>"tau priority over" { tok TTauPriority }
    <sem_prop>"deadlock free"   { tok TDeadlockFree }
    <sem_prop>"deadlock-free"   { tok TDeadlockFree }
    <sem_prop>"livelock free"   { tok TLivelockFree }
    <sem_prop>"livelock-free"   { tok TLivelockFree }
    <sem_prop>"divergence free" { tok TDivergenceFree }
    <sem_prop>"divergence-free" { tok TDivergenceFree }
    <sem_prop>"deterministic"   { tok TDeterministic }
    <sem_prop>@nl"[T]"          { tok (TModel Traces) }
    <sem_prop>@nl"[F]"          { tok (TModel Failures) }
    <sem_prop>@nl"[FD]"         { tok (TModel FailuresDivergences) }
    <sem_prop>@nl"[V]"          { tok (TModel Revivals) }
    <sem_prop>@nl"[VD]"         { tok (TModel RevivalsDivergences) }
    <sem_prop>@nl"[R]"          { tok (TModel Refusals) }
    <sem_prop>@nl"[RD]"         { tok (TModel RefusalsDivergences) }
    <sem_prop>"]:"              { begin 0 }
    <sem_prop>"]"               { begin 0 }

    <0>@nl"[T="@nl              { tok (TRefines Traces) }
    <0>@nl"[F="@nl              { tok (TRefines Failures) }
    <0>@nl"[FD="@nl             { tok (TRefines FailuresDivergences) }
    <0>@nl"[V="@nl              { tok (TRefines Revivals) }
    <0>@nl"[VD="@nl             { tok (TRefines RevivalsDivergences) }
    <0>@nl"[R="@nl              { tok (TRefines Refusals) }
    <0>@nl"[RD="@nl             { tok (TRefines RefusalsDivergences) }

    <soak>((\-\-.*\n)|$whitechar)+  { skip }
    <soak>""/$not_white         { begin 0 }
    <soak>@nl"{-"                  { nestedComment }

    <0>@white_no_nl             { skip }

    <0>@nl"{-"                  { nestedComment }

    <0>@nl"false"/$notid        { tok TFalse }
    <0>@nl"true"/$notid         { tok TTrue }

    <0>"include"$white_no_nl+.*\n { switchInput }

    -- Process Syntax
    <0>@nl"[]"@nl               { tok TExtChoice }
    <0>@nl"|~|"@nl              { tok TIntChoice }
    <0>@nl"|||"@nl              { tok TInterleave }
    <0>@nl"/\"@nl               { tok TInterrupt }
    <0>@nl"->"@nl               { tok TPrefix }
    <0>@nl"[>"@nl               { tok TSlidingChoice }
    <0>@nl"|>"@nl               { tok TRException }
    <0>@nl"||"@nl               { tok TParallel }
    <0>@nl";"@nl                { tok TSemiColon }
    <0>@nl"&"@nl                { tok TGuard }

    -- Boolean Operators
    <0>@nl"and"/$notid          { soakTok TAnd }
    <0>@nl"or"/$notid           { soakTok TOr }
    <0>@nl"not"/$notid          { soakTok TNot }
    <0>@nl"=="@nl               { tok TEq }
    <0>@nl"!="@nl               { tok TNotEq }
    <0>@nl"<="@nl               { tok TLtEq }
    <0>@nl">="@nl               { tok TGtEq }
    -- We need a empty sequence token since the parser will not execute the
    <0>"<"$whitechar*">"        { tok TEmptySeq }
    <0>"<"@nl                   { tok TLt }
    <0>@nl">"                   { gt }

    -- Parenthesis
    <0>"("@nl                   { openseq TLParen }
    <0>@nl")"                   { closeseq TRParen }
    <0>"{|"@nl                  { openseq TLPipeBrace }
    <0>@nl"|}"                  { closeseq TRPipeBrace }
    <0>"{"@nl                   { openseq TLBrace }
    <0>@nl"}"                   { closeseq TRBrace }
    <0>@nl"[["@nl               { openseq TLDoubleSqBracket }
    <0>@nl"]]"                  { closeseq TRDoubleSqBracket }
    <0>@nl"[|"@nl               { openseq TLPipeSqBracket }
    <0>@nl"|]"@nl               { closeseq TRPipeSqBracket }
    <0>@nl"["@nl                { tok TLSqBracket }
    <0>@nl"]"@nl                { tok TRSqBracket }

    -- General Symbols
    <0>@nl"|"@nl                { tok TPipe }
    <0>@nl","@nl                { tok TComma }
    <0>@nl".."@nl               { tok TDoubleDot }
    <0>@nl"."@nl                { tok TDot }
    <0>@nl"?"@nl                { tok TQuestionMark }
    <0>@nl"!"@nl                { tok TExclamationMark }
    <0>@nl"$"@nl                { tok TDollar }
    <0>@nl"<-"@nl               { tok TDrawnFrom }
    <0>@nl"<->"@nl              { tok TTie }
    <0>@nl":"@nl                { tok TColon }

    <0>@nl"@@"@nl               { tok TDoubleAt }

    -- Program Structure
    <0>@nl"="@nl                { tok TDefineEqual }
    <0>@nl"if"/$notid           { soakTok TIf }
    <0>@nl"then"/$notid         { soakTok TThen }
    <0>@nl"else"/$notid         { soakTok TElse }
    <0>@nl"let"/$notid          { soakTok TLet }
    <0>@nl"within"/$notid       { soakTok TWithin }
    <0>"channel"/$notid         { soakTok TChannel }
    <0>"assert"/$notid          { soakTok TAssert }
    <0>"datatype"/$notid        { soakTok TDataType }
    <0>"external"/$notid        { soakTok TExternal }
    <0>"transparent"/$notid     { soakTok TTransparent }
    <0>"nametype"/$notid        { soakTok TNameType }

    <0>@nl"\"@nl                { tok TBackSlash }
    <0>@nl"@"@nl                { tok TLambdaDot }

    -- Arithmetic
    <0>@nl"+"@nl                { tok TPlus }
    <0>"-"@nl                   { tok TMinus }
    <0>@nl"*"@nl                { tok TTimes }
    <0>@nl"/"@nl                { tok TDivide }
    <0>@nl"%"@nl                { tok TMod }

    -- Sequence Symbols
    <0>@nl"^"@nl                { tok TConcat }
    <0>"#"@nl                   { tok THash }

    -- 'Wildcards'
    <0>$alpha+$alphanum*$prime* { stok (\s -> TIdent s) }
    <0>@nl$digit+               { stok (\ s -> TInteger (read s)) }

    -- Must be after names
    <0>@nl"_"@nl                { tok TWildCard }

    <0>@nltok                   { tok TNewLine }

{
wschars :: String
wschars = " \t\r\n"

strip :: String -> String
strip = lstrip . rstrip

-- | Same as 'strip', but applies only to the left side of the string.
lstrip :: String -> String
lstrip s = case s of
    [] -> []
    (x:xs) -> if elem x wschars then lstrip xs else s

-- | Same as 'strip', but applies only to the right side of the string.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

openseq token inp len = 
    do
        cs <- getSequenceStack
        setSequenceStack (0:cs)
        tok token inp len
closeseq token inp len = 
    do
        (c:cs) <- getSequenceStack
        case cs of
            c1:cs -> setSequenceStack (c+c1:cs)
            [] -> 
                -- Must be because of a syntax error (too many closing brakcets)
                -- We let the parser catch this and we try and do something
                -- sensible.
                setSequenceStack [0]
        tok token inp len

gt :: AlexInput -> Int -> ParseMonad LToken
gt inp len = do
    (c:cs) <- getSequenceStack
    if c > 0 then do
        setSequenceStack (c-1:cs)
        tok TCloseSeq inp len
    else tok TGt inp len

soakTok :: Token -> AlexInput -> Int -> ParseMonad LToken
soakTok t inp len = setCurrentStartCode soak >> tok t inp len

-- TODO: don't count whitespace in the tokens
tok :: Token -> AlexInput -> Int -> ParseMonad LToken
tok t (ParserState { fileStack = fps:_ }) len =
        return $ L (SrcSpanOneLine f lineno colno (colno+len)) t
    where
        (FileParserState { tokenizerPos = FilePosition offset lineno colno, 
                            fileName = f }) = fps

stok :: (String -> Token) -> AlexInput -> Int -> ParseMonad LToken
stok f (st @ ParserState { fileStack = stk }) len = do
        tok (f (filter (\ c -> c /= '\n') (takeChars len stk))) st len

skip input len = getNextToken

takeChars :: Int -> [FileParserState] -> String
takeChars 0 _ = ""
takeChars len (FileParserState {input = [] }:stk) = takeChars len stk
takeChars len (fps@(FileParserState {input = (c:cs) }):stk) = 
    c:(takeChars (len-1) (fps {input = cs}:stk))

nestedComment :: AlexInput -> Int -> ParseMonad LToken
nestedComment _ _ = do
    st <- getParserState
    go 1 st
    where 
        err :: ParseMonad a
        err = do
            FileParserState { 
                fileName = fname, 
                tokenizerPos = pos, 
                currentStartCode = sc } <- getTopFileParserState
            throwSourceError [lexicalErrorMessage (filePositionToSrcLoc fname pos)]
        go :: Int -> AlexInput -> ParseMonad LToken
        go 0 st = do setParserState st; getNextToken
        go n st = do
            case alexGetChar st of
                Nothing  -> err
                Just (c,st) -> do
                    case c of
                        '-' -> do
                            case alexGetChar st of
                                Nothing          -> err
                                Just ('\125',st) -> go (n-1) st
                                Just (c,st)      -> go n st
                        '\123' -> do
                            case alexGetChar st of
                                Nothing       -> err
                                Just ('-',st) -> go (n+1) st
                                Just (c,st)   -> go n st
                        c -> go n st

switchInput :: AlexInput -> Int -> ParseMonad LToken
switchInput st len = do
    FileParserState { 
        fileName = fname, 
        tokenizerPos = pos, 
        currentStartCode = sc } <- getTopFileParserState
    let
        str = takeChars len (fileStack st)
        quotedFname = strip (drop (length "include") str)
        
        hasStartQuote ('\"':cs) = True
        hasStartQuote _ = False

        hasEndQuote [] = False
        hasEndQuote ('\"':cs) = True
        hasEndQuote (c:cs) = hasEndQuote cs
        
        file = calcFile (tail quotedFname)
        calcFile ('\"':cs) = ""
        calcFile (c:cs) = c:calcFile cs

    if not (hasStartQuote quotedFname) || not (hasEndQuote (tail quotedFname)) then
        throwSourceError [invalidIncludeErrorMessage (filePositionToSrcLoc fname pos)]
    else pushFile file getNextToken

type AlexInput = ParserState

begin :: Int -> AlexInput -> Int -> ParseMonad LToken
begin sc' st len = setCurrentStartCode sc' >> getNextToken

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (ParserState { fileStack = fps:_ })= previousChar fps

-- For compatibility with Alex 3
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte inp = 
    case alexGetChar inp of 
        Nothing -> Nothing
        Just (c, inp') -> 
            -- Truncate the char to the first byte
            Just (c2w c, inp')
    
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (ParserState { fileStack = [] }) = Nothing
alexGetChar (st @ (ParserState { fileStack = fps:fpss })) = gc fps
    where
        gc (fps @ (FileParserState { input = [] })) = 
            alexGetChar (st { fileStack = fpss })
        gc (fps @ (FileParserState { tokenizerPos = p, input = (c:s) })) =
                p' `seq` Just (c, st')
            where
                p' = movePos p c
                fps' = fps { input = s, tokenizerPos = p', previousChar = c }
                st' = st { fileStack = fps':fpss }

getNextToken :: ParseMonad LToken
getNextToken = do
    FileParserState { 
        fileName = fname, 
        tokenizerPos = pos, 
        currentStartCode = sc } <- getTopFileParserState
    st <- getParserState
    case alexScan st sc of
        AlexEOF -> return $ L Unknown TEOF
        AlexError st' -> 
            throwSourceError [lexicalErrorMessage (filePositionToSrcLoc fname pos)]
        AlexSkip st' len -> do
            setParserState st'
            getNextToken
        AlexToken st' len action -> do
            setParserState st'
            action st len

getNextTokenWrapper :: (LToken -> ParseMonad a) -> ParseMonad a
getNextTokenWrapper cont = getNextToken >>= cont

}
