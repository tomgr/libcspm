{
{-# OPTIONS_GHC -fno-warn-lazy-unlifted-bindings -fno-warn-unused-imports 
    -fno-warn-unused-binds #-}
module CSPM.Parser.Lexer where

import Data.ByteString.Internal (c2w)
import Data.Word

import CSPM.Parser.Exceptions
import CSPM.Parser.Monad
import CSPM.Parser.Tokens
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

@spaces = $white_no_nl*
@white_no_nl = ((\-\-.*)|$white_no_nl)+
@nl = ((\-\-.*\n)|$whitechar)*
@comment = (\-\-.*) 
@nltok = (@comment|())\n@nl
@notnot = [^n]|(n[^o])|(no[^t])

-- For string identification
$symbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$large = [A-Z \xc0-\xd6 \xd8-\xde]
$small = [a-z \xdf-\xf6 \xf8-\xff \_]
$string_alpha = [$small $large]
$graphic   = [$string_alpha $symbol $digit \:\"\']

$octit     = 0-7
$hexit     = [0-9 A-F a-f]

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
     | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
     | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
     | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap


-- Note that we allow newlines to preceed all tokens, except for those that
-- may possibly be at the start of a new expression. Therefore, for example,
-- as a + may never be at the start of an expression we allow newlines before
-- them. However, - and < may either appear in the middle of an expression or
-- the start of one and thus we do not allow newlines to come between them.
tokens :-
    <0>@nl:\[                   { begin sem_prop }
    <sem_prop>@spaces"tau priority"    { tok TTauPriority }
    <sem_prop>@spaces"tau priority over" { tok TTauPriority }
    <sem_prop>@spaces"deadlock free"   { tok TDeadlockFree }
    <sem_prop>@spaces"deadlock-free"   { tok TDeadlockFree }
    <sem_prop>@spaces"livelock free"   { tok TLivelockFree }
    <sem_prop>@spaces"livelock-free"   { tok TLivelockFree }
    <sem_prop>@spaces"divergence free" { tok TDivergenceFree }
    <sem_prop>@spaces"divergence-free" { tok TDivergenceFree }
    <sem_prop>@spaces"deterministic"   { tok TDeterministic }
    <sem_prop>@spaces"partial order reduce" { tok TPartialOrderReduce }
    <sem_prop>@nl"[T]"          { tok (TModel Traces) }
    <sem_prop>@nl"[F]"          { tok (TModel Failures) }
    <sem_prop>@nl"[FD]"         { tok (TModel FailuresDivergences) }
    <sem_prop>@nl"[V]"          { tok (TModel Revivals) }
    <sem_prop>@nl"[VD]"         { tok (TModel RevivalsDivergences) }
    <sem_prop>@nl"[R]"          { tok (TModel Refusals) }
    <sem_prop>@nl"[RD]"         { tok (TModel RefusalsDivergences) }
    <sem_prop>@nl"["$alpha+"]"  { stok (TStringOption . tail . lstrip . init) }
    <sem_prop>@spaces"]:"       { begin 0 }
    <sem_prop>@spaces"]"        { begin 0 }

    <0>@nl"[T="@nl              { tok (TRefines Traces) }
    <0>@nl"[F="@nl              { tok (TRefines Failures) }
    <0>@nl"[FD="@nl             { tok (TRefines FailuresDivergences) }
    <0>@nl"[V="@nl              { tok (TRefines Revivals) }
    <0>@nl"[VD="@nl             { tok (TRefines RevivalsDivergences) }
    <0>@nl"[R="@nl              { tok (TRefines Refusals) }
    <0>@nl"[RD="@nl             { tok (TRefines RefusalsDivergences) }

    <soak>((\-\-.*\n)|$whitechar)+ { skip }
    <soak>""/$not_white         { begin 0 }
    <soak>@nl"{-"               { nestedComment }

    <assert>((\-\-.*\n)|$whitechar)+ { skip }
    <assert>@nl"not"/$notid     { begin' soak TAssertNot }
    <assert>""/@notnot          { begin 0 }
    <assert>@nl"{-"             { nestedComment }

    <0>"print ".*$              { stok (\ s ->
                                    TPrint (drop (length "print ") s))}

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
    <0>@nl"/+"@nl               { tok TLSyncInterrupt }
    <0>@nl"+\"@nl               { tok TRSyncInterrupt }
    <0>@nl"[+"@nl               { tok TLSyncExtChoice }
    <0>@nl"+]"@nl               { tok TRSyncExtChoice }

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
    <0>"(| "@nl                 { openseq TLMap }
    <0>@nl"|)"                  { closeseq TRMap }
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
    <0>"::"                     { tok TScope }
    <0>@nl":"                   { soakTok TColon }

    <0>@nl" ::"@nl              { tok TOfType }
    <0>@nl":: "@nl              { tok TOfType }
    <0>@nl"=>*"@nl              { tok TYieldStar }
    <0>@nl"=>"@nl               { tok TYield }

    <0>@nl"@@"@nl               { tok TDoubleAt }

    -- Program Structure
    <0>@nl"="@nl                { tok TDefineEqual }
    <0>@nl"if"/$notid           { soakTok TIf }
    <0>@nl"then"/$notid         { soakTok TThen }
    <0>@nl"else"/$notid         { soakTok TElse }
    <0>@nl"let"/$notid          { soakTok TLet }
    <0>@nl"within"/$notid       { soakTok TWithin }
    <0>"channel"/$notid         { soakTok TChannel }
    <0>"assert"/$notid          { soakTok' TAssert }
    <0>"datatype"/$notid        { soakTok TDataType }
    <0>"subtype"/$notid         { soakTok TSubType }
    <0>"external"/$notid        { soakTok TExternal }
    <0>"transparent"/$notid     { soakTok TTransparent }
    <0>"nametype"/$notid        { soakTok TNameType }
    <0>"module"/$notid          { soakTok TModule }
    <0>"exports"/$notid         { tok TExports }
    <0>"endmodule"/$notid       { tok TEndModule }
    <0>"instance"               { tok TInstance }
    <0>@nl"\"@nl                { tok TBackSlash }
    <0>@nl"@"@nl                { tok TLambdaDot }
    <0>"Timed"/$notid           { soakTok TTimed }

    -- Arithmetic
    <0>@nl"+"@nl                { tok TPlus }
    <0>"-"/[[^\->]\n]           { soakTok TMinus }
    <0>@nl"*"@nl                { tok TTimes }
    <0>@nl"/"@nl                { tok TDivide }
    <0>@nl"%"@nl                { tok TMod }

    -- Sequence Symbols
    <0>@nl"^"@nl                { tok TConcat }
    <0>"#"@nl                   { tok THash }

    -- 'Wildcards'
    <0>$alpha+$alphanum*$prime* { stok (\ s -> TIdent s) }
    <0>@nl$digit+               { stok (\ s -> TInteger (read s)) }
    <0>@nl \' ($graphic # [\'\\] | " " | @escape) \' 
                                { stok (\ s -> TChar (read s)) }
    <0>@nl\"@string*\"          { stok (\ s -> TString (read s)) }

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

soakTok' :: Token -> AlexInput -> Int -> ParseMonad LToken
soakTok' t inp len = setCurrentStartCode assert >> tok t inp len

-- TODO: don't count whitespace in the tokens
tok :: Token -> AlexInput -> Int -> ParseMonad LToken
tok t (ParserState { fileStack = fps:_ }) len =
        return $ L (SrcSpanOneLine f lineno colno (colno+len)) t
    where
        (FileParserState { tokenizerPos = FilePosition _ lineno colno, 
                            fileName = f }) = fps
tok _ _ _ = panic "tok: invalid state"

stok :: (String -> Token) -> AlexInput -> Int -> ParseMonad LToken
stok f (st @ ParserState { fileStack = stk }) len = do
        tok (f (filter (\c -> not (elem c ['\r','\n'])) (takeChars len stk))) st len

skip :: AlexInput -> Int -> ParseMonad LToken
skip _ _ = getNextToken

takeChars :: Int -> [FileParserState] -> String
takeChars 0 _ = ""
takeChars len (FileParserState {input = [] }:stk) = takeChars len stk
takeChars len (fps@(FileParserState {input = (c:cs) }):stk) = 
    c:(takeChars (len-1) (fps {input = cs}:stk))
takeChars _ _ = panic "takeChars: invalid input"

nestedComment :: AlexInput -> Int -> ParseMonad LToken
nestedComment _ _ = do
    st <- getParserState
    go 1 st
    where 
        err :: ParseMonad a
        err = do
            FileParserState { 
                fileName = fname, 
                tokenizerPos = pos } <- getTopFileParserState
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
                                Just (_,st)      -> go n st
                        '\123' -> do
                            case alexGetChar st of
                                Nothing       -> err
                                Just ('-',st) -> go (n+1) st
                                Just (_,st)   -> go n st
                        _ -> go n st

switchInput :: AlexInput -> Int -> ParseMonad LToken
switchInput st len = do
    FileParserState { 
        fileName = fname, 
        tokenizerPos = pos } <- getTopFileParserState
    let
        str = takeChars len (fileStack st)
        quotedFname = strip (drop (length "include") str)
        
        hasStartQuote ('\"':_) = True
        hasStartQuote _ = False

        hasEndQuote [] = False
        hasEndQuote ('\"':_) = True
        hasEndQuote (_:cs) = hasEndQuote cs
        
        file = calcFile (tail quotedFname)
        calcFile ('\"':_) = ""
        calcFile (c:cs) = c:calcFile cs
        calcFile [] = panic "switchInput::calcFile: empty file"

    if not (hasStartQuote quotedFname) || not (hasEndQuote (tail quotedFname)) then
        throwSourceError [invalidIncludeErrorMessage (filePositionToSrcLoc fname pos)]
    -- We push a newline token here to make sure that any expression that
    -- crosses the include boundary will be a hard parse error
    else pushFile file $ return $!
            L ((filePositionToSrcLoc fname pos)) TNewLine

type AlexInput = ParserState

begin :: Int -> AlexInput -> Int -> ParseMonad LToken
begin sc' _ _ = setCurrentStartCode sc' >> getNextToken

begin' :: Int -> Token -> AlexInput -> Int -> ParseMonad LToken
begin' sc' t st len = setCurrentStartCode sc' >> tok t st len

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (ParserState { fileStack = fps:_ }) = previousChar fps
alexInputPrevChar _ = panic "alexInputPrevChar: invalid state - no previous char"

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
        gc (FileParserState { input = [] }) = 
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
        input = input,
        fileName = fname, 
        tokenizerPos = pos, 
        currentStartCode = sc } <- getTopFileParserState
    st <- getParserState
    if length (fileStack st) > 1 && input == [] then do
        -- Switch input back
        setParserState (st { fileStack = tail (fileStack st) })
        -- Insert a newline to stop expressions spanning files
        return $! L ((filePositionToSrcLoc fname pos)) TNewLine
    else case alexScan st sc of
        AlexEOF -> return $ L Unknown TEOF
        AlexError _ -> 
            throwSourceError [lexicalErrorMessage (filePositionToSrcLoc fname pos)]
        AlexSkip st' _ -> do
            setParserState st'
            getNextToken
        AlexToken st' len action -> do
            setParserState st'
            action st len

getNextTokenWrapper :: (LToken -> ParseMonad a) -> ParseMonad a
getNextTokenWrapper cont = getNextToken >>= cont
}
