{
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
module CSPM.Parser.Lexer where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (c2w, w2c)
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


-- Note that we allow newlines to preceed all tokens, except for those that
-- may possibly be at the start of a new expression. Therefore, for example,
-- as a + may never be at the start of an expression we allow newlines before
-- them. However, - and < may either appear in the middle of an expression or
-- the start of one and thus we do not allow newlines to come between them.
tokens :-
    <0>@nl:\[                   { begin sem_prop }
    <sem_prop>@spaces"tau priority"    { tok TTauPriority }
    <sem_prop>@spaces"tags"    { tok TTags }
    <sem_prop>@spaces"tau priority over" { tok TTauPriority }
    <sem_prop>@spaces"deadlock free"   { tok TDeadlockFree }
    <sem_prop>@spaces"deadlock-free"   { tok TDeadlockFree }
    <sem_prop>@spaces"sublock free"    { tok TSublockFree }
    <sem_prop>@spaces"sublock-free"    { tok TSublockFree }
    <sem_prop>@spaces"mutual exclusion" { tok TMutualExclusion }
    <sem_prop>@spaces"livelock free"   { tok TLivelockFree }
    <sem_prop>@spaces"livelock-free"   { tok TLivelockFree }
    <sem_prop>@spaces"divergence free" { tok TDivergenceFree }
    <sem_prop>@spaces"divergence-free" { tok TDivergenceFree }
    <sem_prop>@spaces"deterministic"   { tok TDeterministic }
    <sem_prop>@spaces"has trace"   { tok THasTrace }
    <sem_prop>@spaces"partial order reduce" { tok TPartialOrderReduce }
    <sem_prop>@spaces"static" 	{ tok TAnalyseStatically }
    <sem_prop>@spaces"global" 	{ tok TGlobalStaticProperty }
    <sem_prop>@spaces"local" 	{ tok TLocalStaticProperty }
    <sem_prop>@nl"[T]"          { tok (TModel Traces) }
    <sem_prop>@nl"[F]"          { tok (TModel Failures) }
    <sem_prop>@nl"[FD]"         { tok (TModel FailuresDivergences) }
    <sem_prop>@nl"[V]"          { tok (TModel Revivals) }
    <sem_prop>@nl"[VD]"         { tok (TModel RevivalsDivergences) }
    <sem_prop>@nl"[R]"          { tok (TModel Refusals) }
    <sem_prop>@nl"[RD]"         { tok (TModel RefusalsDivergences) }
    <sem_prop>@nl"["$alpha+"]"  { stok (TStringOption . B.tail . lstrip . B.init) }
    <sem_prop>@spaces"]:"       { begin 0 }
    <sem_prop>@spaces"]"        { begin 0 }

    <0>@nl"[T="@nl              { soakTok (TRefines Traces) }
    <0>@nl"[F="@nl              { soakTok (TRefines Failures) }
    <0>@nl"[FD="@nl             { soakTok (TRefines FailuresDivergences) }
    <0>@nl"[V="@nl              { soakTok (TRefines Revivals) }
    <0>@nl"[VD="@nl             { soakTok (TRefines RevivalsDivergences) }
    <0>@nl"[R="@nl              { soakTok (TRefines Refusals) }
    <0>@nl"[RD="@nl             { soakTok (TRefines RefusalsDivergences) }

    <soak>((\-\-.*\n)|$whitechar)+ { skip }
    <soak>""/$not_white         { begin 0 }
    <soak>@nl"{-"               { nestedComment }

    <assert>((\-\-.*\n)|$whitechar)+ { skip }
    <assert>@nl"not"/$notid     { begin' soak TAssertNot }
    <assert>""/@notnot          { begin 0 }
    <assert>@nl"{-"             { nestedComment }

    <0>"print ".*$              { stok (TPrint . B.drop (length ("print " :: [Char]))) }

    <0>@white_no_nl             { skip }

    <0>@nl"{-"                  { nestedComment }

    <0>@nl"false"/$notid        { tok TFalse }
    <0>@nl"true"/$notid         { tok TTrue }

    <0>"include"$white_no_nl+.*\n { switchInput }

    -- Process Syntax
    <0>@nl"[]"@nl               { soakTok TExtChoice }
    <0>@nl"|~|"@nl              { soakTok TIntChoice }
    <0>@nl"|||"@nl              { soakTok TInterleave }
    <0>@nl"/\"@nl               { soakTok TInterrupt }
    <0>@nl"->"@nl               { soakTok TPrefix }
    <0>@nl"[>"@nl               { soakTok TSlidingChoice }
    <0>@nl"|>"@nl               { soakTok TRException }
    <0>@nl"||"@nl               { soakTok TParallel }
    <0>@nl";"@nl                { soakTok TSemiColon }
    <0>@nl"&"@nl                { soakTok TGuard }
    <0>@nl"|\"@nl               { soakTok TProject }
    <0>@nl"/+"@nl               { soakTok TLSyncInterrupt }
    <0>@nl"+\"@nl               { soakTok TRSyncInterrupt }
    <0>@nl"[+"@nl               { soakTok TLSyncExtChoice }
    <0>@nl"+]"@nl               { soakTok TRSyncExtChoice }

    -- Boolean Operators
    <0>@nl"and"/$notid          { soakTok TAnd }
    <0>@nl"or"/$notid           { soakTok TOr }
    <0>@nl"not"/$notid          { soakTok TNot }
    <0>@nl"=="@nl               { soakTok TEq }
    <0>@nl"!="@nl               { soakTok TNotEq }
    <0>@nl"<="@nl               { soakTok TLtEq }
    <0>@nl">="@nl               { soakTok TGtEq }
    -- We need a empty sequence token since the parser will not execute the
    <0>"<"$whitechar*">"        { tok TEmptySeq }
    <0>"<"@nl                   { soakTok TLt }
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
    <0>@nl"["@nl                { soakTok TLSqBracket }
    <0>@nl"]"@nl                { soakTok TRSqBracket }

    -- General Symbols
    <0>@nl"|"@nl                { soakTok TPipe }
    <0>@nl","@nl                { soakTok TComma }
    <0>@nl".."@nl               { soakTok TDoubleDot }
    <0>@nl"."@nl                { soakTok TDot }
    <0>@nl"?"@nl                { soakTok TQuestionMark }
    <0>@nl"!"@nl                { soakTok TExclamationMark }
    <0>@nl"$"@nl                { soakTok TDollar }
    <0>@nl"<-"@nl               { soakTok TDrawnFrom }
    <0>@nl"<->"@nl              { soakTok TTie }
    <0>"::"                     { soakTok TScope }
    <0>@nl":"                   { soakTok TColon }

    <0>@nl" ::"@nl              { soakTok TOfType }
    <0>@nl":: "@nl              { soakTok TOfType }
    <0>@nl"=>*"@nl              { soakTok TYieldStar }
    <0>@nl"=>"@nl               { soakTok TYield }

    <0>@nl"@@"@nl               { soakTok TDoubleAt }

    -- Program Structure
    <0>@nl"="@nl                { soakTok TDefineEqual }
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
    <0>"instance"               { soakTok TInstance }
    <0>@nl"\"@nl                { soakTok TBackSlash }
    <0>@nl"@"@nl                { soakTok TLambdaDot }
    <0>"Timed"/$notid           { soakTok TTimed }

    -- Arithmetic
    <0>@nl"+"@nl                { soakTok TPlus }
    <0>"-"/[[^\->]\n]           { soakTok TMinus }
    <0>@nl"*"@nl                { soakTok TTimes }
    <0>@nl"/"@nl                { soakTok TDivide }
    <0>@nl"%"@nl                { soakTok TMod }

    -- Sequence Symbols
    <0>@nl"^"@nl                { soakTok TConcat }
    <0>"#"@nl                   { soakTok THash }

    -- 'Wildcards'
    <0>$alpha+$alphanum*$prime* { stok TIdent }
    <0>@nl$digit+               { stok (TInteger . read . map B.w2c . B.unpack) }
    <0>@nl\'                    { lexChar }
    <0>@nl\"                    { lexString }

    -- Must be after names
    <0>@nl"_"@nl                { soakTok TWildCard }

    <0>@nltok                   { tok TNewLine }

{
wschars :: [Word8]
wschars = map B.c2w [' ', '\t', '\r', '\n']

strip :: B.ByteString -> B.ByteString
strip = lstrip . rstrip

-- | Same as 'strip', but applies only to the left side of the string.
lstrip :: B.ByteString -> B.ByteString
lstrip = B.dropWhile (`elem` wschars)

-- | Same as 'strip', but applies only to the right side of the string.
rstrip :: B.ByteString -> B.ByteString
rstrip = B.reverse . lstrip . B.reverse

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

lexString :: AlexInput -> Int -> ParseMonad LToken
lexString _ _ = do
    st <- getParserState
    FileParserState { fileName = fname, tokenizerPos = pos } <- getTopFileParserState
    let
        startLoc = filePositionToSrcLoc fname pos

        accumulate :: [Word8] -> AlexInput -> ParseMonad LToken
        accumulate s st  = do
            case alexGetByte' st of
                (c, st) ->
                    case B.w2c c of
                        '\n' -> raiseLexicalError st
                        '"' -> do
                            let endLoc = currentFilePosition st
                            setParserState st
                            return $! L (makeLineSpan startLoc endLoc)
                                (TString (B.pack (reverse s)))
                        '\\' ->
                            -- Escaped string
                            case alexGetByte' st of
                                (c, st) ->
                                    case B.w2c c of
                                        '\n' -> raiseLexicalError st
                                        'r' -> accumulate (B.c2w '\r':s) st
                                        'n' -> accumulate (B.c2w '\n':s) st
                                        't' -> accumulate (B.c2w '\t':s) st
                                        _ -> accumulate (B.c2w '\"':s) st
                        c -> accumulate (B.c2w c:s) st
    accumulate [] st

lexChar :: AlexInput -> Int -> ParseMonad LToken
lexChar _ _ = do
    st <- getParserState
    let startLoc = currentFilePosition st

        -- Consumes the closing '
        checkClosing st c =
            let (closing, st') = alexGetByte' st
            in if B.w2c closing == '\'' then do
                    let endLoc = currentFilePosition st'
                    setParserState st'
                    return $! L (makeLineSpan startLoc endLoc) (TChar c)
                else raiseLexicalError st'

    case alexGetByte' st of
        (c, st) -> do
            case B.w2c c of
                '\n' -> raiseLexicalError st
                '\\' ->
                    let (c, st') = lexCharSubtitution st
                    in checkClosing st' c
                c -> checkClosing st c

lexCharSubtitution :: AlexInput -> (Char, AlexInput)
lexCharSubtitution st =
    case alexGetByte' st of
        (c, st) -> (sub (B.w2c c), st)
    where
        sub 'r' = '\r'
        sub 'n' = '\n'
        sub 't' = '\t'
        sub c = c

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

stok :: (B.ByteString -> Token) -> AlexInput -> Int -> ParseMonad LToken
stok f (st @ ParserState { fileStack = stk }) len =
    tok (f (takeChars len stk)) st len
    -- (filter (\c -> not (elem c ['\r','\n']))

skip :: AlexInput -> Int -> ParseMonad LToken
skip _ _ = getNextToken

takeChars :: Int -> [FileParserState] -> B.ByteString
takeChars 0 _ = ""
takeChars len (FileParserState { input = input }:stk) =
    if B.length input < len then
        panic "takeChars: invalid input"
    else B.take len input

nestedComment :: AlexInput -> Int -> ParseMonad LToken
nestedComment _ _ = do
    st <- getParserState
    let
        startLoc = currentFilePosition st
        getChar st =
            alexGetByteWithErrorMessage (commentNotClosedErrorMessage startLoc) st

        go :: Int -> AlexInput -> ParseMonad LToken
        go 0 st = do setParserState st; getNextToken
        go n st = do
            case getChar st of
                (c, st) -> do
                    case B.w2c c of
                        '-' ->
                            let (w, st') = getChar st
                            in if B.w2c w == '\125' then go (n-1) st'
                                else go n st'
                        '\123' ->
                            let (w, st') = getChar st
                            in if B.w2c w == '-' then go (n+1) st'
                                else go n st'
                        _ -> go n st
    go 1 st

switchInput :: AlexInput -> Int -> ParseMonad LToken
switchInput st len = do
    FileParserState {
        fileName = fname,
        tokenizerPos = pos } <- getTopFileParserState
    let
        str :: String
        str = map B.w2c $ B.unpack $ takeChars len (fileStack st)

        strip = lstrip . rstrip
        lstrip = dropWhile (`elem` (map B.w2c wschars))
        rstrip = reverse . lstrip . reverse

        quotedFname = strip (drop (length ("include" :: [Char])) str)

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

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (ParserState { fileStack = [] }) = Nothing
alexGetByte (st @ (ParserState { fileStack = fps:fpss })) = gc fps
    where
        gc (FileParserState { input = input }) | B.null input =
            alexGetByte (st { fileStack = fpss })
        gc (fps @ (FileParserState { tokenizerPos = p, input = input })) =
                c `seq` p' `seq` fps' `seq` st' `seq` Just (c, st')
            where
                Just (c, s) = B.uncons input
                p' = movePos p (B.w2c c)
                fps' = fps { input = s, tokenizerPos = p',
                            previousChar = B.w2c c
                        }
                st' = st { fileStack = fps':fpss }

alexGetByte' :: AlexInput -> (Word8, AlexInput)
alexGetByte' st =
    alexGetByteWithErrorMessage (lexicalErrorMessage (currentFilePosition st)) st

alexGetByteWithErrorMessage :: ErrorMessage -> AlexInput -> (Word8, AlexInput)
alexGetByteWithErrorMessage msg st =
    case alexGetByte st of
        Just t -> t
        Nothing -> throwSourceError [msg]

getNextToken :: ParseMonad LToken
getNextToken = do
    FileParserState {
        input = input,
        currentStartCode = sc } <- getTopFileParserState
    st <- getParserState
    if length (fileStack st) > 1 && B.null input then do
        -- Switch input back
        setParserState (st { fileStack = tail (fileStack st) })
        -- Insert a newline to stop expressions spanning files
        return $! L (currentFilePosition st) TNewLine
    else case alexScan st sc of
        AlexEOF -> return $ L Unknown TEOF
        AlexError _ -> raiseLexicalError st
        AlexSkip st' _ -> do
            setParserState st'
            getNextToken
        AlexToken st' len action -> do
            setParserState st'
            action st len

raiseLexicalError :: AlexInput -> ParseMonad a
raiseLexicalError st =
    throwSourceError [lexicalErrorMessage (currentFilePosition st)]

getNextTokenWrapper :: (LToken -> ParseMonad a) -> ParseMonad a
getNextTokenWrapper cont = getNextToken >>= cont

currentFilePosition :: AlexInput -> SrcSpan
currentFilePosition st =
   let FileParserState {
            fileName = fname,
            tokenizerPos = pos
            } = head (fileStack st)
    in filePositionToSrcLoc fname pos

makeLineSpan :: SrcSpan -> SrcSpan -> SrcSpan
makeLineSpan (SrcSpanPoint f1 l1 c1) (SrcSpanPoint f2 l2 c2)
    | f1 == f2 && l1 == l2 = SrcSpanOneLine f1 l1 c1 c2

}
