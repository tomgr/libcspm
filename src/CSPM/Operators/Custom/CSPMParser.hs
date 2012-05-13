{-# LANGUAGE QuasiQuotes #-}
module CSPM.Operators.Custom.CSPMParser (parseCSPMFile, stringParser) where

import Control.Monad(liftM, sequence)
import Control.Monad.Trans
import Control.Monad.Error
import Data.List
import qualified Text.Parsec.Expr as E
import Text.Parsec.Language
import Text.Parsec
import qualified Text.Parsec.Token as PT

import qualified CSPM.DataStructures.Literals as CSP
import qualified CSPM.DataStructures.Names as CSP
import qualified CSPM.DataStructures.Syntax as CSP
import qualified CSPM.DataStructures.Types as CSP
import CSPM.Operators.Custom.OpSemDataStructures
import CSPM.Operators.Custom.OperatorParsers
import qualified CSPM.Operators.Custom.Syntax as CSP
import qualified Util.Annotated as CSP
import Util.Exception
import Util.PartialFunctions

data CSPMParserState = 
    CSPMParserState {
        operatorDefinition :: OpSemDefinition,
        operatorSyntax :: PartialFunction Name (Maybe OperatorSyntax),
        repOperatorSyntax :: PartialFunction Name (Maybe OperatorSyntax),
        canUseAngles :: Bool
    }
    deriving Show

gets :: (CSPMParserState -> b) -> Parser b
gets f = liftM f getState

type Parser = ParsecT String CSPMParserState IO

prohibitAngles :: Parser a -> Parser a
prohibitAngles p =
    do
        useAngles <- gets canUseAngles
        modifyState (\ s -> s { canUseAngles = False })
        v <- p
        modifyState (\ s -> s { canUseAngles = useAngles })
        return v
allowAngles :: Parser a -> Parser a
allowAngles p =
    do
        useAngles <- gets canUseAngles
        modifyState (\ s -> s { canUseAngles = True })
        v <- p
        modifyState (\ s -> s { canUseAngles = useAngles })
        return v

angles = between (symbol "<") (symbol ">")
braces = between (symbol "{") (symbol "}")
bars = between (symbol "|") (symbol "|")

dotSep1 = \ p -> sepBy1 p dot


cspmLanguage = PT.LanguageDef {
        PT.commentStart = "{-",
        PT.commentEnd = "-}",
        PT.commentLine = "--",
        PT.nestedComments = False,
        PT.identStart = letter,
        PT.identLetter = alphaNum <|> char '\'' <|> char '_',
        -- TODO: these - NEED TO MAKE SURE USERS REALLY CAN ONLY USE THESE LETTERS
        -- TODO: deifne set operators
        PT.opStart = oneOf "+-*/%.^#=!;<>|\\[]",
        PT.opLetter = oneOf "+-*/%.^#=!;<>|\\[]",
        PT.reservedOpNames = [
            -- lambda functions
            "@",
            -- arthmetic
            "+", "-", "*", "/", "%",
            -- sequences
            "^", "#",
            -- boolean statements
            "and", "or", "not", "==", "!=", "<", ">", "<=", ">=",
            -- declaration
            -- TODO: we should possibly add in all user operators to ensure
            -- we properly parse those
            "=", "::"],
        PT.reservedNames = 
            ["let", "within", "if", "then", "else",
                "external", "transparent", "channel",
                "true", "false", "assert", "datatype"],
                -- TODO: we need to stop anyone from declaring anything that
                -- clashes with a) an operator definition, b) Proc_Ex clauses
                --              c) rep operator definitions
        PT.caseSensitive = True
    }

PT.TokenParser{ PT.parens = parens,
            PT.identifier = identifier,
            PT.natural = natural,
            PT.charLiteral = charLiteral,
            PT.stringLiteral = stringLiteral,
            PT.lexeme = lexeme,
            PT.reservedOp = reservedOp,
            PT.operator = operator,
            PT.reserved = reserved,
            PT.whiteSpace = whiteSpace,
            PT.symbol = symbol,
            PT.comma = comma,
            PT.dot = dot,
            PT.commaSep = commaSep,
            PT.commaSep1 = commaSep1} = PT.makeTokenParser cspmLanguage

annotateTypeParser :: Parser a -> Parser (CSP.Annotated (Maybe CSP.Type, CSP.PType) a)
annotateTypeParser = annotateParser $ do
    t <- CSP.freshPType
    return (Nothing, t)

annotateTypeParser' :: Parser a -> Parser (CSP.Annotated CSP.PType a)
annotateTypeParser' = annotateParser CSP.freshPType

annotateNullParser :: Parser a -> Parser (CSP.Annotated () a)
annotateNullParser = annotateParser (return ())

annotateParser :: IO c -> Parser a -> Parser (CSP.Annotated c a)
annotateParser mtyp p = 
    do
        typ <- lift mtyp
        startPos <- getPosition
        inner <- p
        endPos <- getPosition
        let srcloc = (CSP.SrcSpanPoint (sourceName startPos) 
                        (sourceLine startPos) (sourceColumn startPos))
        return $ CSP.An srcloc typ inner

annotateFunctionParser 
    :: IO c -> Parser (a -> b) -> Parser (a -> CSP.Annotated c b)
annotateFunctionParser mtyp p =
    do
        typ <- lift mtyp
        startPos <- getPosition
        inner <- p
        endPos <- getPosition
        let srcloc = (CSP.SrcSpanPoint (sourceName startPos) 
                        (sourceLine startPos) (sourceColumn startPos))
        return $ CSP.An srcloc typ . inner
annotateFunctionParser2
    :: IO d -> Parser (a -> b -> c) -> Parser (a -> b -> CSP.Annotated d c)
annotateFunctionParser2 mtyp p =
    do
        typ <- lift mtyp
        startPos <- getPosition
        inner <- p
        endPos <- getPosition
        let srcloc = (CSP.SrcSpanPoint (sourceName startPos) 
                        (sourceLine startPos) (sourceColumn startPos))
        return $ \ e1 e2 -> CSP.An srcloc typ (inner e1 e2)
annotateFunctionParser3
    :: IO e -> Parser (a -> b -> c -> d) -> Parser (a -> b -> c -> CSP.Annotated e d)
annotateFunctionParser3 mtyp p =
    do
        typ <- lift mtyp
        startPos <- getPosition
        inner <- p
        endPos <- getPosition
        let srcloc = (CSP.SrcSpanPoint (sourceName startPos) 
                        (sourceLine startPos) (sourceColumn startPos))
        return $ \ e1 e2 e3 -> CSP.An srcloc typ (inner e1 e2 e3)
        
stringParser :: String -> OpSemDefinition -> IO [CSP.CustomModule]
stringParser s opSemDef =
    do
        let opMap = [(n, syntax) | Operator n _ _ syntax <- operators opSemDef]
            repOpMap = [(n, syntax) | ReplicatedOperator n _ _ _ syntax <- operators opSemDef]
        res <- liftIO $ runParserT fileParser 
                                    (CSPMParserState opSemDef opMap repOpMap True) 
                                    "<stdin>" s
        case res of
            Left err -> panic (show err)
            Right v -> return v

parseCSPMFile :: String -> OpSemDefinition -> IO [CSP.CustomModule]
parseCSPMFile fname opSemDef = 
    do 
        input <- liftIO $ readFile fname
        let opMap = [(n, syntax) | Operator n _ _ syntax <- operators opSemDef]
            repOpMap = [(n, syntax) | ReplicatedOperator n _ _ _ syntax <- operators opSemDef]
        res <- liftIO $ runParserT fileParser (CSPMParserState opSemDef opMap repOpMap True) 
                                    fname input
        case res of
            Left err -> panic (show err)
            Right v -> return v

fileParser :: Parser [CSP.CustomModule]
fileParser = 
    do
        m <- annotateNullParser $
            do
                whiteSpace
                decls <- many declarationParser
                eof
                return $ CSP.GlobalModule decls
        return [m]

dotAppToList :: CSP.CustomExp -> [CSP.CustomExp]
dotAppToList (CSP.An a b exp) = 
    let
        dotAppToList' (CSP.DotApp e1 e2) = dotAppToList e1++dotAppToList e2
        dotAppToList' x = [CSP.An a b x]
    in
        dotAppToList' exp
    
freshSymbolTable = do
    pt <- CSP.freshPSymbolTable
    return (Nothing, pt)

declarationParser :: Parser CSP.CustomDecl
declarationParser = annotateParser freshSymbolTable (
    (try (reserved "external") >> liftM CSP.External (commaSep nameParser))
    <|> (try (reserved "transparent") >> liftM CSP.Transparent (commaSep nameParser))
    <|> do
            try (reserved "channel")
            names <- commaSep1 nameParser
            option (CSP.Channel names Nothing) (
                do
                    symbol ":"
                    exp <- expressionParser
                    return $ CSP.Channel names (Just exp))
    <|> 
        do
            try (reserved "datatype")
            name <- nameParser
            lexeme (string "=")
            es <- sepBy (annotateNullParser $
                    do
                        name <- nameParser
                        ts <- option Nothing (
                            do
                                dot
                                exp <- expressionParser
                                return $ Just exp
                                )
                        return $ CSP.DataTypeClause name ts) (symbol "|")
            return $ CSP.DataType name es
    <|> do
            try (reserved "assert")
            e1 <- expressionParser
            m <- modelParser
            e2 <- expressionParser
            return $ CSP.Assert $ CSP.Refinement e1 m e2 []
    <|> do
        pat <- try (
                do
                    pat <- patternParser
                    -- We don't use reservedOp here since if we did it would
                    -- not parse p =#s properly since we have said =# is a
                    -- proper operator. TODO: This could be fixed by 
                    -- making all user operators reservedOps.
                    lexeme (string "=")
                    return pat)
        exp <- expressionParser
        return (CSP.PatBind pat exp)
    <|> do
        name <- lookAhead identifier
        typ <- typeAnnotationParser name
        -- ignore the type
        matches <- many1 (matchParser name)
        return $ CSP.FunBind (CSP.UnQual (CSP.OccName name)) matches
    <?> "declaration")

typeAnnotationParser :: String -> Parser (Maybe [CSP.CustomExp])
typeAnnotationParser n =
    (do
        try (lexeme (string n) >> lookAhead (string "::"))
        reserved "::"
        args <- parens (commaSep expressionParser)
        reserved "->"
        reserved "Proc"
        return $ Just args)
    <|> return Nothing

modelParser :: Parser CSP.Model
modelParser =
    do
        whiteSpace
        char '['
        model <- choice [
            string "T" >> return CSP.Traces,
            try (string "FD" >> return CSP.FailuresDivergences),
            string "F" >> return CSP.Failures]
        char '='
        whiteSpace
        return model

matchParser :: String -> Parser CSP.CustomMatch
matchParser fname = annotateNullParser $
    do
        -- Test to see if the function name is the same
        try ((lexeme . string $ fname) >> lookAhead (symbol "("))
        groups <- 
            many1 (parens (option [] (commaSep patternParser)))
        lexeme (string "=")
        exp <- expressionParser
        return $ CSP.Match groups exp

patternParser :: Parser CSP.CustomPat
patternParser = 
    let
        sequence = liftM CSP.PList . angles . commaSep $ patternParser
        set = liftM CSP.PSet . braces . commaSep $ patternParser
        tuple = liftM CSP.PTuple . parens . commaSep $ patternParser
    
        operatorTable = 
            -- Top binds tightest
            [[E.Infix 
                (annotateFunctionParser2 (return ())
                    (reservedOp "." >> return CSP.PDotApp)) 
                E.AssocLeft],
            [E.Infix 
                (annotateFunctionParser2 (return ())
                    (reservedOp "^" >> return CSP.PConcat))
                E.AssocLeft],
            [E.Infix 
                (annotateFunctionParser2 (return ())
                    (reservedOp "@@" >> return CSP.PDoublePattern))
                E.AssocLeft]]
            
        infixParser = E.buildExpressionParser operatorTable term
        
        tupleOrPatternParser = 
            do
                p1 <- patternParser
                option p1 (annotateNullParser $ do
                    comma
                    ps <- commaSep1 patternParser
                    return $ CSP.PTuple (p1:ps))
        
        term =
            choice [
                annotateNullParser $ symbol "_" >> return CSP.PWildCard,
                annotateNullParser $ liftM CSP.PLit literalParser,
                annotateNullParser sequence,
                annotateNullParser set,
                parens tupleOrPatternParser,
                annotateNullParser $ liftM CSP.PVar nameParser]
    in
        infixParser
        <|> term

nameParser :: Parser CSP.UnRenamedName
nameParser = liftM (CSP.UnQual . CSP.OccName) identifier

expressionParser :: Parser CSP.CustomExp
expressionParser =
    let
        sequence =
            angles (prohibitAngles (
                option (CSP.List []) (
                    do
                        e1 <- expressionParser
                        choice [
                            do
                                lexeme (string "..")
                                option (CSP.ListEnumFrom e1) (
                                    do
                                        ub <- expressionParser
                                        return (CSP.ListEnumFromTo e1 ub)),
                            do
                                es <- option [] 
                                        (comma >> commaSep expressionParser)
                                option (CSP.List (e1:es)) (
                                    do
                                        lexeme (string "|")
                                        stmts <- commaSep1 stmtParser
                                        return (CSP.ListComp (e1:es) stmts))])))
            
        set = 
            braces (
                bars (
                    do
                        es <- commaSep expressionParser
                        option (CSP.SetEnum es) (try $
                            do
                                lexeme (string "|")
                                stmts <- commaSep1 stmtParser
                                return $ CSP.SetEnumComp es stmts))
            <|> option (CSP.Set []) (
                do
                    e1 <- expressionParser
                    choice [
                        do
                            lexeme (string "..")
                            option (CSP.SetEnumFrom e1) (
                                do
                                    ub <- expressionParser
                                    return (CSP.SetEnumFromTo e1 ub)),
                        do
                            es <- option [] 
                                    (comma >> commaSep expressionParser)
                            option (CSP.Set (e1:es)) (
                                do
                                    lexeme (string "|")
                                    stmts <- commaSep1 stmtParser
                                    return (CSP.SetComp (e1:es) stmts))]))

        letExp :: Parser (CSP.Exp CSP.UnRenamedName CSP.Process)
        letExp =
            do
                reserved "let"                  
                ds <- many1 declarationParser
                reserved "within"
                e <- expressionParser
                return $ CSP.Let ds e
            
        ifExp =
            do
                reserved "if"
                cond <- expressionParser
                reserved "then"
                e1 <- expressionParser
                reserved "else"
                e2 <- expressionParser
                return $ CSP.If cond e1 e2
        
        builtInOps useAngles =
            if useAngles then
                angleOps++normalOps
            else normalOps
            where
                angleOps =
                    [booleanOp ">" CSP.GreaterThan E.AssocNone 4,
                    booleanOp ">=" CSP.GreaterThanEq E.AssocNone 4]
                normalOps = 
                    [functionApp 0,
                    binaryOp "." CSP.DotApp E.AssocLeft 3,
                    mathsOp "/" CSP.Divide E.AssocLeft 2,
                    mathsOp "%" CSP.Mod E.AssocLeft 2,
                    mathsOp "*" CSP.Times E.AssocLeft 2,
                    mathsOp "+" CSP.Plus E.AssocLeft 2,
                    mathsOp "-" CSP.Minus E.AssocLeft 2,
                    prefixOp "-" (CSP.MathsUnaryOp CSP.Negate) 1,
                    booleanOp "<" CSP.LessThan E.AssocNone 4,
                    booleanOp "<=" CSP.LessThanEq E.AssocNone 4,
                    prefixOp "not" (CSP.BooleanUnaryOp CSP.Not) 1,
                    booleanOp "and" CSP.And E.AssocNone 6,
                    booleanOp "or" CSP.Or E.AssocNone 6,
                    booleanOp "==" CSP.Equals E.AssocNone 4,
                    booleanOp "!=" CSP.NotEquals E.AssocNone 4,
                    binaryOp "^" CSP.Concat E.AssocNone 3,
                    prefixOp "#" CSP.ListLength 3]

                mathsOp pat sem assoc int = 
                    binaryOp pat (CSP.MathsBinaryOp sem) assoc int
                booleanOp pat sem assoc int = 
                    binaryOp pat (CSP.BooleanBinaryOp sem) assoc int
                binaryOp pat sem assoc int =
                    (int, E.Infix 
                        (annotateFunctionParser2 freshType 
                            (reservedOp pat >> return sem)) assoc)
                prefixOp pat sem int =
                    (int, E.Prefix (annotateFunctionParser freshType 
                        (reservedOp pat >> return sem)))
                functionApp int =
                    (int, E.Postfix (annotateFunctionParser freshType $
                        try (do
                            args:rest <- 
                                many (parens(commaSep expressionParser))
                            freshPTypes <- liftIO $ replicateM (length rest) freshType
                            -- TODO
                            -- The below allows us to differentiate between
                            -- x = y(a,b) ... and
                            --
                            -- x = y
                            -- (a,b) = ...
                            notFollowedBy (reservedOp "=" >> return '=')
                            
                            let mkApplication (func @ (CSP.An srcloc _ _)) =
                                    foldr (\ (typ, args) f -> 
                                                CSP.App 
                                                    (CSP.An srcloc typ f) 
                                                    args)
                                        (CSP.App func args) (zip freshPTypes rest)
                                mkApplicationOrOperator (f @ (CSP.An _ _ node)) =
                                    case node of
                                        CSP.Process (CSP.UserOperator n [] opdefn) ->
                                            if length rest /= 0 then error "TODO"
                                            else
                                                CSP.Process $ CSP.UserOperator n args opdefn
                                        _   -> mkApplication f
                            return mkApplication)))
        
        freshType :: IO (Maybe CSP.Type, CSP.PType)
        freshType = do
            t <- CSP.freshPType
            return (Nothing, t)

        infixOperatorParser =
            do
                useAngles <- gets canUseAngles
                ops <- gets operatorSyntax
                repOps <- gets repOperatorSyntax
                typ <- CSP.freshPType
                opdefn <- gets operatorDefinition
                let srcloc = CSP.SrcLoc "" 0 0
                constructParseTable (builtInOps useAngles) ops repOps
                        (annotateFunctionParser2 freshType (
                            return (\ s es -> CSP.Process (CSP.UserOperator s es opdefn))))
                        (annotateFunctionParser3 freshType (
                            return (\ s gens args -> 
                                CSP.Process (CSP.ReplicatedUserOperator s args gens opdefn))))
                        term
                        (annotateNullParser (
                            try (do
                                    pat <- patternParser
                                    lexeme (string ":")
                                    exp <- expressionParser
                                    return (CSP.Generator pat exp)
                            )
                            <|> liftM CSP.Qualifier expressionParser))
        
        -- TODO: look at trys in above + OperatorParsers.lhs
        lambdaFunction =
            do
                symbol "\\"
                pat <- patternParser
                reservedOp "@"
                exp <- expressionParser
                return (CSP.Lambda pat exp)
        
        tupleOrExpressionParser :: Parser CSP.CustomExp
        tupleOrExpressionParser =
            do
                e1 <- expressionParser
                option e1 (annotateTypeParser $ do
                    comma
                    es <- commaSep1 expressionParser
                    return $ CSP.Tuple (e1:es))
        
        variableParser =
            do
                opdefn <- gets operatorDefinition
                ops <- gets operatorSyntax
                let opNames = map (\ (Name s, _) -> s) ops
                n <- nameParser
                return $ 
                    if show n `elem` opNames then 
                        CSP.Process (CSP.UserOperator (Name (show n)) [] opdefn)
                    else CSP.Var n
                
        term :: Parser CSP.CustomExp
        term =  choice [allowAngles $ parens tupleOrExpressionParser,
                        annotateTypeParser letExp,
                        annotateTypeParser ifExp,
                        annotateTypeParser lambdaFunction,
                        annotateTypeParser sequence,
                        annotateTypeParser set,
                        annotateTypeParser $ liftM CSP.Lit literalParser,
                        annotateTypeParser variableParser]
        in
            try (infixOperatorParser) <|> term

stmtParser :: Parser CSP.CustomStmt
stmtParser = annotateNullParser (
    try (do
            pat <- patternParser
            lexeme (string "<-")
            exp <- expressionParser
            return (CSP.Generator pat exp)
    )
    <|> liftM CSP.Qualifier expressionParser)

literalParser :: Parser CSP.Literal
literalParser =
    liftM (CSP.Int . fromIntegral) natural
    <|> (try (reserved "true") >> (return $ CSP.Bool True))
    <|> (try (reserved "false") >> (return $ CSP.Bool False))
