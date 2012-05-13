module CSPM.Operators.Custom.OpSemParser (parseOpSemFile) where

import CSPM.Operators.Custom.OpSemDataStructures
import CSPM.Operators.Custom.OperatorParsers
import Util.Exception
import Util.PartialFunctions

import Control.Monad(liftM, sequence)
import Control.Monad.Error
import Data.List
import qualified Text.Parsec.Expr as E
import Text.Parsec.Language
import Text.Parsec
import qualified Text.Parsec.Token as PT

type Parser = Parsec String (PartialFunction Name (Maybe OperatorSyntax))

dotSep p = sepBy p dot
braces = between (symbol "{") (symbol "}")

opSemLanguage = PT.LanguageDef {
        PT.commentStart = "/*",
        PT.commentEnd = "*/",
        PT.commentLine = "//",
        PT.nestedComments = False,
        PT.identStart = letter,
        PT.identLetter = alphaNum <|> char '\'' <|> char '_',
        -- TODO: these
        -- TODO: deifne set operators
        PT.opStart = oneOf "[]|/\\-<>{}~=@",
        PT.opLetter = oneOf "[]|/\\-<>{}~=@",
        PT.reservedOpNames = ["=>", "==", "!=", "<="],
        PT.reservedNames = [
                            -- Section delimeters
                            "Rule", "EndRule", "Operator", "EndOperator",
                            -- Syntax rules
                            "Syntax", "Binary", 
                            -- Expressions
                            "Union",
                            "Sigma", "union", "diff", "inter", "Set",
                            -- Side conditions
                            "member", "true", "false",
                            -- Events
                            "tau",
                            "Identity",
                            "Replicated", "EndReplicated",
                            "BaseCase", "EndBaseCase",
                            "InductiveCase", "EndInductiveCase",
                            "Channels", "EndChannels"],
                -- TODO: need to stop people from using Replicated as a prefix
                -- to any of their operators
        PT.caseSensitive = True
    }
-- TODO: channels
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
            PT.commaSep1 = commaSep1} = PT.makeTokenParser opSemLanguage

parseOpSemFile :: String -> IO InputOpSemDefinition
parseOpSemFile fname = 
    let
        pass1 = 
            do
                whiteSpace
                option [] channelSectionParser
                whiteSpace
                operatorParseInfo <- many operatorPhase1Parser
                eof
                return operatorParseInfo
        pass2 = 
            do
                whiteSpace
                chans <- option [] channelSectionParser
                whiteSpace
                ops <- many operatorParser
                eof
                return $ InputOpSemDefinition ops chans
    in
        do 
            input <- liftIO $ readFile fname
            case runP pass1 [] fname input of
                Left err -> panic (show err)
                Right opParseInfo   -> 
                    case runP pass2 opParseInfo fname input of
                        Left err -> panic (show err)
                        Right ops -> return ops

channelSectionParser :: Parser [Channel]
channelSectionParser =
    do
        reserved "Channels"
        chans <- many (do
                        name <- identifier
                        typ <- option [] (do
                                    lexeme (string ":")
                                    components <- dotSep expressionParser
                                    return components)
                        return $ Channel (Name name) typ
            )
        reserved "EndChannels"
        return chans

operatorPhase1Parser :: Parser (Name, Maybe OperatorSyntax)
operatorPhase1Parser = 
    do
        reserved "Operator"
        friendlyName <- identifier
        args <- option [] (parens (commaSep nameSubtypeParser))
        syntax <- option Nothing (liftM Just syntaxParser)
        manyTill anyChar (reserved "EndOperator")
        return (Name friendlyName, syntax)

operatorParser :: Parser InputOperator
operatorParser =
    do
        reserved "Operator"
        friendlyName <- identifier
        args <- option [] (parens (commaSep nameSubtypeParser))
        syntax <- option Nothing (liftM Just syntaxParser)
        rules <- many ruleParser
        replicatedOp <- option Nothing (liftM Just replicatedOpParser)
        reserved "EndOperator"
        return $ InputOperator (Name friendlyName) 
                                args
                                rules 
                                replicatedOp 
                                syntax

nameSubtypeParser :: Parser (Name, ProcessSubtype)
nameSubtypeParser = 
    do
        id <- identifier 
        st <- ((lexeme (string ":") >>
                choice [
                    lexeme (string "FinRec") >> return FinitelyRecursive,
                    lexeme (string "InfRec") >> return InfinitelyRecursive])
                <|> return Unknown)
        return (Name id, st)

replicatedOpParser :: Parser InputReplicatedOperator
replicatedOpParser =
    do
        reserved "Replicated"
        args <- option [] (parens (commaSep identifier))
        syntax <- option Nothing (liftM Just syntaxParser)
        
        reserved "BaseCase"
        basePats <- parens (commaSep patternParser)
        baseCase <- expressionParser
        reserved "EndBaseCase"
        reserved "InductiveCase"
        recursiveArgs <- parens (commaSep identifier)
        inductiveCase <- expressionParser
        reserved "EndInductiveCase"
        
        reserved "EndReplicated"

        return $ InputReplicatedOperator (map Name args) (basePats, baseCase) 
                    (map Name recursiveArgs, inductiveCase) syntax
        
ruleParser :: Parser InductiveRule
ruleParser =
    do
        reserved "Rule"
        pres <- many (processRelationParser False)
        lexeme (skipMany1 (char '-'))
        scs <- commaSep (try sideConditionParser)
        whiteSpace
        post <- processRelationParser True
        reserved "EndRule"
        return (InductiveRule pres post scs)

syntaxParser :: Parser OperatorSyntax
syntaxParser = 
    let
        patternParser = between (lexeme (char '"')) (lexeme (char '"'))
                            (many parseComponentParser)
    in
        reserved "Syntax" >>
        choice [
            do
                reserved "Binary"
                pattern <- patternParser
                precedence <- natural
                string "Assoc"
                associativity <-
                    ((lexeme (string "Left") >> return AssocLeft)
                        <|> (lexeme (string "Right") >> return AssocRight)
                        <|> (lexeme (string "None") >> return AssocNone)
                        <?> "associativity specification")
                return $ InfixOp pattern precedence associativity,
            do
                reserved "Prefix"
                pattern <- patternParser
                precedence <- natural
                return $ PrefixOp pattern precedence,
            do
                reserved "Postfix"
                pattern <- patternParser
                precedence <- natural
                return $ PostfixOp pattern precedence]

parseComponentParser :: Parser ParseComponent
parseComponentParser =
    (try (lexeme (string "\\$" >> return (String "$"))))
    <|> (do
        char '$'
        id <- natural
        return (Argument id))
    <|> (operator >>= (\ n -> return (String n)))
    <|> (identifier >>= (\ n -> return (String n)))

processRelationParser :: Bool -> Parser ProcessRelation
processRelationParser isPost =
    do
        e1 <- expressionParser
        lexeme (string "=")
        ev <- eventParser
        reservedOp "=>"
        e2 <- expressionParser
        case (isPost,e2) of
            (True, Var n)   -> return (Performs e1 ev 
                                        (OperatorApp (Name "Identity") [e2]))
            _               -> return (Performs e1 ev e2)

expressionParser :: Parser Exp
expressionParser =
    do 
        st <- getState
        (let
            parseFunctionCall1 s e = 
                do
                    reserved s
                    e1 <- parens (expressionParser)
                    return (e e1)
            parseFunctionCall2 s e = 
                do
                    reserved s
                    [e1,e2] <- parens (commaSep expressionParser)
                    return (e e1 e2)
                                    
            normalOperatorNames = [s | (s, _) <- st]
            exprParser = constructParseTable [] st []
                            (return (\ n es -> OperatorApp n es)) 
                            (error "generator sem called")
                            term
                            (error "generator called")

            term = choice [
                    try (liftM Tuple (parens (commaSep expressionParser))),
                    parens expressionParser,
                    braces (
                        do
                            exps <- commaSep expressionParser
                            option (Set exps) (do
                                symbol "|"
                                scs <- commaSep1 sideConditionParser
                                return $ SetComprehension exps scs)
                    ),
                    try (
                        do 
                            opName <- liftM Name identifier
                            args <- option [] (parens (commaSep expressionParser))
                            if (elem opName normalOperatorNames) then 
                                    return (OperatorApp opName args)
                                -- TODO: problem: error swalloed by try
                                -- but try necessary since we consume an identifier
                                else (unexpected ("operator "++show opName++
                                                    " not recognized"))
                            ),
                    -- identifier uses try, hence we can use it here and it will
                    -- not consume any input
                    liftM (Var . Name) identifier,
                    (reserved "Sigma" >> return Sigma),
                    (reserved "InductiveCase" >> return InductiveCase),
                    parseFunctionCall1 "Set" Powerset,
                    parseFunctionCall1 "Union" ReplicatedUnion,
                    parseFunctionCall2 "union" Union,
                    parseFunctionCall2 "inter" Intersection,
                    parseFunctionCall2 "diff" SetMinus]
                    <?> "expression"
            in
                try exprParser
                <|> term)


sideConditionParser :: Parser SideCondition
sideConditionParser =
    let
        table = [
            [E.Prefix (prefix "!" Not)],
            [E.Infix (binary "&&" And) E.AssocLeft,
             E.Infix (binary "||" Or) E.AssocLeft]
            ]
        prefix s e = lexeme (string s) >> return e
        binary s e = lexeme (string s) >> return e
        
        booleanParser = E.buildExpressionParser table term
        term =
            parens booleanParser
            <|> try (reserved "true" >> return PTrue)
            <|> try (reserved "false" >> return PFalse)
            <|> try (do
                    reserved "member"
                    parens (
                        do
                            p <- patternParser
                            comma
                            e <- expressionParser
                            return (Member p e)))
            <|> try (do
                    e1 <- expressionParser
                    lexeme (reservedOp "==")
                    e2 <- expressionParser
                    return (Equals e1 e2))
            <|> try (do
                    e1 <- expressionParser
                    lexeme (reservedOp "!=")
                    e2 <- expressionParser
                    return (Not (Equals e1 e2)))
            <|> try (do
                    e1 <- expressionParser
                    lexeme (reservedOp "<=")
                    e2 <- expressionParser
                    return (Subset e1 e2))
            <?> "boolean term"                  
    in
        try (do
            pat <- patternParser
            lexeme (string "<-")
            exp <- expressionParser
            return (SCGenerator pat exp))
        <|>
            liftM Formula booleanParser

patternParser :: Parser Pattern
patternParser =
    liftM PTuple (parens (commaSep patternParser))
    <|> liftM PSet (braces (commaSep patternParser))
    <|> liftM (PVar . Name) identifier

eventParser :: Parser Event
eventParser =
    try (reserved "tau" >> return Tau)
    <|> do 
            chanName <- identifier
            option (Event (Name chanName)) (do
                dot
                dataComponents <- dotSep expressionParser
                return $ ChanEvent (Name chanName) dataComponents)
