{-# LANGUAGE QuasiQuotes #-}
module CSPMParser (parseCSPMFile, stringParser) where

import Control.Monad(liftM, sequence)
import Control.Monad.Trans
import Control.Monad.Error
import List
import qualified Text.Parsec.Expr as E
import Text.Parsec.Language
import Text.Parsec
import qualified Text.Parsec.Token as PT

import qualified CSPMDataStructures as CSP
import OpSemDataStructures
import OperatorParsers
import Util

data CSPMParserState = 
	CSPMParserState {
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
				--				c) rep operator definitions
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

annotateTypeParser :: Parser a -> Parser (CSP.Annotated CSP.PType a)
annotateTypeParser = annotateParser CSP.freshPType

annotateTypeSchemeParser :: Parser a -> Parser (CSP.Annotated CSP.PTypeScheme a)
annotateTypeSchemeParser = annotateParser CSP.freshPTypeScheme

annotateNullParser :: Parser a -> Parser (CSP.Annotated () a)
annotateNullParser = annotateParser (return ())

annotateParser :: IO c -> Parser a -> Parser (CSP.Annotated c a)
annotateParser mtyp p = 
	do
		typ <- lift	mtyp
		startPos <- getPosition
		inner <- p
		endPos <- getPosition
		let srcloc = (CSP.SrcLoc (sourceName startPos) 
						(sourceLine startPos) (sourceColumn startPos))
		return $ CSP.Annotated srcloc typ inner

annotateFunctionParser 
	:: IO c -> Parser (a -> b) -> Parser (a -> CSP.Annotated c b)
annotateFunctionParser mtyp p =
	do
		typ <- lift mtyp
		startPos <- getPosition
		inner <- p
		endPos <- getPosition
		let srcloc = (CSP.SrcLoc (sourceName startPos) 
						(sourceLine startPos) (sourceColumn startPos))
		return $ CSP.Annotated srcloc typ . inner
annotateFunctionParser2
	:: IO d -> Parser (a -> b -> c) -> Parser (a -> b -> CSP.Annotated d c)
annotateFunctionParser2 mtyp p =
	do
		typ <- lift mtyp
		startPos <- getPosition
		inner <- p
		endPos <- getPosition
		let srcloc = (CSP.SrcLoc (sourceName startPos) 
						(sourceLine startPos) (sourceColumn startPos))
		return $ \ e1 e2 -> CSP.Annotated srcloc typ (inner e1 e2)
annotateFunctionParser3
	:: IO e -> Parser (a -> b -> c -> d) -> Parser (a -> b -> c -> CSP.Annotated e d)
annotateFunctionParser3 mtyp p =
	do
		typ <- lift mtyp
		startPos <- getPosition
		inner <- p
		endPos <- getPosition
		let srcloc = (CSP.SrcLoc (sourceName startPos) 
						(sourceLine startPos) (sourceColumn startPos))
		return $ \ e1 e2 e3 -> CSP.Annotated srcloc typ (inner e1 e2 e3)
		
stringParser :: String -> [Operator] -> Tyger [CSP.PModule]
stringParser s ops =
	do
		let opMap = [(n, syntax) | Operator n _ _ syntax <- ops]
		let repOpMap = [(n, syntax) | ReplicatedOperator n _ _ _ syntax <- ops]
		res <- liftIO $ runParserT fileParser 
									(CSPMParserState opMap repOpMap True) 
									"<stdin>" s
		case res of
			Left err -> throwError $ CSPMParseError (show err)
			Right v -> return v

parseCSPMFile :: String -> OpSemDefinition -> Tyger [CSP.PModule]
parseCSPMFile fname opSemDef = 
	do 
		input <- liftIO $ readFile fname
		let opMap = [(n, syntax) | Operator n _ _ syntax <- operators opSemDef]
		let repOpMap = 
			[(n, syntax) | ReplicatedOperator n _ _ _ syntax <- operators opSemDef]
		res <- liftIO $ runParserT fileParser (CSPMParserState opMap repOpMap True) 
									fname input
		case res of
			Left err -> throwError $ CSPMParseError (show err)
			Right v -> return v

fileParser :: Parser [CSP.PModule]
fileParser = 
	do
		m <- annotateNullParser $
			do
				whiteSpace
				decls <- many declarationParser
				eof
				return $ CSP.GlobalModule decls
		return [m]

dotAppToList :: CSP.PExp -> [CSP.PExp]
dotAppToList (CSP.Annotated a b exp) = 
	let
		dotAppToList' (CSP.DotApp e1 e2) = dotAppToList e1++dotAppToList e2
		dotAppToList' x = [CSP.Annotated a b x]
	in
		dotAppToList' exp
	
declarationParser :: Parser CSP.PDecl
declarationParser = annotateParser CSP.freshPSymbolTable (
	(try (reserved "external") >> liftM CSP.External (commaSep nameParser))
	<|> (try (reserved "transparent") >> liftM CSP.Transparent (commaSep nameParser))
	<|> do
			try (reserved "channel")
			names <- commaSep1 nameParser
			option (CSP.Channel names []) (
				do
					symbol ":"
					exp <- expressionParser
					return $ CSP.Channel names (dotAppToList exp))
	<|> 
		do
			try (reserved "datatype")
			name <- nameParser
			lexeme (string "=")
			es <- sepBy (annotateNullParser $
					do
						name <- nameParser
						ts <- option [] (
							do
								dot
								exp <- expressionParser
								return $ dotAppToList exp)
						return $ CSP.DataTypeClause name ts) (symbol "|")
			return $ CSP.DataType name es
	<|> do
			try (reserved "assert")
			e1 <- expressionParser
			m <- modelParser
			e2 <- expressionParser
			return $ CSP.Assert e1 e2 m
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
		matches <- many1 (matchParser name)
		return $ CSP.FunBind (CSP.Name name) matches typ
	<?> "declaration")

typeAnnotationParser :: String -> Parser (Maybe [CSP.AnExp])
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

matchParser :: String -> Parser CSP.PMatch
matchParser fname = annotateNullParser $
	do
		-- Test to see if the function name is the same
		try ((lexeme . string $ fname) >> lookAhead (symbol "("))
		groups <- 
			many1 (parens (option [] (commaSep patternParser)))
		lexeme (string "=")
		exp <- expressionParser
		return $ CSP.Match groups exp

patternParser :: Parser CSP.PPat
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

nameParser :: Parser CSP.Name
nameParser = liftM CSP.Name identifier

qNameParser :: Parser CSP.QualifiedName
qNameParser = liftM (CSP.UnQual . CSP.Name) identifier

expressionParser :: Parser CSP.PExp
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
					prefixOp "-" CSP.NegApp 1,
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
						(annotateFunctionParser2 CSP.freshPType 
							(reservedOp pat >> return sem)) assoc)
				prefixOp pat sem int =
					(int, E.Prefix (annotateFunctionParser CSP.freshPType 
						(reservedOp pat >> return sem)))
				functionApp int =
					(int, E.Postfix (annotateFunctionParser CSP.freshPType $
						try (do
							args:rest <- 
								many (parens(commaSep expressionParser))
							freshPTypes <- 
								replicateM (length rest) CSP.freshPType
							-- TODO
							-- The below allows us to differentiate between
							-- x = y(a,b) ... and
							--
							-- x = y
							-- (a,b) = ...
							notFollowedBy (reservedOp "=" >> return '=')
							
							let mkApplication 
										(func @ (CSP.Annotated srcloc _ _)) =
									foldr (\ (typ, args) f -> 
												CSP.App 
													(CSP.Annotated srcloc typ f) 
													args)
										(CSP.App func args) (zip freshPTypes rest)
							let mkApplicationOrOperator 
										(f @ (CSP.Annotated _ _ node)) =
									case node of
										CSP.UserOperator n [] ->
											if length rest /= 0 then error "TODO"
											else
												CSP.UserOperator n args
										_	-> mkApplication f
							return mkApplication)))
		
		infixOperatorParser =
			do
				useAngles <- gets canUseAngles
				ops <- gets operatorSyntax
				repOps <- gets repOperatorSyntax
				typ <- CSP.freshPType
				let srcloc = CSP.SrcLoc "" 0 0
				constructParseTable (builtInOps useAngles) ops repOps
						(annotateFunctionParser2 CSP.freshPType (
							return (\ (Name s) es -> 
										CSP.UserOperator (CSP.Name s) es)))
						(annotateFunctionParser3 CSP.freshPType (
							return (\ (Name s) gens args -> 
								CSP.ReplicatedUserOperator (CSP.Name s) args gens)))
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
		
		tupleOrExpressionParser :: Parser CSP.PExp
		tupleOrExpressionParser =
			do
				e1 <- expressionParser
				option e1 (annotateTypeParser $ do
					comma
					es <- commaSep1 expressionParser
					return $ CSP.Tuple (e1:es))
		
		variableParser =
			do
				ops <- gets operatorSyntax
				let opNames = map (\ (Name s, _) -> s) ops
				(n @ (CSP.UnQual (CSP.Name s))) <- qNameParser
				if s `elem` opNames then return $ CSP.UserOperator (CSP.Name s) []
					else return $ CSP.Var n
				
		term :: Parser CSP.PExp
		term =	choice [allowAngles $ parens tupleOrExpressionParser,
						annotateTypeParser letExp,
						annotateTypeParser ifExp,
						annotateTypeParser lambdaFunction,
						annotateTypeParser sequence,
						annotateTypeParser set,
						annotateTypeParser $ liftM CSP.Lit literalParser,
						annotateTypeParser variableParser]
		in
			try (infixOperatorParser) <|> term

stmtParser :: Parser CSP.PStmt
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
	liftM CSP.Int natural
	<|> (try (reserved "true") >> (return $ CSP.Bool True))
	<|> (try (reserved "false") >> (return $ CSP.Bool False))
