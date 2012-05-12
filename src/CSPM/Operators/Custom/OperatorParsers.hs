module OperatorParsers where

import OpSemDataStructures

import Char
import Control.Monad.Identity
import Control.Monad(liftM, sequence)
import List
import Text.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Expr as E

constructParseTable :: (Monad m) => 
	[(Integer, E.Operator String u m a)] ->
	[(Name, Maybe OperatorSyntax)] ->
	[(Name, Maybe OperatorSyntax)] ->
	ParsecT String u m (Name -> [a] -> a) ->
	ParsecT String u m (Name -> [b] -> [a] -> a) ->
	ParsecT String u m a -> 
	ParsecT String u m b ->
	ParsecT String u m a
constructParseTable builtInOps ops replicatedOps semAction replicatedSemAction 
					expParser generatorParser =
	let
		constructParser comps =
			let
				parser (Argument n) =
					do	-- TODO: should this be recursive like this?
						e <- (infixParser <|> expParser)
						return [(n, e)]
				parser (String s) =
					do
						string s
						skipMany (satisfy isSpace)
						return []
				processResults = 
					map snd . sortBy (\(n1,_) (n2,_) -> compare n1 n2) . concat
			in liftM processResults (sequence . map parser $ comps)

		constructReplicatedParser comps =
			let
				parser (Argument 0) =
					do
						gens <- sepBy generatorParser (string ",") 
						return (gens, [])
				parser (Argument n) =
					do	-- TODO: should this be recursive like this?
						e <- (infixParser <|> expParser)
						return ([], [(n, e)])
				parser (String s) =
					do
						string s
						skipMany (satisfy isSpace)
						return ([], [])
				processResults = 
					map snd . sortBy (\(n1,_) (n2,_) -> compare n1 n2) . concat
			in do
				(gens, args) <- liftM unzip (sequence . map parser $ comps)
				return (concat gens, processResults args)
				
		convAssociativity AssocLeft = E.AssocLeft
		convAssociativity AssocRight = E.AssocRight
		convAssociativity AssocNone = E.AssocNone
		
		getPriority (InfixOp _ n _) = n
		getPriority (PrefixOp _ n) = n
		getPriority (PostfixOp _ n) = n

		operatorParser1 n pat =
			do
				args <- constructParser pat
				res <- semAction
				return (\ e1 -> res n (e1:args))
		operatorParser2 n pat =
			do
				args <- constructParser pat
				res <- semAction
				return (\ e1 e2 -> res n (e1:e2:args))
		
		replicatedOperatorParser n pat =
			do
				(gens, args) <- constructReplicatedParser pat
				res <- replicatedSemAction
				return (\ e1 -> res n gens (e1:args))
		
		convOperator (n, InfixOp pat _ assoc) =
			E.Infix (try (operatorParser2 n pat)) (convAssociativity assoc)
		convOperator (n, PrefixOp pat _) =
			E.Prefix (try (operatorParser1 n pat))
		convOperator (n, PostfixOp pat _) =
			E.Postfix (try (operatorParser1 n pat))

		convReplicatedOperator (n, PrefixOp pat _) =
			E.Prefix (try (replicatedOperatorParser n pat))
		convReplicatedOperator (n, PostfixOp pat _) =
			E.Postfix (try (replicatedOperatorParser n pat))

		allOperators = 
			[(getPriority s, convReplicatedOperator (n, s)) 
				| (n, Just s) <- replicatedOps]
			++[(getPriority s, convOperator (n, s)) |  (n, Just s) <- ops]
			++builtInOps
		
		operatorTable = (map (map snd)
				. sortBy (\ ((e1, _):_) ((e2, _):_) -> compare e1 e2)
				. groupBy (\ (e1,_) (e2,_) -> e1 == e2)) allOperators
		
		infixParser = E.buildExpressionParser operatorTable expParser
	in
		infixParser
