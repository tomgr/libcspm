{-# LANGUAGE TypeSynonymInstances #-}
module CSPM (
	module CSPM.DataStructures.Names,
	module CSPM.DataStructures.Syntax,
	module CSPM.DataStructures.Types,
	CSPM, CSPMMonad, unCSPM,
	CSPMSession, newCSPMSession, getSession, setSession, withSession,
	parse, stringFileParser, interactiveStmtParser, fileParser, 
	expressionParser,
	typeCheck, interactiveStmtTypeChecker, expressionTypeChecker,
	fileTypeChecker,
	typeOfExpression,
	loadFile,
	evaluateExp, evaluateInteractiveStmt,
	getBoundNames,
)
where

import Control.Monad.State
import Control.Monad.Trans
import System.FilePath
import System.IO

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import qualified CSPM.Evaluator as EV
import CSPM.Evaluator.Values
import qualified CSPM.Parser as P
import qualified CSPM.TypeChecker as TC
import qualified CSPM.Desugar as DS
import Util.Annotated
import Util.PrettyPrint

data CSPMSession = CSPMSession {
		tcState :: TC.TypeInferenceState,
		evState :: EV.EvaluationState
	}

newCSPMSession :: MonadIO m => m CSPMSession
newCSPMSession = do
	-- Get the type checker environment with the built in functions already
	-- injected
	tcState <- liftIO $ TC.initTypeChecker
	let evState = EV.initEvaluator
	return $ CSPMSession tcState evState

class (MonadIO m) => CSPMMonad m where
	getSession :: m CSPMSession
	setSession :: CSPMSession -> m ()
	
withSession :: CSPMMonad m => (CSPMSession -> m a) -> m a
withSession f = getSession >>= f

modifySession :: CSPMMonad m => (CSPMSession -> CSPMSession) -> m ()
modifySession f = do
	s <- getSession
	setSession (f s)

-- A basic implementation
type CSPM = StateT CSPMSession IO

unCSPM :: CSPMSession -> CSPM a -> IO (a, CSPMSession)
unCSPM = flip runStateT
	
instance CSPMMonad CSPM where
	getSession = get
	setSession = put

-- General API

-- Parser API
type Parser a = (String, P.ParseMonad a)

parse :: CSPMMonad m => Parser a -> m a
parse (dir, p) = liftIO $ P.runParser p dir

fileParser :: String -> Parser [PModule]
fileParser fp = 
	let (dir, fname) = splitFileName fp
	in (dir, P.parseFile fname)
	
stringFileParser :: String -> Parser [PModule]
stringFileParser str = ("", P.parseStringAsFile str)

interactiveStmtParser :: String -> Parser PInteractiveStmt
interactiveStmtParser str = ("", P.parseInteractiveStmt str)

expressionParser :: String -> Parser PExp
expressionParser str = ("", P.parseExpression str)

-- TypeChecker API
-- All the type checkers also perform desugaring
type TypeChecker a = TC.TypeCheckMonad a

runTypeCheckerInCurrentState :: CSPMMonad m => TC.TypeCheckMonad a -> m a
runTypeCheckerInCurrentState p = withSession $ \s -> do
	(a, st) <- liftIO $ TC.runFromStateToState (tcState s) p
	modifySession (\s -> s { tcState = st })
	return a

typeCheck :: CSPMMonad m => TypeChecker a -> m a
typeCheck p = runTypeCheckerInCurrentState p

fileTypeChecker :: [PModule] -> TypeChecker [TCModule]
fileTypeChecker ms = do
	ms <- TC.typeCheckModules ms
	return $ DS.desugar ms

interactiveStmtTypeChecker :: PInteractiveStmt -> TypeChecker TCInteractiveStmt
interactiveStmtTypeChecker pstmt = do
	stmt <- TC.typeCheckInteractiveStmt pstmt
	return $ DS.desugar stmt

expressionTypeChecker :: PExp -> TypeChecker TCExp
expressionTypeChecker exp = do
	e <- TC.typeCheckExp exp
	return $ DS.desugar e

-- | Gets the type of the expression in the current context.
typeOfExpression :: CSPMMonad m => PExp -> m Type
typeOfExpression = typeCheck . TC.typeOfExp

-- Evaluator API
runEvaluatorInCurrentState :: CSPMMonad m => EV.EvaluationMonad a -> m a
runEvaluatorInCurrentState p = withSession $ \s -> do
	let (a, st) = EV.runFromStateToState (evState s) p
	modifySession (\s -> s { evState = st })
	return a

-- Environment API
getBoundNames :: CSPMMonad m => m [Name]
getBoundNames = runEvaluatorInCurrentState EV.getBoundNames 

-- | Evaluates the stmt in the current context. Extends the context with the
-- variables. Returns a State
evaluateInteractiveStmt :: CSPMMonad m => TCInteractiveStmt -> m (Maybe Value)
evaluateInteractiveStmt tcStmt = withSession $ \s -> do
	case unAnnotate tcStmt of 
		Evaluate exp -> do
			val <- evaluateExp exp
			return $ Just val
		Bind decl -> do
			evSt <- runEvaluatorInCurrentState (do
				EV.addToEnvironment (EV.evaluateDecl decl))
			modifySession (\s -> s { evState = evSt })
			return Nothing

-- | Loads the specified file into the evaluators' environment.
evaluateFile :: CSPMMonad m => [TCModule] -> m [(Name, Value)]
evaluateFile ms = runEvaluatorInCurrentState (EV.evaluateFile ms)

-- | Evaluates the expression in the current context.
evaluateExp :: CSPMMonad m => TCExp -> m Value
evaluateExp e = runEvaluatorInCurrentState (EV.evaluateExp e)

loadFile :: CSPMMonad m => [TCModule] -> m ()
loadFile ms = do
	-- Bind
	evSt <- runEvaluatorInCurrentState (EV.addToEnvironment (EV.evaluateFile ms))
	modifySession (\s -> s { evState = evSt })
	return ()
