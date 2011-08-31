{-# LANGUAGE TypeSynonymInstances #-}
module CSPM (
    module CSPM.DataStructures.Names,
    module CSPM.DataStructures.Syntax,
    module CSPM.DataStructures.Types,
    CSPM, CSPMMonad, unCSPM,
    CSPMSession, newCSPMSession, getSession, setSession, withSession,
    parseStringAsFile, parseFile, parseInteractiveStmt, parseExpression,
    typeCheckFile, typeCheckInteractiveStmt, typeCheckExpression, ensureExpressionIsOfType,
    dependenciesOfExp, typeOfExpression,
    loadFile, evaluateExp, bindDeclaration,
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
parse :: CSPMMonad m => FilePath -> P.ParseMonad a -> m a
parse dir p = liftIO $ P.runParser p dir

parseFile :: CSPMMonad m => FilePath -> m [PModule]
parseFile fp =
    let (dir, fname) = splitFileName fp
    in parse dir (P.parseFile fname)
    
parseStringAsFile :: CSPMMonad m => String -> m [PModule]
parseStringAsFile str = parse "" (P.parseStringAsFile str)

parseInteractiveStmt :: CSPMMonad m => String -> m PInteractiveStmt
parseInteractiveStmt str = parse "" (P.parseInteractiveStmt str)

parseExpression :: CSPMMonad m => String -> m PExp
parseExpression str = parse "" (P.parseExpression str)

-- TypeChecker API
-- All the type checkers also perform desugaring
runTypeCheckerInCurrentState :: CSPMMonad m => TC.TypeCheckMonad a -> m a
runTypeCheckerInCurrentState p = withSession $ \s -> do
    (a, st) <- liftIO $ TC.runFromStateToState (tcState s) p
    modifySession (\s -> s { tcState = st })
    return a

typeCheckFile :: CSPMMonad m => [PModule] -> m [TCModule]
typeCheckFile ms = runTypeCheckerInCurrentState (TC.typeCheck ms >> return (DS.desugar ms))

typeCheckInteractiveStmt :: CSPMMonad m => PInteractiveStmt -> m TCInteractiveStmt
typeCheckInteractiveStmt pstmt = 
    runTypeCheckerInCurrentState (TC.typeCheck pstmt >> return (DS.desugar pstmt))

typeCheckExpression :: CSPMMonad m => PExp -> m TCExp
typeCheckExpression exp = 
    runTypeCheckerInCurrentState (TC.typeCheck exp >> return (DS.desugar exp))

ensureExpressionIsOfType :: CSPMMonad m => Type -> PExp -> m TCExp
ensureExpressionIsOfType t exp =
    runTypeCheckerInCurrentState (TC.typeCheckExpect t exp >> return (DS.desugar exp))

-- | Gets the type of the expression in the current context.
typeOfExpression :: CSPMMonad m => PExp -> m Type
typeOfExpression = runTypeCheckerInCurrentState . TC.typeOfExp

dependenciesOfExp :: CSPMMonad m => TCExp -> m [Name]
dependenciesOfExp e = runTypeCheckerInCurrentState (TC.dependenciesOfExp e)

-- Evaluator API
runEvaluatorInCurrentState :: CSPMMonad m => EV.EvaluationMonad a -> m a
runEvaluatorInCurrentState p = withSession $ \s -> do
    let (a, st) = EV.runFromStateToState (evState s) p
    modifySession (\s -> s { evState = st })
    return a

-- Environment API
getBoundNames :: CSPMMonad m => m [Name]
getBoundNames = runEvaluatorInCurrentState EV.getBoundNames 

bindDeclaration :: CSPMMonad m => TCDecl -> m ()
bindDeclaration d = withSession $ \s -> do
    evSt <- runEvaluatorInCurrentState (EV.addToEnvironment (EV.evaluateDecl d))
    modifySession (\s -> s { evState = evSt })

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
