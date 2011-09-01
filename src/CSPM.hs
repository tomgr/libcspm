{-# LANGUAGE TypeSynonymInstances #-}
module CSPM (
    module CSPM.DataStructures.Names,
    module CSPM.DataStructures.Syntax,
    module CSPM.DataStructures.Types,
    -- * CSPM Monad
    CSPMSession, newCSPMSession,
    CSPMMonad,
    getSession, setSession, handleWarnings,
    withSession,
    -- ** A basic implementation of the monad
    CSPM, unCSPM,
    -- * Parser API
    parseStringAsFile, parseFile, parseInteractiveStmt, parseExpression,
    -- * Type Checker API
    typeCheckFile, typeCheckInteractiveStmt, typeCheckExpression, ensureExpressionIsOfType,
    dependenciesOfExp, typeOfExpression,
    -- * Evaluator API
    evaluateExp, 
    bindFile, bindDeclaration, getBoundNames,
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
import Util.Exception
import Util.PrettyPrint

-- | A `CSPMSession` represents the internal states of all the various
-- components.
data CSPMSession = CSPMSession {
        tcState :: TC.TypeInferenceState,
        evState :: EV.EvaluationState
    }

-- | Create a new `CSPMSession`.
newCSPMSession :: MonadIO m => m CSPMSession
newCSPMSession = do
    -- Get the type checker environment with the built in functions already
    -- injected
    tcState <- liftIO $ TC.initTypeChecker
    let evState = EV.initEvaluator
    return $ CSPMSession tcState evState

-- | The CSPMMonad is the main monad in which all functions must be called.
-- Whilst there is a build in representation (see `CSPM`) it is recommended
-- that you define an instance of CSPMMonad over whatever monad you use.
class (MonadIO m) => CSPMMonad m where
    getSession :: m CSPMSession
    setSession :: CSPMSession -> m ()
    handleWarnings :: [ErrorMessage] -> m ()
    
withSession :: CSPMMonad m => (CSPMSession -> m a) -> m a
withSession f = getSession >>= f

modifySession :: CSPMMonad m => (CSPMSession -> CSPMSession) -> m ()
modifySession f = do
    s <- getSession
    setSession (f s)

reportWarnings :: CSPMMonad m => m(a, [ErrorMessage]) -> m a
reportWarnings prog = withSession $ \ sess -> do
    (v, ws) <- prog
    if ws == [] then return ()
    else handleWarnings ws
    return v

-- A basic implementation
type CSPM = StateT CSPMSession IO

unCSPM :: CSPMSession -> CSPM a -> IO (a, CSPMSession)
unCSPM = flip runStateT
    
instance CSPMMonad CSPM where
    getSession = get
    setSession = put
    handleWarnings ms = liftIO $ putStrLn $ show $ prettyPrint ms

-- Parser API
parse :: CSPMMonad m => FilePath -> P.ParseMonad a -> m a
parse dir p = liftIO $ P.runParser p dir

-- | Parse a file `fp`. Throws a `SourceError` on any parse error.
parseFile :: CSPMMonad m => FilePath -> m [PModule]
parseFile fp =
    let (dir, fname) = splitFileName fp
    in parse dir (P.parseFile fname)

-- | Parses a string, treating it as though it were a file. Throws a 
-- `SourceError` on any parse error.
parseStringAsFile :: CSPMMonad m => String -> m [PModule]
parseStringAsFile str = parse "" (P.parseStringAsFile str)

-- | Parses an `InteractiveStmt`. Throws a `SourceError` on any parse error.
parseInteractiveStmt :: CSPMMonad m => String -> m PInteractiveStmt
parseInteractiveStmt str = parse "" (P.parseInteractiveStmt str)

-- | Parses an `Exp`. Throws a `SourceError` on any parse error.
parseExpression :: CSPMMonad m => String -> m PExp
parseExpression str = parse "" (P.parseExpression str)

-- TypeChecker API
-- All the type checkers also perform desugaring
runTypeCheckerInCurrentState :: CSPMMonad m => TC.TypeCheckMonad a -> m (a, [ErrorMessage])
runTypeCheckerInCurrentState p = withSession $ \s -> do
    (a, ws, st) <- liftIO $ TC.runFromStateToState (tcState s) p
    modifySession (\s -> s { tcState = st })
    return (a, ws)

-- | Type checks a file, also desugaring it. Throws a `SourceError`
-- if an error is encountered and will call handleWarnings 
typeCheckFile :: CSPMMonad m => [PModule] -> m [TCModule]
typeCheckFile ms = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheck ms
    return $ DS.desugar ms

-- | Type checks a `InteractiveStmt`.
typeCheckInteractiveStmt :: CSPMMonad m => PInteractiveStmt -> m TCInteractiveStmt
typeCheckInteractiveStmt pstmt = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheck pstmt
    return $ DS.desugar pstmt

typeCheckExpression :: CSPMMonad m => PExp -> m TCExp
typeCheckExpression exp = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheck exp
    return $ DS.desugar exp

ensureExpressionIsOfType :: CSPMMonad m => Type -> PExp -> m TCExp
ensureExpressionIsOfType t exp = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheckExpect t exp
    return $ DS.desugar exp

-- | Gets the type of the expression in the current context.
typeOfExpression :: CSPMMonad m => PExp -> m Type
typeOfExpression exp = 
    reportWarnings $ runTypeCheckerInCurrentState (TC.typeOfExp exp)

dependenciesOfExp :: CSPMMonad m => TCExp -> m [Name]
dependenciesOfExp e = 
    reportWarnings $ runTypeCheckerInCurrentState (TC.dependenciesOfExp e)

-- Evaluator API
runEvaluatorInCurrentState :: CSPMMonad m => EV.EvaluationMonad a -> m a
runEvaluatorInCurrentState p = withSession $ \s -> do
    let (a, st) = EV.runFromStateToState (evState s) p
    modifySession (\s -> s { evState = st })
    return a

-- Environment API

-- | Get a list of currently bound names in the environment.
getBoundNames :: CSPMMonad m => m [Name]
getBoundNames = runEvaluatorInCurrentState EV.getBoundNames 

-- | Takes a declaration and adds it to the current environment.
bindDeclaration :: CSPMMonad m => TCDecl -> m ()
bindDeclaration d = withSession $ \s -> do
    evSt <- runEvaluatorInCurrentState (EV.addToEnvironment (EV.evaluateDecl d))
    modifySession (\s -> s { evState = evSt })

-- | Binds all the declarations that are in a particular file.
bindFile :: CSPMMonad m => [TCModule] -> m ()
bindFile ms = do
    -- Bind
    evSt <- runEvaluatorInCurrentState (EV.addToEnvironment (EV.evaluateFile ms))
    modifySession (\s -> s { evState = evSt })
    return ()

-- | Returns a list of all declarations in the specified file.
evaluateFile :: CSPMMonad m => [TCModule] -> m [(Name, Value)]
evaluateFile ms = runEvaluatorInCurrentState (EV.evaluateFile ms)

-- | Evaluates the expression in the current context.
evaluateExp :: CSPMMonad m => TCExp -> m Value
evaluateExp e = runEvaluatorInCurrentState (EV.evaluateExp e)
