{-# LANGUAGE FlexibleInstances #-}
-- | This module provides the main high-level interface to the library 
-- functionality. It does this through a monadic interface, mainly due to the
-- fact that several of the components require the use of the IO monad. It is
-- highly recommended that users of this library use a monad and then implement
-- the 'CSPMMonad' class on their own custom monad. An example of this is shown
-- by the basic implementation of the 'CSPM' monad.
--
-- The main library datatype is exported by 'CSPM.DataStructures.Syntax', which
-- provides an AST representation of machine CSP. Most of the pieces of syntax,
-- like expressions ('Exp'), are parametrised by the type of the variables that
-- it contains. For more information see the comment at the top of the above
-- module.
--
-- The library exports several APIs which, in likely order of usage, are:
-- 
--      [@Parser@] Parses strings or files and produces an AST, parametrised
--        by 'UnRenamedName', which are simply pieces of text.
--
--      [@Renamer@] Renames the AST and produces an equivalent AST, but 
--        parametrised by 'Name', which uniquely identify the binding instance
--        of each variable (see documentation of 'Name').
--
--      [@Type Checker@] Type checks an AST, in the process annotating it with
--        types.
--
--      [@Desugarer@] Desugars an AST, remove syntactic sugar and prepares it for
--        evaluation. The AST produced by this phase should not be pretty 
--        printed as it parenthesis have been removed, potentially making it not
--        equivalent.
--
--      [@Evaluator@] Evaluates an AST, returning a 'Value'. Note that the 
--        evaluator is lazy, meaning that the resulting Value will be generated
--        as it is consumed, making it suitable for streaming to subsequent
--        compilation phases.
--
-- For example, suppose we wish to evaluate the expression @test(1,2,3)@ within
-- the context of the file @test.csp@ we could use the following segment of
-- code:
--
-- >    main :: IO ()
-- >    main = do
-- >        session <- newCSPMSession
-- >        (value, resultingSession) <- unCSPM session $ do
-- >            -- Parse the file, returning something of type PModule.
-- >            parsedFile <- parseFile "test.csp"
-- >            -- Rename the file, returning something of type TCModule.
-- >            renamedFile <- renameFile parsedFile
-- >            -- Typecheck the file, annotating it with types.
-- >            typeCheckedFile <- typeCheckFile renamedFile
-- >            -- Desugar the file, returning the version ready for evaluation.
-- >            desugaredFile <- desugarFile typeCheckedFile
-- >            -- Bind the file, making all functions and patterns available.
-- >            bindFile desugaredFile
-- >            
-- >            -- The file is now ready for use, so now we build the expression
-- >            -- to be evaluated.
-- >            parsedExpression <- parseExpression "test(1,2,3)"
-- >            renamedExpression <- renameExpression parsedExpression
-- >            typeCheckedExpression <- typeCheckExpression renamedExpression
-- >            desugaredExpression <- desugarExpression typeCheckedExpression
-- >
-- >            -- Evaluate the expression in the current context.
-- >            value <- evaluateExpression desugaredExpression
-- >            return value
-- >        putStrLn (show (prettyPrint value))
-- >        return ()
--
-- This would pretty print the value of the expression to stdout.
module CSPM (
    -- * CSPM Monad
    CSPMSession, newCSPMSession,
    CSPMMonad(..),
    withSession,
    -- ** A basic implementation of the monad
    CSPM, unCSPM,
    -- * Common Data Types
    -- | Defines the names that are used by machine CSP.
    module CSPM.DataStructures.Names,
    -- | Defines the abstract syntax for machine CSP.
    module CSPM.DataStructures.Syntax,
    -- | Defines the types used by the typechecker.
    module CSPM.DataStructures.Types,
    -- | Defines the values produced by the evaluator.
    module CSPM.Evaluator.Values,
    -- * Parser API
    parseStringAsFile, parseFile, parseInteractiveStmt, parseExpression,
    -- * Renamer API
    renameFile, renameInteractiveStmt, renameExpression,
    -- * Type Checker API
    typeCheckFile, typeCheckInteractiveStmt, typeCheckExpression,
    ensureExpressionIsOfType, dependenciesOfExp, typeOfExpression,
    -- * Desugarer API
    desugarFile, desugarInteractiveStmt, desugarExpression,
    -- * Evaluator API
    bindFile, bindDeclaration, getBoundNames,
    evaluateExpression,
    -- * Low-Level API
    -- | Whilst this module provides many of the commonly used functionality 
    -- within the CSPM monad, sometimes there are additional functions exported
    -- by other modules that are of use. The following functions allow the
    -- renamer, typechecker and evaluator to be run in the current state. They
    -- also save the resulting state in the current session.
    runParserInCurrentState,
    runRenamerInCurrentState, 
    runTypeCheckerInCurrentState,
    runEvaluatorInCurrentState, 
    reportWarnings,
    -- * Misc functions
    getLibCSPMVersion,
)
where

import Control.Monad.State
import Control.Monad.Trans
import Data.Version
import System.FilePath
import System.IO

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import qualified CSPM.Evaluator as EV
import CSPM.Evaluator.Values
import qualified CSPM.Parser as P
import qualified CSPM.Renamer as RN
import qualified CSPM.TypeChecker as TC
import qualified CSPM.Desugar as DS
import Paths_libcspm (version)
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

-- | A 'CSPMSession' represents the internal states of all the various
-- components.
data CSPMSession = CSPMSession {
        -- | The state of the renamer.
        rnState :: RN.RenamerState,
        -- | The state of the type checker.
        tcState :: TC.TypeInferenceState,
        -- | The state of the evaluator.
        evState :: EV.EvaluationState
    }

-- | Create a new 'CSPMSession'.
newCSPMSession :: MonadIO m => m CSPMSession
newCSPMSession = do
    -- Get the type checker environment with the built in functions already
    -- injected
    rnState <- liftIO $ RN.initRenamer
    tcState <- liftIO $ TC.initTypeChecker
    let evState = EV.initEvaluator
    return $ CSPMSession rnState tcState evState

-- | The CSPMMonad is the main monad in which all functions must be called.
-- Whilst there is a build in representation (see 'CSPM') it is recommended
-- that you define an instance of 'CSPMMonad' over whatever monad you use.
class (MonadIO m) => CSPMMonad m where
    -- | Get the current session.
    getSession :: m CSPMSession
    -- | Update the current session.
    setSession :: CSPMSession -> m ()
    -- | This is called whenever warnings are emitted.
    handleWarnings :: [ErrorMessage] -> m ()

-- | Executes an operation giving it access to the current 'CSPMSession'.
withSession :: CSPMMonad m => (CSPMSession -> m a) -> m a
withSession f = getSession >>= f

-- | Modifies the session using the given function.
modifySession :: CSPMMonad m => (CSPMSession -> CSPMSession) -> m ()
modifySession f = do
    s <- getSession
    setSession (f s)

-- | Given a program that can return warnings, runs the program and raises
-- any warnings found using 'handleWarnings'.
reportWarnings :: CSPMMonad m => m (a, [ErrorMessage]) -> m a
reportWarnings prog = withSession $ \ sess -> do
    (v, ws) <- prog
    when (ws /= []) $ handleWarnings ws
    return v

-- | A basic implementation of 'CSPMMonad', using the 'StateT' monad. This
-- prints out any warnings to stdout.
type CSPM = StateT CSPMSession IO

-- | Runs a 'CSPM' function, returning the result and the resulting session.
unCSPM :: CSPMSession -> CSPM a -> IO (a, CSPMSession)
unCSPM = flip runStateT

instance CSPMMonad CSPM where
    getSession = get
    setSession = put
    handleWarnings ms = liftIO $ putStrLn $ show $ prettyPrint ms

-- | Runs the parser.
runParserInCurrentState :: CSPMMonad m => FilePath -> P.ParseMonad a -> m a
runParserInCurrentState dir p = liftIO $ P.runParser p dir

-- | Parse a file `fp`. Throws a `SourceError` on any parse error.
parseFile :: CSPMMonad m => FilePath -> m [PModule]
parseFile fp =
    let (dir, fname) = splitFileName fp
    in runParserInCurrentState dir (P.parseFile fname)

-- | Parses a string, treating it as though it were a file. Throws a 
-- 'SourceError' on any parse error.
parseStringAsFile :: CSPMMonad m => String -> m [PModule]
parseStringAsFile str = runParserInCurrentState "" (P.parseStringAsFile str)

-- | Parses a 'PInteractiveStmt'. Throws a 'SourceError' on any parse error.
parseInteractiveStmt :: CSPMMonad m => String -> m PInteractiveStmt
parseInteractiveStmt str = 
    runParserInCurrentState "" (P.parseInteractiveStmt str)

-- | Parses an 'Exp'. Throws a 'SourceError' on any parse error.
parseExpression :: CSPMMonad m => String -> m PExp
parseExpression str = runParserInCurrentState "" (P.parseExpression str)

-- Renamer API

-- | Runs renamer in the current state.
runRenamerInCurrentState :: CSPMMonad m => RN.RenamerMonad a -> m a
runRenamerInCurrentState p = withSession $ \s -> do
    (a, st) <- liftIO $ RN.runFromStateToState (rnState s) p
    modifySession (\s -> s { rnState = st })
    return a

-- | Renames a file.
renameFile :: CSPMMonad m => [PModule] -> m [TCModule]
renameFile m = runRenamerInCurrentState $ do
    RN.newScope
    RN.rename m

-- | Renames an expression.
renameExpression :: CSPMMonad m => PExp -> m TCExp
renameExpression e = runRenamerInCurrentState $ RN.rename e

-- | Rename ian interactive statement.
renameInteractiveStmt :: CSPMMonad m => PInteractiveStmt -> m TCInteractiveStmt
renameInteractiveStmt e = runRenamerInCurrentState $ do
    RN.newScope
    RN.rename e

-- TypeChecker API

-- | Runs the typechecker in the current state, saving the resulting state and
-- returning any warnings encountered.
runTypeCheckerInCurrentState :: CSPMMonad m => TC.TypeCheckMonad a -> m (a, [ErrorMessage])
runTypeCheckerInCurrentState p = withSession $ \s -> do
    (a, ws, st) <- liftIO $ TC.runFromStateToState (tcState s) p
    modifySession (\s -> s { tcState = st })
    return (a, ws)

-- | Type checks a file, also desugaring and annotating it. Throws a 
-- 'SourceError' if an error is encountered and will call 'handleWarnings' on 
-- any warnings. This also performs desugaraing.
typeCheckFile :: CSPMMonad m => [TCModule] -> m [TCModule]
typeCheckFile ms = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheck ms
    return ms

-- | Type checks a 'PInteractiveStmt'.
typeCheckInteractiveStmt :: CSPMMonad m => TCInteractiveStmt -> m TCInteractiveStmt
typeCheckInteractiveStmt pstmt = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheck pstmt
    return pstmt

-- | Type checkes a 'PExp', returning the desugared and annotated version.
typeCheckExpression :: CSPMMonad m => TCExp -> m TCExp
typeCheckExpression exp = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheck exp
    return exp

-- | Given a 'Type', ensures that the 'PExp' is of that type. It returns the
-- annoated and desugared expression.
ensureExpressionIsOfType :: CSPMMonad m => Type -> TCExp -> m TCExp
ensureExpressionIsOfType t exp = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheckExpect t exp
    return exp

-- | Gets the type of the expression in the current context.
typeOfExpression :: CSPMMonad m => TCExp -> m Type
typeOfExpression exp = 
    reportWarnings $ runTypeCheckerInCurrentState (TC.typeOfExp exp)

-- | Returns the 'Name's that the given type checked expression depends on.
dependenciesOfExp :: CSPMMonad m => TCExp -> m [Name]
dependenciesOfExp e = 
    reportWarnings $ runTypeCheckerInCurrentState (TC.dependenciesOfExp e)

-- | Desugar a file, preparing it for evaulation.
desugarFile :: CSPMMonad m => [TCModule] -> m [TCModule]
desugarFile [m] = return [DS.desugar m]

-- | Desugars an expression.
desugarExpression :: CSPMMonad m => TCExp -> m TCExp
desugarExpression e = return $ DS.desugar e

-- | Desugars an interactive statement.
desugarInteractiveStmt :: CSPMMonad m => TCInteractiveStmt -> m TCInteractiveStmt
desugarInteractiveStmt s = return $ DS.desugar s

-- Evaluator API

-- | Runs the evaluator in the current state, saving the resulting state.
runEvaluatorInCurrentState :: CSPMMonad m => EV.EvaluationMonad a -> m a
runEvaluatorInCurrentState p = withSession $ \s -> do
    let (a, st) = EV.runFromStateToState (evState s) p
    modifySession (\s -> s { evState = st })
    return a

-- Environment API

-- | Get a list of currently bound names in the environment.
getBoundNames :: CSPMMonad m => m [Name]
getBoundNames = runEvaluatorInCurrentState EV.getBoundNames 
 
-- | Takes a declaration and adds it to the current environment. Requires the
-- declaration to be desugared.
bindDeclaration :: CSPMMonad m => TCDecl -> m ()
bindDeclaration d = withSession $ \s -> do
    evSt <- runEvaluatorInCurrentState (do
        ds <- EV.evaluateDecl d
        EV.addToEnvironment ds)
    modifySession (\s -> s { evState = evSt })
 
-- | Binds all the declarations that are in a particular file. Requires the
-- file to be desugared.
bindFile :: CSPMMonad m => [TCModule] -> m ()
bindFile ms = do
    -- Bind
    evSt <- runEvaluatorInCurrentState (do
        ds <- EV.evaluateFile ms
        EV.addToEnvironment ds)
    modifySession (\s -> s { evState = evSt })
    return ()
 
-- | Evaluates the expression in the current context. Requires the expression
-- to be desugared.
evaluateExpression :: CSPMMonad m => TCExp -> m Value
evaluateExpression e = runEvaluatorInCurrentState (EV.evaluateExp e)

-- | Return the version of libcspm that is being used.
getLibCSPMVersion :: Version
getLibCSPMVersion = version
