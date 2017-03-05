{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, IncoherentInstances,
    MultiParamTypeClasses, TypeSynonymInstances, UndecidableInstances #-}
-- | This module provides the main high-level interface to the library 
-- functionality. It does this through a monadic interface, mainly due to the
-- fact that several of the components require the use of the IO monad. It is
-- highly recommended that users of this library use a monad and then implement
-- the 'CSPMMonad' class on their own custom monad. An example of this is shown
-- by the basic implementation of the 'CSPM' monad.
--
-- The main library datatype is exported by 'CSPM.Syntax.AST', which
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
-- >        session <- newCSPMSession False
-- >        (value, resultingSession) <- unCSPM session $ do
-- >            -- Parse the file, returning something of type PCSPMFile.
-- >            parsedFile <- parseFile "test.csp"
-- >            -- Rename the file, returning something of type TCCSPMFile.
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
    module CSPM.Syntax.Names,
    -- | Defines the abstract syntax for machine CSP.
    module CSPM.Syntax.AST,
    -- | Defines the types used by the typechecker.
    module CSPM.Syntax.Types,
    -- | Defines the values produced by the evaluator.
    module CSPM.Evaluator.Values,
    -- * Parser API
    parseStringAsFile, parseStringsAsFile, parseFile, parseInteractiveStmt,
    parseExpression, filesRequiredByFile,
    -- * Renamer API
    renameFile, renameInteractiveStmt, renameExpression, getBoundNames,
    -- * Type Checker API
    typeCheckFile, typeCheckInteractiveStmt, typeCheckExpression,
    ensureExpressionIsOfType, typeOfExpression, modifyTypeCheckerErrorOptions,
    typeOfName, boundProcessNames,
    -- * Desugarer API
    desugarFile, desugarInteractiveStmt, desugarExpression,
    -- * Evaluator API
    bindFile, bindDeclaration,
    evaluateExpression, maybeProcessNameToProcess,
    dumpProfilingData,
    -- * Shortcuts
    stringToValue,
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
import qualified Data.ByteString as B
import Data.Version
import System.FilePath

import CSPM.Syntax.Names
import CSPM.Syntax.AST
import CSPM.Syntax.Types
import qualified CSPM.Evaluator as EV
import CSPM.Evaluator.Values
import qualified CSPM.Parser as P
import qualified CSPM.Renamer as RN
import qualified CSPM.TypeChecker as TC
import qualified CSPM.Desugar as DS
import Paths_libcspm (version)
import Util.Exception
import Util.PrettyPrint
import qualified Util.MonadicPrettyPrint as M

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
    evState <- liftIO $ EV.initEvaluator
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
parseFile :: CSPMMonad m => FilePath -> m PCSPMFile
parseFile fp =
    let (dir, fname) = splitFileName fp
        dir' = if dir == "./" then "" else dir
    in runParserInCurrentState dir' (P.parseFile fname)

-- | Parses a string, treating it as though it were a file. Throws a 
-- 'SourceError' on any parse error.
parseStringAsFile :: CSPMMonad m => String -> m PCSPMFile
parseStringAsFile str = runParserInCurrentState "" (P.parseStringAsFile str)

-- | Parses the file, with the file contents according to the given map.
parseStringsAsFile :: CSPMMonad m => String -> [(String, B.ByteString)] -> m PCSPMFile
parseStringsAsFile rootFile fileContents =
    let (dir, fname) = splitFileName rootFile
        dir' = if dir == "./" then "" else dir
    in runParserInCurrentState dir' (P.parseStringsAsFile fname fileContents)

-- | Parses a 'PInteractiveStmt'. Throws a 'SourceError' on any parse error.
parseInteractiveStmt :: CSPMMonad m => String -> m PInteractiveStmt
parseInteractiveStmt str = 
    runParserInCurrentState "" (P.parseInteractiveStmt str)

-- | Parses an 'Exp'. Throws a 'SourceError' on any parse error.
parseExpression :: CSPMMonad m => String -> m PExp
parseExpression str = runParserInCurrentState "" (P.parseExpression str)

-- | Returns the list of files that are loaded by the specified file.
filesRequiredByFile :: MonadIO m => String -> m [String]
filesRequiredByFile = liftIO . P.filesRequiredByFile

-- Renamer API

-- | Runs renamer in the current state.
runRenamerInCurrentState :: CSPMMonad m => RN.RenamerMonad a -> m a
runRenamerInCurrentState p = withSession $ \s -> do
    (a, st) <- liftIO $ RN.runFromStateToState (rnState s) p
    modifySession (\s -> s { rnState = st })
    return a

-- | Renames a file.
renameFile :: CSPMMonad m => PCSPMFile -> m TCCSPMFile
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

-- | Get a list of currently bound names in the environment.
getBoundNames :: CSPMMonad m => m [Name]
getBoundNames = runRenamerInCurrentState RN.getBoundNames

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
typeCheckFile :: CSPMMonad m => TCCSPMFile -> m TCCSPMFile
typeCheckFile ms = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheck ms

-- | Type checks a 'PInteractiveStmt'.
typeCheckInteractiveStmt :: CSPMMonad m => TCInteractiveStmt -> m TCInteractiveStmt
typeCheckInteractiveStmt pstmt = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheck pstmt

-- | Type checkes a 'PExp', returning the desugared and annotated version.
typeCheckExpression :: CSPMMonad m => TCExp -> m TCExp
typeCheckExpression exp = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheck exp

-- | Given a 'Type', ensures that the 'PExp' is of that type. It returns the
-- annoated and desugared expression.
ensureExpressionIsOfType :: CSPMMonad m => Type -> TCExp -> m TCExp
ensureExpressionIsOfType t exp = reportWarnings $ runTypeCheckerInCurrentState $ do
    TC.typeCheckExpect t exp

-- | Gets the type of the expression in the current context.
typeOfExpression :: CSPMMonad m => TCExp -> m Type
typeOfExpression exp = 
    reportWarnings $ runTypeCheckerInCurrentState (TC.typeOfExp exp)

-- | Returns all currently bound process names, optionally including functions
-- that evaluate to processes
boundProcessNames :: CSPMMonad m =>
    Bool -- ^ If true includes functions that evaluate to processes.
    -> m [Name]
boundProcessNames includeFunctions = do
    ns <- getBoundNames
    ts <- mapM typeOfName ns
    let isProcessFunction (TFunction _ (f@(TFunction _ _))) =
            isProcessFunction f
        isProcessFunction (TFunction _ t) = isProcess t
        isProcessFunction _ = False

        isProcess TProc = True
        isProcess _ = False

        nonFuncNames = map fst $ filter (isProcess . typeSchemeType . snd)
            (zip ns ts)
        funcNames = map fst $ filter (isProcessFunction . typeSchemeType . snd)
            (zip ns ts)
    return $ nonFuncNames ++ (if includeFunctions then funcNames else [])

-- | Returns the type of the given name in the current context.
--
-- The file in which this name has been bound must have been typechecked using
-- typeCheckFile.
typeOfName :: CSPMMonad m => Name -> m TypeScheme
typeOfName n = reportWarnings $ runTypeCheckerInCurrentState (TC.typeOfName n)

modifyTypeCheckerErrorOptions :: CSPMMonad m =>
    (TC.ErrorOptions -> TC.ErrorOptions) -> m ()
modifyTypeCheckerErrorOptions f = reportWarnings $
    runTypeCheckerInCurrentState (TC.modifyErrorOptions f)

-- | Desugar a file, preparing it for evaulation.
desugarFile :: CSPMMonad m => TCCSPMFile -> m TCCSPMFile
desugarFile m = DS.runDesugar $ DS.desugar m

-- | Desugars an expression.
desugarExpression :: CSPMMonad m => TCExp -> m TCExp
desugarExpression e = DS.runDesugar $ DS.desugar e

-- | Desugars an interactive statement.
desugarInteractiveStmt :: CSPMMonad m => TCInteractiveStmt -> m TCInteractiveStmt
desugarInteractiveStmt s = DS.runDesugar $ DS.desugar s

-- Evaluator API

-- | Runs the evaluator in the current state, saving the resulting state.
runEvaluatorInCurrentState :: CSPMMonad m => EV.EvaluationMonad a -> m a
runEvaluatorInCurrentState p = withSession $ \s -> do
    (a, st) <- liftIO $ EV.runFromStateToState (evState s) p
    modifySession (\s -> s { evState = st })
    return a

-- Environment API
 
-- | Takes a declaration and adds it to the current environment. Requires the
-- declaration to be desugared.
bindDeclaration :: CSPMMonad m => TCDecl -> m ()
bindDeclaration d = runEvaluatorInCurrentState $ EV.evaluateDecl d
 
-- | Binds all the declarations that are in a particular file. Requires the
-- file to be desugared.
bindFile :: CSPMMonad m => TCCSPMFile -> m ()
bindFile m = runEvaluatorInCurrentState $ EV.evaluateFile m
 
-- | Evaluates the expression in the current context. Requires the expression
-- to be desugared.
evaluateExpression :: CSPMMonad m => TCExp -> m Value
evaluateExpression e = runEvaluatorInCurrentState $ EV.evaluateExp e

-- | Given a process name, attempts to convert the name into a process. This
-- is only possible for top-level function applications.
maybeProcessNameToProcess :: CSPMMonad m => EV.ProcName -> m (Maybe EV.Proc)
maybeProcessNameToProcess pn =
    runEvaluatorInCurrentState $ EV.maybeProcessNameToProcess pn

-- | Takes an expression string and a type and evaluates the expression,
-- providing the expression is of the correct type.
stringToValue :: CSPMMonad m => Type -> String -> m Value
stringToValue typ str =
    parseExpression str >>= renameExpression >>= 
    ensureExpressionIsOfType typ >>= desugarExpression >>= evaluateExpression

-- | Dumps any profiling data that has been computed to stdout/stderr.
dumpProfilingData :: CSPMMonad m => m ()
dumpProfilingData = liftIO $ EV.dumpProfilingData

-- | Return the version of libcspm that is being used.
getLibCSPMVersion :: Version
getLibCSPMVersion = version

instance {-# OVERLAPPABLE #-} (Applicative m, CSPMMonad m,
            M.MonadicPrettyPrintable EV.EvaluationMonad a) => 
        M.MonadicPrettyPrintable m a where
    prettyPrint = runEvaluatorInCurrentState . M.prettyPrint
