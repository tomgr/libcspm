module Main where

import Data.Char
import Control.Exception (AsyncException(..))
import Control.Monad.Trans
import Data.List
import Prelude hiding (catch)
import System.Console.Haskeline
import System.Environment
import System.FilePath
import System.IO

import CSPM
import CSPM.Evaluator.ProcessValues
import Monad
import Util.Annotated
import Util.Exception
import Util.Monad
import qualified Util.MonadicPrettyPrint as M
import Util.Prelude
import Util.PrettyPrint

main :: IO ()
main = do
    st <- initICheckerState
    -- Ensure the output is not buffered
    hSetBuffering stdout NoBuffering
    runIChecker st runICheckerInput

runICheckerInput :: IChecker ()
runICheckerInput = do
    settingsDir <- getState settingsDirectory
    let settings = setComplete iCheckerComplete $ defaultSettings {
            historyFile = Just $ 
                joinPath [settingsDir, "interactive", "prompt_history"]
        }
    runInputT settings $ do
        args <- liftIO $ getArgs
        case args of
            [f] -> handleSourceError () (loadFileCommand f)
            _ -> return ()
        interactiveLoop

interactiveLoop :: InputT IChecker ()
interactiveLoop = do
    currentPath <- lift $ getState currentFilePath
    let 
        prompt = case currentPath of
            Just fp -> last (splitPath fp)
            Nothing -> ""
    minput <- handleSourceError (Just "") (getInputLine (prompt ++ "> "))
    case minput of
        Nothing -> return ()
        Just input -> do
            c <- handleSourceError True (processInput input)
            if c then interactiveLoop else return ()

handleSourceError :: a -> InputT IChecker a -> InputT IChecker a
handleSourceError v prog = (prog `catch` handle v) `catch` handleInt v
    where
        handle :: a -> LibCSPMException -> InputT IChecker a
        handle v e = do
            printError (show e)
            return v
        handleInt :: a -> AsyncException -> InputT IChecker a 
        handleInt v UserInterrupt = do
            printError "Interrupted"
            return v

processInput :: String -> InputT IChecker Bool
processInput (':':str) = do
    let (cmd,rest) = break isSpace str
    case getCommand cmd of
        Just (_, f, _) -> f (dropWhile isSpace rest)
        Nothing -> do
            printError ("unknown command :"++ str)
            return True
processInput expr = 
    if dropWhile isSpace expr == "" then return True
    else keepGoing evaluate expr

type CommandFunc = String -> InputT IChecker Bool
type Command = (String, CommandFunc, CompletionFunc IChecker)

builtInCommands :: [Command]
builtInCommands = [
    ("load", keepGoing loadFileCommand, completeFilename),
    ("printProc", keepGoing printProcCommand, completeExpression),
    ("reload", keepGoing reload, noCompletion),
    ("type", keepGoing typeOfExpr, completeExpression),
    ("quit", quit, noCompletion)
    ]

getCommand :: String -> Maybe Command
getCommand cmd = 
    case getCommands cmd of
        c@(s,_,_):[] -> Just c
        _    -> Nothing

getCommands :: String -> [Command]
getCommands str = 
    [c | c@(n,_,_) <- builtInCommands, str `isPrefixOf` n]

-- Completers
lineBreakers :: [Char]
lineBreakers = " \t\n"
expressionBreakers :: [Char]
expressionBreakers = 
    lineBreakers 
    ++ "+/%*?!$.(),;[]{}\\|"

wrapCompleter :: [Char] -> (String -> IChecker [String]) -> CompletionFunc IChecker
wrapCompleter breakers fun = completeWord Nothing breakers
    $ fmap (map simpleCompletion) . fmap sort . fun

iCheckerComplete :: CompletionFunc IChecker
iCheckerComplete line@(left,_) =
    case firstWord of
        ':':cmd | null rest -> completeCommand line
                | otherwise -> (lookupCompleter cmd) line
        _                   -> completeExpression line
    where
        (firstWord,rest) = break isSpace $ dropWhile isSpace $ reverse left
        lookupCompleter cmd = 
            case getCommand cmd of
                Just (_,_,c) -> c
                Nothing -> noCompletion

completeCommand :: CompletionFunc IChecker
completeCommand = wrapCompleter lineBreakers $ 
    \str -> case str of
        ':':cmd -> return (map (\ (s,_,_) -> ':':s) (getCommands cmd))
        _       -> return []

completeExpression :: CompletionFunc IChecker
completeExpression = wrapCompleter expressionBreakers $ \str -> do
    ns <- getBoundNames
    return [s | n <- ns, let s = show n, str `isPrefixOf` s]
    
-- Commands

keepGoing :: (String -> InputT IChecker a) -> String -> InputT IChecker Bool
keepGoing prog str = prog str >> return True

quit :: String -> InputT IChecker Bool
quit _ = return False

typeOfExpr :: String ->  InputT IChecker ()
typeOfExpr str = do
    pExpr <- parseExpression str
    rnExpr <- renameExpression pExpr
    typ <- typeOfExpression rnExpr
    outputStrLn $ show $ 
        text (trim str) <+> text "::" <+> prettyPrint typ
    return ()

loadFileCommand :: String -> InputT IChecker ()
loadFileCommand fname = do
    fname <- liftIO $ expandPathIO (trim fname)
    -- Reset the context
    lift resetCSPM
    lift $ modifyState (\st -> st { currentFilePath = Just fname })
    -- Handle the error here so that the filepath is remembered (for reloading)
    handleSourceError () $ do
        pFile <- parseFile fname
        rnFile <- renameFile pFile
        tcFile <- typeCheckFile rnFile
        dsFile <- desugarFile tcFile
        bindFile dsFile
        outputStrLn $ "Ok, loaded "++fname

reload :: String -> InputT IChecker ()
reload _ = do
    lift resetCSPM
    currentPath <- lift $ getState currentFilePath
    case currentPath of
        Just fp -> loadFileCommand fp
        Nothing -> return ()

evaluate :: String -> InputT IChecker ()
evaluate str = do
    pStmt <- parseInteractiveStmt str
    rnStmt <- renameInteractiveStmt pStmt
    tcStmt <- typeCheckInteractiveStmt rnStmt
    dsStmt <- desugarInteractiveStmt tcStmt
    case (unAnnotate dsStmt) of
        Bind ds -> mapM_ bindDeclaration ds
        Evaluate e -> do
            v <- evaluateExpression e
            d <- M.prettyPrint v
            outputStrLn $ show $ d 

printProcCommand :: String -> InputT IChecker ()
printProcCommand str = do
    pExpr <- parseExpression str
    rnExpr <- renameExpression pExpr
    tcExpr <- ensureExpressionIsOfType TProc rnExpr
    dsExpr <- desugarExpression tcExpr
    VProc p <- evaluateExpression dsExpr
    outputStrLn $ show $ prettyPrintAllRequiredProcesses p
    return ()
