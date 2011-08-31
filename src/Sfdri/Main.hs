module Main where

import Data.Char
import Control.Exception (AsyncException(..))
import Control.Monad.Trans
import Data.List
import Prelude hiding (catch)
import System.Console.Haskeline
import System.FilePath

import CSPM
import Sfdri.Monad
import Util.Annotated
import Util.Exception
import Util.Monad
import Util.Prelude
import Util.PrettyPrint

main :: IO ()
main = do
    st <- initSfdriState
    runSfdri st runSfdriInput

runSfdriInput :: Sfdri ()
runSfdriInput = do
    settingsDir <- getState settingsDirectory
    let settings = setComplete sfdriComplete $ defaultSettings {
            historyFile = Just $ 
                joinPath [settingsDir, "interactive", "prompt_history"]
        }
    runInputT settings interactiveLoop

interactiveLoop :: InputT Sfdri ()
interactiveLoop = do
    currentPath <- lift $ getState currentFilePath
    let prompt = 
        case currentPath of
            Just fp -> last (splitPath fp)
            Nothing -> ""
    minput <- handleSourceError (Just "") (getInputLine (prompt ++ "> "))
    case minput of
        Nothing -> return ()
        Just input -> do
            c <- handleSourceError True (processInput input)
            if c then interactiveLoop else return ()

handleSourceError :: a -> InputT Sfdri a -> InputT Sfdri a
handleSourceError v prog = (prog `catch` handle v) `catch` handleInt v
    where
        handle :: a -> SfdrException -> InputT Sfdri a
        handle v e = do
            printError (show e)
            return v
        handleInt :: a -> AsyncException -> InputT Sfdri a 
        handleInt v UserInterrupt = do
            printError "Interrupted"
            return v

printError :: String -> InputT Sfdri ()
printError s = outputStrLn $ "\ESC[1;31m\STX"++s++"\ESC[0m\STX" 

processInput :: String -> InputT Sfdri Bool
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

type CommandFunc = String -> InputT Sfdri Bool
type Command = (String, CommandFunc, CompletionFunc Sfdri)

builtInCommands :: [Command]
builtInCommands = [
    ("load", keepGoing loadFileCommand, completeFilename),
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

wrapCompleter :: [Char] -> (String -> Sfdri [String]) -> CompletionFunc Sfdri
wrapCompleter breakers fun = completeWord Nothing breakers
    $ fmap (map simpleCompletion) . fmap sort . fun

sfdriComplete :: CompletionFunc Sfdri
sfdriComplete line@(left,_) =
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

completeCommand :: CompletionFunc Sfdri
completeCommand = wrapCompleter lineBreakers $ 
    \str -> case str of
        ':':cmd -> return (map (\ (s,_,_) -> ':':s) (getCommands cmd))
        _       -> return []

completeExpression :: CompletionFunc Sfdri
completeExpression = wrapCompleter expressionBreakers $ \str -> do
    ns <- getBoundNames
    return [s | Name s <- ns, str `isPrefixOf` s]
    
-- Commands

keepGoing :: (String -> InputT Sfdri a) -> String -> InputT Sfdri Bool
keepGoing prog str = prog str >> return True

quit :: String -> InputT Sfdri Bool
quit _ = return False

typeOfExpr :: String ->  InputT Sfdri ()
typeOfExpr str = do
    pExpr <- parseExpression str
    typ <- typeOfExpression pExpr
    outputStrLn $ show $ 
        text str <+> text "::" <+> prettyPrint typ
    return ()

loadFileCommand :: String -> InputT Sfdri ()
loadFileCommand fname = do
    fname <- liftIO $ expandPathIO (trim fname)
    -- Reset the context
    lift resetCSPM
    lift $ modifyState (\st -> st { currentFilePath = Just fname })
    -- Handle the error here so that the filepath is remembered (for reloading)
    handleSourceError () $ do
        pFile <- parseFile fname
        tcFile <- typeCheckFile pFile
        loadFile tcFile
        outputStrLn $ "Ok, loaded "++fname

reload :: String -> InputT Sfdri ()
reload _ = do
    lift resetCSPM
    currentPath <- lift $ getState currentFilePath
    case currentPath of
        Just fp -> loadFileCommand fp
        Nothing -> return ()

evaluate :: String -> InputT Sfdri ()
evaluate str = do
    pStmt <- parseInteractiveStmt str
    tcStmt <- typeCheckInteractiveStmt pStmt
    case (unAnnotate tcStmt) of
        Bind d -> bindDeclaration d
        Evaluate e -> evaluateExp e >>= outputStrLn . show . prettyPrint
