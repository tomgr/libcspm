{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Main where

import Data.Char
import Control.Exception (AsyncException(..))
import Control.Monad.State
import Control.Monad.Trans
import Data.List
import Prelude hiding (catch)
import System.Console.Haskeline
import System.FilePath
import System.IO

import CSPM
import CSPM.Compiler.Processes
import CSPM.Operators.CSP
import Monad
import Util.Annotated
import Util.Exception
import Util.Monad
import Util.Prelude
import Util.PrettyPrint

type InputM c ops = InputT (IChecker c ops)

main :: IO ()
main = do
    st <- initICheckerState :: IO (ICheckerState () UnCompiledCSPOp)
    -- Ensure the output is not buffered
    hSetBuffering stdout NoBuffering
    let settingsDir = settingsDirectory st
        settings = setComplete iCheckerComplete $ defaultSettings {
            historyFile = Just $ 
                joinPath [settingsDir, "interactive", "prompt_history"]
        }
    runIChecker st (runInputT settings interactiveLoop)

interactiveLoop :: CSPLike () p ops => InputM () ops ()
interactiveLoop = do
    currentPath <- lift $ gets currentFilePath
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

handleSourceError :: CSPLike () p ops => a -> InputM () ops a -> InputM () ops a
handleSourceError v prog = (prog `catch` handle v) `catch` handleInt v
    where
        handle :: a -> LibCSPMException -> InputM c ops a
        handle v e = do
            printError (show e)
            return v
        handleInt :: a -> AsyncException -> InputM c ops a 
        handleInt v UserInterrupt = do
            printError "Interrupted"
            return v

processInput :: CSPLike () p ops => String -> InputM () ops Bool
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

type CommandFunc c ops = String -> InputM c ops Bool
type Command c ops = (String, CommandFunc c ops, CompletionFunc (IChecker c ops))

builtInCommands :: CSPLike () p ops => [Command () ops]
builtInCommands = [
    ("load", keepGoing loadFileCommand, completeFilename),
    ("printProc", keepGoing printProcCommand, completeExpression),
    ("reload", keepGoing reload, noCompletion),
    ("type", keepGoing typeOfExpr, completeExpression),
    ("quit", quit, noCompletion)
    ]

getCommand :: CSPLike () p ops => String -> Maybe (Command () ops)
getCommand cmd = 
    case getCommands cmd of
        c@(s,_,_):[] -> Just c
        _    -> Nothing

getCommands :: CSPLike () p ops => String -> [Command () ops]
getCommands str = 
    [c | c@(n,_,_) <- builtInCommands, str `isPrefixOf` n]

-- Completers
lineBreakers :: [Char]
lineBreakers = " \t\n"
expressionBreakers :: [Char]
expressionBreakers = 
    lineBreakers 
    ++ "+/%*?!$.(),;[]{}\\|"

wrapCompleter :: [Char] -> (String -> IChecker c ops [String]) -> CompletionFunc (IChecker c ops)
wrapCompleter breakers fun = completeWord Nothing breakers
    $ fmap (map simpleCompletion) . fmap sort . fun

iCheckerComplete :: CSPLike () p ops => CompletionFunc (IChecker () ops)
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

completeCommand :: forall p ops . CSPLike () p ops => CompletionFunc (IChecker () ops)
completeCommand = wrapCompleter lineBreakers $ 
    \str -> case str of
        ':':cmd -> return (map (\ (s,_,_) -> ':':s) (getCommands cmd :: [Command () ops]))
        _       -> return []

completeExpression :: CSPLike () p ops => CompletionFunc (IChecker () ops)
completeExpression = wrapCompleter expressionBreakers $ \str -> do
    ns <- getBoundNames
    return [s | n <- ns, let OccName s = nameOccurrence n, str `isPrefixOf` s]
    
-- Commands

keepGoing :: (String -> InputM c ops a) -> String -> InputM c ops Bool
keepGoing prog str = prog str >> return True

quit :: String -> InputM c ops Bool
quit _ = return False

typeOfExpr :: CSPLike c p ops => String ->  InputM c ops ()
typeOfExpr str = do
    typ <- lift $ do
        pExpr <- parseExpression str
        rnExpr <- renameExpression pExpr
        typeOfExpression rnExpr
    outputStrLn $ show $ 
        text str <+> text "::" <+> prettyPrint typ
    return ()

loadFileCommand :: CSPLike () p ops => String -> InputM () ops ()
loadFileCommand fname = do
    fname <- liftIO $ expandPathIO (trim fname)
    -- Reset the context
    lift $ resetCSPM
    lift $ modify (\st -> st { currentFilePath = Just fname })
    -- Handle the error here so that the filepath is remembered (for reloading)
    handleSourceError () $ do
        lift $ do
            pFile <- parseFile fname
            rnFile <- renameFile pFile
            tcFile <- typeCheckFile rnFile
            dsFile <- desugarFile tcFile
            bindFile dsFile
        outputStrLn $ "Ok, loaded "++fname

reload :: CSPLike () p ops => String -> InputM () ops ()
reload _ = do
    lift $ resetCSPM
    currentPath <- lift $ gets currentFilePath
    case currentPath of
        Just fp -> loadFileCommand fp
        Nothing -> return ()

evaluate :: CSPLike c p ops => String -> InputM c ops ()
evaluate str = do
    dsStmt <- lift $ do
        pStmt <- parseInteractiveStmt str
        rnStmt <- renameInteractiveStmt pStmt
        tcStmt <- typeCheckInteractiveStmt rnStmt
        desugarInteractiveStmt tcStmt
    case (unAnnotate dsStmt) of
        Bind d -> lift $ bindDeclaration d
        Evaluate e -> do
            v <- lift $ evaluateExpression e
            outputStrLn $ show $ prettyPrint v

printProcCommand :: CSPLike c p ops => String -> InputM c ops ()
printProcCommand str = do
    VProc p <- lift $ do
        pExpr <- parseExpression str
        rnExpr <- renameExpression pExpr
        tcExpr <- ensureExpressionIsOfType TProc rnExpr
        dsExpr <- desugarExpression tcExpr
        evaluateExpression dsExpr
    outputStrLn $ show $ prettyPrintAllRequiredProcesses p
    return ()
