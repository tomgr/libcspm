module Main where

import Control.Monad
import Control.Monad.Trans
import Data.List
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.Exit
import System.IO

import qualified Paths_cspmchecker as C
import Data.Version (showVersion)

import CSPM
import CSPM.PrettyPrinter
import Monad
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

countSuccesses :: [Checker Bool] -> Checker ()
countSuccesses tasks = do
    results <- sequence tasks
    let 
        failed = length $ filter id results
        succeeded = length $ filter id results
        total = length tasks
    if failed+succeeded > 1 then do
        liftIO $ putStrLn $ show succeeded++" files succeeded out of "++show total
    else return ()

getFilesFromDir :: FilePath -> IO [FilePath]
getFilesFromDir path = do
    all <- getDirectoryContents path
    let all' = [path++"/"++f | f <- all]
    files <- filterM doesFileExist all'
    dirs <- filterM doesDirectoryExist all'
    let 
        dirs' = filter (\f -> not $ (isSuffixOf "." f) 
                                    || (isSuffixOf ".." f)) dirs
        files' = filter (isSuffixOf ".csp") files
    fss <- mapM getFilesFromDir [dir | dir <- dirs']
    return $ files'++concat fss

doFile :: FilePath -> Checker Bool
doFile fp = do
    liftIO $ putStr $ "Checking "++fp++"....."
    res <- tryM $ do
        ms <- parseFile fp
        typeCheckFile ms
        return ()
    ws <- getState lastWarnings
    resetCSPM
    case res of
        Left e -> do
            printError ("\n"++show e)
            return False
        Right _ -> do
            liftIO $ putStrLn $ "Ok"
            if ws /= [] then printError (show (prettyPrint ws))
            else return ()
            return True

printError :: String -> Checker ()
printError s = liftIO $ putStrLn $ "\ESC[1;31m\STX"++s++"\ESC[0m\STX"

data Options = Options {
        recursive :: Bool,
        help :: Bool,
        printVersion :: Bool
    }
defaultOptions = Options { 
        recursive = False, 
        help = False,
        printVersion = False
    }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['v'] ["version"] 
        (NoArg (\o -> o { printVersion = True }))
        "Print out the version number",
    Option ['r'] ["recursive"] 
        (NoArg (\o -> o { recursive = True })) 
        "If the input file is a directory, check all files contained in all subdirectories",
    Option ['h'] ["help"] 
        (NoArg (\o -> o { help = True })) 
        "Display usage message"
    ]

header :: String
header = "Usage: cspmchecker [OPTION...] files..."

main :: IO ()
main = do
    args <- getArgs
    st <- initCheckerState
    runChecker st $ case getOpt RequireOrder options args of
        (_,_,e:es) -> liftIO $ putStr $ show $ concat (e:es) ++ usageInfo header options
        (o,files, []) -> do
            let opts = foldl (flip id) defaultOptions o
            case (opts, files) of
                (Options { printVersion = True }, []) ->
                    liftIO $ putStrLn $ show $ 
                        text "cspmchecker version" <+> text (showVersion C.version)
                        $$
                        text "using libcspm version" <+> text (showVersion getLibCSPMVersion)
                (Options { help = True }, []) -> 
                    liftIO $ putStr $ usageInfo header options
                (Options { recursive = True }, dirs) -> do
                    tasks <- mapM (liftIO . getFilesFromDir) dirs
                    countSuccesses (map doFile (concat tasks))
                (_, []) -> 
                    liftIO $ putStr $ usageInfo header options
                (_, files) -> 
                    countSuccesses (map doFile files)
