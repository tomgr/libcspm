{-# LANGUAGE CPP #-}
module Main where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import qualified Data.HashTable.IO as H
import Data.List
import Prelude hiding (catch)
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.Exit
import System.IO

import qualified Paths_cspmchecker as C
import Data.Version (showVersion)

import CSPM
import qualified CSPM.CommandLineOptions as CSPM
import CSPM.Evaluator.DeepSeq
import CSPM.PrettyPrinter
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

assertionExpressions :: TCAssertion -> [TCExp]
assertionExpressions (An _ _ (ASNot a)) = assertionExpressions a
assertionExpressions (An _ _ (Refinement e1 _ e2 _)) = [e1, e2]
assertionExpressions (An _ _ (PropertyCheck e1 _ _ _)) = [e1]

doFile :: Options -> FilePath -> IO ()
doFile opts fp = do
    session <- newCSPMSession defaultEvaluatorOptions
    unCSPM session $ do 
        liftIO $ putStrLn $ "Exploring "++fp
        res <- tryM $ convertExceptionsToPanics $ do
            CSPM.setOptions (cspmOptions opts)
            ms <- parseFile fp
            rms <- CSPM.renameFile ms
            tcs <- typeCheckFile rms
            fs <- desugarFile tcs
            bindFile fs

            exploredNames <- liftIO $ H.new :: CSPM (H.BasicHashTable ProcName ())
            toExplore <- liftIO $ newIORef []
            exploredCount <- liftIO $ newIORef 0
            let addToExplore p = do
                    modifyIORef' toExplore (\ ps -> p : ps)
                    modifyIORef' exploredCount (\ c -> c +1)
                    c <- readIORef exploredCount
                    when (c `mod` 1000 == 0) $ do
                        putStrLn $ "Explored "++show c

                explore (PProcCall pn def) = do
                    b <- H.lookup exploredNames pn
                    case b of
                        Just _ -> return ()
                        Nothing -> do
                            H.insert exploredNames pn ()
                            addToExplore def
                explore (PUnaryOp op p) = deepseq op (addToExplore p)
                explore (PBinaryOp op p1 p2) = deepseq op (addToExplore p2 >> addToExplore p1)
                explore (POp op ps) = deepseq op (mapM_ addToExplore ps)

                exploreUntilDone = do
                    ps <- readIORef toExplore
                    case ps of
                        [] -> return ()
                        (p:ps) -> do
                            writeIORef toExplore ps
                            explore p
                            exploreUntilDone

                exploreProcess :: Proc -> IO ()
                exploreProcess p = do
                    addToExplore p
                    exploreUntilDone

                doExpression :: TCExp -> CSPM ()
                doExpression expr = do
                    VProc p <- evaluateExpression expr
                    liftIO $ exploreProcess p

                doAssertion :: TCAssertion -> CSPM ()
                doAssertion assertion = do
                    liftIO $ putStrLn $ "Exploring "++show (prettyPrint assertion)
                    mapM_ doExpression (assertionExpressions assertion)

            mapM_ doAssertion (allAssertionsInFile fs)
#ifdef CSPM_PROFILING
            dumpProfilingData
#endif
        case res of
            Left e -> printError ("\n"++show e)
            Right _ -> return ()
    return ()

printError :: String -> CSPM ()
printError s = liftIO $ putStrLn $ "\ESC[1;31m\STX"++s++"\ESC[0m\STX"

data Options = Options {
        recursive :: Bool,
        help :: Bool,
        printVersion :: Bool,
        cspmOptions :: CSPM.Options
    }

defaultOptions :: Options
defaultOptions = Options { 
        recursive = False, 
        help = False,
        printVersion = False,
        cspmOptions = CSPM.defaultOptions
    }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['v'] ["version"] 
        (NoArg (\o -> o { printVersion = True }))
        "Print out the version number",
    Option ['h'] ["help"] 
        (NoArg (\o -> o { help = True })) 
        "Display usage message"
    ]
    ++ CSPM.allOptions cspmOptions (\ opts x -> opts { cspmOptions = x })

header :: String
header = "Usage: cspmexplorer [OPTION...] files..."

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (_,_,e:es) -> liftIO $ putStr $ concat (e:es) ++ usageInfo header options
        (o,files, []) -> do
            let opts = foldl (flip id) defaultOptions o
            case (opts, files) of
                (Options { printVersion = True }, []) ->
                    liftIO $ putStrLn $ show $ 
                        text "cspmexplorer version" <+> text (showVersion C.version)
                        $$
                        text "using libcspm version" <+> text (showVersion getLibCSPMVersion)
                (Options { help = True }, []) -> 
                    liftIO $ putStr $ usageInfo header options
                (_, []) -> 
                    liftIO $ putStr $ usageInfo header options
                (_, files) -> mapM_ (doFile opts) files
