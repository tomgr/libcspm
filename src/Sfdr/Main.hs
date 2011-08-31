{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Data.List
import System.Console.GetOpt
import System.Directory
import System.Envrionment
import System.FilePath
import System.Exit
import System.IO

import CSPM
import CSPM.PrettyPrinter
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

countSuccesses :: [IO Bool] -> IO ()
countSuccesses tasks = do
	results <- sequence tasks
	let 
		failed = length $ filter id results
		succeeded = length $ filter id results
		total = length tasks
	if failed+succeeded > 1 then do
		putStrLn $ show succeeded++" files succeeded out of "++show total
	else return ()

getFilesFromDir :: FilePath -> IO [FilePath]
getFilesFromDir path = do
	all <- getDirectoryContents path
	let all' = [path++"/"++f | f <- all]
	files <- filterM doesFileExist all'
	dirs <- filterM doesDirectoryExist all'
	let dirs' = 
		filter (\f -> not $ (isSuffixOf "." f) || (isSuffixOf ".." f)) dirs
	let files' = filter (isSuffixOf ".csp") files
	fss <- mapM getFilesFromDir [dir | dir <- dirs']
	return $ files'++concat fss

doFile :: FilePath -> IO Bool
doFile fp = do
	putStr $ "Checking "++fp++"....."
	s <- newCSPMSession
	res <- tryM $ unCSPM s $ do
		ms <- parseFile fp
		typeCheckFile ms
		return ()
	case res of
		Left e -> do
			putStrLn $ "\n\ESC[1;31m\STX"++(show e)++"\ESC[0m\STX" 
			return False
		Right _ -> do
			putStrLn $ "Ok"
			return True

data Options = Options {
		recursive :: Bool,
		help :: Bool
	}
defaultOptions = Options { 
		recursive = False, 
		help = False 
	}

options :: [OptDescr (Options -> Options)]
options = [
	Option ['r'] ["recursive"] 
		(NoArg (\o -> o { recursive = True })) 
		"If the input file is a directory, check all files contained in all subdirectories",
	Option ['h'] ["help"] 
		(NoArg (\o -> o { help = True })) 
		"Display usage message"
	]

header :: String
header = "Usage: sfdr [OPTION...] files..."

main :: IO ()
main = do 
	args <- getArgs
	case getOpt RequireOrder options args of
		(_,_,e:es) -> putStr $ concat (e:es) ++ usageInfo header options
		(o,files, []) -> do
			let opts = foldl (flip id) defaultOptions o
			case (opts, files) of
				(_, []) -> putStr $ usageInfo header options
				(Options { help = True }, files) -> putStr $ usageInfo header options
				(Options { recursive = True }, dirs) -> do
					tasks <- liftM concat (mapM getFilesFromDir dirs)
					countSuccesses (map doFile tasks)
				(_, files) -> 
					countSuccesses (map doFile files)
