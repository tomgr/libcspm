module Main where

import Control.Monad
import Data.List
import System
import System.Console.GetOpt
import System.Directory
import System.FilePath
import System.Exit
import System.IO

import CSPM
import Util.Exception

tryDoDir :: String -> IO ()
tryDoDir path = do
	r <- doesDirectoryExist path
	if r then doDir path else doFile path

doDir :: String -> IO ()
doDir path = do
	all <- getDirectoryContents path
	let all' = [path++"/"++f | f <- all]
	files <- filterM doesFileExist all'
	dirs <- filterM doesDirectoryExist all'
	let dirs' = 
		filter (\f -> not $ (isSuffixOf "." f) || (isSuffixOf ".." f)) dirs
	let files' = filter (isSuffixOf ".csp") files
	mapM_ doDir [dir | dir <- dirs']
	mapM_ doFile [file | file <- files']

doFile :: FilePath -> IO ()
doFile fp = do
	putStr ("Checking "++fp++".....")
	s <- newCSPMSession
	res <- tryM $ unCSPM s $ do
		ms <- parse (fileParser fp)
		typeCheck (fileTypeChecker ms)
		return ()
	case res of
		Left e -> putStrLn $ "\n\ESC[1;31m\STX"++(show e)++"\ESC[0m\STX" 
		Right _ -> putStrLn $ "Ok"
	return ()

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
				(Options { help = True }, files) -> putStr $ usageInfo header options
				(Options { recursive = True }, dirs) -> mapM_ tryDoDir dirs
				(_, files) -> mapM_ doFile files
