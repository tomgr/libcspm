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

interactiveMain :: FilePath -> IO ()
interactiveMain cspmFile = doFile cspmFile

testMain :: IO ()
testMain = doDir "../../examples"

doDir :: String -> IO ()
doDir path = 
	do
		all <- getDirectoryContents path
		let all' = [path++"/"++f | f <- all]
		files <- filterM doesFileExist all'
		dirs <- filterM doesDirectoryExist all'
		let dirs' = filter (\f -> not $ (isSuffixOf "." f) || (isSuffixOf ".." f)) dirs
		let files' = filter (isSuffixOf ".csp") files
		mapM_ doDir [dir | dir <- dirs']
		mapM_ doFile [file | file <- files']

doFile :: FilePath -> IO ()
doFile fp = do
	putStrLn("*****************************")
	putStrLn ("Doing "++fp)
	s <- newCSPMSession
	res <- tryM $ unCSPM s $ do
		ms <- parse (fileParser fp)
		typeCheck (fileTypeChecker ms)
		return ()
	case res of
		Left e -> putStrLn $ "\ESC[1;31m\STX"++(show e)++"\ESC[0m\STX" 
		Right _ -> putStrLn $ "Ok"
	return ()

main :: IO ()
main = 
	do 
		args <- getArgs
		mapM_ doFile args
