module Main where

import Data.List
import System
import System.Console.GetOpt
import System.Directory
import System.FilePath
import System.Exit
import System.IO

import CSPMDataStructures.Syntax
import CSPMParser.Parser
import CSPMTypeChecker.Module
import CSPMTypeChecker.Monad
import Util.Annotated
import Util.Monad

-- We make the decision that union types are not supported. This is because of cases
-- such as:
-- 		x = if 0==1 then 1 else False
--		y = x + 1
-- This does create some problems. For example, consider the definitions:
--		channel a : A.A.A
--		f(x) = a.x
-- Then the only type assignable to f is:
--		f :: (A.A.A -> TEvent) or (A.A -> TDotable A TEvent) 
--							   or (A -> TDotable A (TDotable A TEvent))
-- Thus, we make the decision that the shortest type should be assigned.
-- In other words, the right hand side would be a TEvent in the above case,
-- or a TDatatype N in other cases.

interactiveMain :: FilePath -> IO ()
interactiveMain cspmFile = doFile cspmFile

testMain :: IO ()
testMain = doDir "../examples"

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
doFile fp = 
	do
 		putStrLn("*****************************")
		putStrLn ("Doing "++fp)
		res <- runTyger (do
			ast <- parseFile fp
    			runTypeChecker (typeCheckModules [An (error "") (error "") (GlobalModule ast)]))
		case res of
			Left err	-> putStrLn (show err)
			Right _		-> putStrLn ("Done")


main :: IO ()
main = 
	do 
		args <- getArgs
		mapM_ doFile args
{-
	do
		res <- runTyger (tygerMain "../Examples/SingletonAvailabilityTesting.opsem" 
									"../Examples/SingletonAvailabilityTestingExample.csp")
		case res of
			Left err	-> putStrLn (show err) >> exitFailure
			Right _		-> exitSuccess
-}
