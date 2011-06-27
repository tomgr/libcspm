module Main where

import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.FilePath

import CSPM
import Util.Exception

main :: IO ()
main = do
	files <- getTestFiles
	results <- mapM (\(fp,shouldPass) -> runTest fp shouldPass) files
	if and results then exitSuccess else exitFailure

getAndFilterDirectoryContents :: FilePath -> IO [FilePath]
getAndFilterDirectoryContents fp = do
	names <- getDirectoryContents fp
	return $ filter (`notElem` [".", ".."]) names

getTestFiles :: IO [(FilePath, Bool)]
getTestFiles = do
	let testDir = "tests"
	sections <- getAndFilterDirectoryContents testDir
	fs <- mapM (\section -> do
			shouldPassFiles <- getAndFilterDirectoryContents $ 
								joinPath [testDir, section, "should_pass"]
			shouldFailFiles <- getAndFilterDirectoryContents $ 
								joinPath [testDir, section, "should_fail"]
			let 
				pf = [(joinPath [testDir, section, "should_pass", f] , False) 
						| f <- shouldPassFiles]
				ff = [(joinPath [testDir, section, "should_fail", f], True) 
						| f <- shouldFailFiles]
			return $ pf++ff
		) sections
	return $ concat fs

runTest :: FilePath -> Bool -> IO Bool
runTest fp shouldFail = do
	putStr $ "Running test "++fp++"..."
	s <- newCSPMSession
	res <- tryM $ unCSPM s $ do
		ms <- parse (fileParser fp)
		typeCheck (fileTypeChecker ms)
	let
		failed :: Maybe SfdrException -> IO Bool
		failed (Nothing) = do
			putStrLn "FAILED (test passed but should have failed)"
			return False
		failed (Just e) = do
			putStrLn "FAILED (test failed but should have passed)"
			putStrLn $ show e
			return False
		passed = do
			putStrLn "Passed"
			return True
	case res of 
		Left e -> if shouldFail then passed else failed (Just e)
		Right _ -> if shouldFail then failed Nothing else passed
