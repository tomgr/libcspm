module Main where

import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.FilePath

import CSPM
import Util.Exception
import Util.PrettyPrint

main :: IO ()
main = do
	tests <- runSections
	results <- sequence tests
	if and results then exitSuccess else exitFailure

getAndFilterDirectoryContents :: FilePath -> IO [FilePath]
getAndFilterDirectoryContents fp = do
	names <- getDirectoryContents fp
	return $ filter (`notElem` [".", "..", ".DS_Store"]) names

runSections ::IO [IO Bool]
runSections = do
	let testDir = "tests"
	sections <- getAndFilterDirectoryContents testDir
	fs <- mapM (\section -> do
			shouldPassFiles <- getAndFilterDirectoryContents $ 
								joinPath [testDir, section, "should_pass"]
			shouldFailFiles <- getAndFilterDirectoryContents $ 
								joinPath [testDir, section, "should_fail"]
			let 
				Just test = lookup section testFunctions
				pf = [runTest (joinPath [testDir, section, "should_pass", f]) 
						test False | f <- shouldPassFiles]
				ff = [runTest (joinPath [testDir, section, "should_fail", f]) 
						test True | f <- shouldFailFiles]
			return $ pf++ff
		) sections
	return $ concat fs

runTest :: FilePath -> (FilePath -> CSPM a) -> Bool -> IO Bool
runTest fp test shouldFail = do
	putStr $ "Running test "++fp++"..."
	s <- newCSPMSession
	res <- tryM $ unCSPM s $ test fp
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

testFunctions = [
		("parser", parserTest),
		("typechecker", typeCheckerTest),
		("prettyprinter", prettyPrinterTest)
	]

typeCheckerTest :: FilePath -> CSPM ()
typeCheckerTest fp = do
	ms <- parse (fileParser fp)
	typeCheck (fileTypeChecker ms)
	return ()

parserTest :: FilePath -> CSPM ()
parserTest fp = do
	ms <- parse (fileParser fp)
	-- Force evaluation of the whole of ms. We can't just use seq
	-- as this would leave thunks in the data structure. Instead we take
	-- the length of the string representing ms and then compute the length
	(length (show ms)) `seq` (return ())

prettyPrinterTest :: FilePath -> CSPM ()
prettyPrinterTest fp = do
	ms <- parse (fileParser fp)
	let str = show (prettyPrint ms)
	ms' <- parse (stringFileParser str)
	if ms /= ms' then throwException UserError else return ()
