module Main (main) where

import Control.Monad
import Control.Monad.Trans
import Data.List
import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.FilePath

import CSPM
import CSPM.Compiler.Processes
import CSPM.Operators.CSP
import Monad
import Util.Annotated
import Util.Exception
import Util.Monad hiding (($$))
import Util.PrettyPrint

data RunResult = 
    ErrorOccured
    | WarningsEmitted
    | PassedNoWarnings
    deriving Eq
        
main :: IO ()
main = do
    tests <- runSections
    results <- sequence tests
    let
        failureCount = length results - successCount
        successCount = length (filter id results)
    putStrLn $ show $ 
        int failureCount <+> text "failures" 
        <+> int successCount <+> text "passes"
    if failureCount == 0 then exitSuccess else exitFailure

getAndFilterDirectoryContents :: FilePath -> IO [FilePath]
getAndFilterDirectoryContents fp = do
    b <- doesDirectoryExist fp
    if not b then return [] else do
        names <- getDirectoryContents fp
        let ns = filter (`notElem` [".", "..", ".DS_Store"]) names
        concatMapM (\n -> do
            let fp' = joinPath [fp, n]
            b <- doesDirectoryExist fp'
            if b then do
                ns <- getAndFilterDirectoryContents fp'
                return [joinPath [n, n'] | n' <- ns]
            else if takeExtension n == ".csp" then return [n]
            else return []) ns

runSections ::IO [IO Bool]
runSections = do
    let 
        testDir = "tests"
        sections = ["parser", "prettyprinter", "typechecker", "evaluator"]
    
    fs <- mapM (\section -> do
            shouldPassFiles <- getAndFilterDirectoryContents $ 
                                joinPath [testDir, section, "should_pass"]
            shouldFailFiles <- getAndFilterDirectoryContents $ 
                                joinPath [testDir, section, "should_fail"]
            shouldWarnFiles <- getAndFilterDirectoryContents $
                                joinPath [testDir, section, "should_warn"]
        
            case lookup section testFunctions of
                Just test ->
                    let
                        pf = [runTest (joinPath [testDir, section, "should_pass", f]) 
                            test PassedNoWarnings | f <- shouldPassFiles]
                        ff = [runTest (joinPath [testDir, section, "should_fail", f]) 
                            test ErrorOccured | f <- shouldFailFiles]
                        wf = [runTest (joinPath [testDir, section, "should_warn", f]) 
                            test WarningsEmitted | f <- shouldWarnFiles]
                    in return $ pf++ff++wf
                Nothing -> return []
        ) sections
    return $ concat fs

runTest :: FilePath -> (FilePath -> TestM a) -> RunResult -> IO Bool
runTest fp test expectedResult = do
    putStr $ "Running test "++fp++"..."
    s <- initTestState
    res <- tryM $ runTestM s $ do
        test fp
        getState lastWarnings
    let
        failed :: Maybe Doc -> IO Bool
        failed (Just e) = do
            putStrLn "FAILED"
            putStrLn $ show e
            return False
        failed Nothing = do
            putStrLn "FAILED"
            return False
        passed = do
            putStrLn "Passed"
            return True
        
        shouldPass = expectedResult == PassedNoWarnings
        shouldFail = expectedResult == ErrorOccured
        shouldWarn = expectedResult == WarningsEmitted
    case res of 
        Left (SourceError e) -> if shouldFail then passed else failed (Just (prettyPrint e))
        Right [] -> if shouldPass then passed else failed Nothing
        Right ws -> if shouldWarn then passed else failed (Just (prettyPrint ws))
        _ -> failed (Just (text "Internal Error"))

testFunctions = [
        ("parser", parserTest),
        ("typechecker", typeCheckerTest),
        ("prettyprinter", prettyPrinterTest),
        ("evaluator", evaluatorTest)
    ]

typeCheckerTest :: FilePath -> TestM ()
typeCheckerTest fp = do
    ms <- disallowErrors (parseFile fp)
    ms <- CSPM.renameFile ms
    typeCheckFile ms
    return ()

parserTest :: FilePath -> TestM ()
parserTest fp = do
    ms <- parseFile fp
    -- Force evaluation of the whole of ms. We can't just use seq
    -- as this would leave thunks in the data structure. Instead we take
    -- the length of the string representing ms and then compute the length
    (length (show ms)) `seq` (return ())

prettyPrinterTest :: FilePath -> TestM ()
prettyPrinterTest fp = do
    ms <- disallowErrors (parseFile fp)
    let str = show (prettyPrint ms)
    ms' <- parseStringAsFile str
    if ms /= ms' then throwException UserError else return ()

disallowErrors :: TestM a -> TestM a
disallowErrors a = do
    res <- tryM a
    case res of
        Left e -> panic $ show $ text "Test failed at an unexpected point:"
                    $$ tabIndent (text (show e))
        Right v -> return v

evaluatorTest :: FilePath -> TestM ()
evaluatorTest fp = do
    let 
        evalExpr :: String -> Type -> TestM (Value UnCompiledCSPOp)
        evalExpr s t = do
            tce <- disallowErrors $ do
                e <- parseExpression s
                rne <- renameExpression e
                tce <- ensureExpressionIsOfType t rne
                desugarExpression tce
            evaluateExpression tce
    
    dsms <- disallowErrors $ do
        ms <- parseFile fp
        rms <- CSPM.renameFile ms
        tms <- typeCheckFile rms
        dsms <- desugarFile tms
        bindFile dsms
        return dsms

    -- Extract all declarations of the form "test...", which should be of
    -- patterns of type :: Bool
    mapM_ (\ (GlobalModule ds) -> mapM_ (\ d ->
        case d of 
            PatBind p _ ->
                case unAnnotate p of
                    PVar n -> do
                        let OccName s = nameOccurrence n
                        when ("test" `isPrefixOf` s) $ do
                            VBool b <- evalExpr s TBool
                            when (not b) $
                                throwSourceError [mkErrorMessage (loc p) 
                                            (prettyPrint n <+> text "was false")
                                        ]
                        when ("procTest" `isPrefixOf` s) $ do
                            VProc proc <- evalExpr s TProc
                            let expectedOutputFile = 
                                    (dropExtension fp)++"-"++s++"-expected.txt"
                            expectedOutput <- liftIO $ readFile expectedOutputFile
                            let output = prettyPrintAllRequiredProcesses proc
                            when (show output /= expectedOutput) $
                                throwSourceError [mkErrorMessage (loc p) $
                                        text "The output of" 
                                        <+> prettyPrint n 
                                        <+> text "did not match the expected output."
                                        <+> text "The actual output was:"
                                        $$ tabIndent output
                                    ]
                    _ -> return ()
            _ -> return ()
            ) (map unAnnotate ds)
        ) (map unAnnotate dsms)
