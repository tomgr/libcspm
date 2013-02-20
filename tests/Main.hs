{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
module Main (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.List
import CSPM
import CSPM.Evaluator.ProcessValues
import CSPM.Evaluator.ValuePrettyPrinter
import Monad
import Prelude hiding (catch)
import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import Test.Framework
import qualified Test.Framework.Providers.API as T
import Test.Framework.Runners.Console
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
    defaultMain tests

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

data LibCSPMTest = IOTestFunction (IO LibCSPMTestResult)
data LibCSPMTestRunning = LibCSPMTestRunning
data LibCSPMTestResult =
    LibCSPMTestResult RunResult RunResult [ErrorMessage] [ErrorMessage]
    | LibCSPMTestPanic String

instance Show LibCSPMTestRunning where
    show _ = "Running"
instance Show LibCSPMTestResult where
    show (LibCSPMTestResult r1 r2 _ _) | r1 == r2 = "OK"
    show (LibCSPMTestResult ErrorOccured PassedNoWarnings es ws) = 
        "Failed (test should have failed but passed)"
    show (LibCSPMTestResult ErrorOccured WarningsEmitted es ws) = 
        "Failed (test should have failed but only warned)"
    show (LibCSPMTestResult WarningsEmitted PassedNoWarnings es ws) =
        "Failed (test passed but should have emitted warnings)"
    show (LibCSPMTestResult WarningsEmitted ErrorOccured es ws) =
        "Failed (test failed but should have only emitted warnings)\n"
        ++ show (prettyPrint es)
    show (LibCSPMTestResult PassedNoWarnings WarningsEmitted es ws) =
        "Failed (test emitted warnings but should have passed)\n"
        ++ show (prettyPrint ws)
    show (LibCSPMTestResult PassedNoWarnings ErrorOccured es ws) =
        "Failed (test failed but should have passed)\n"
        ++ show (prettyPrint es)
    show (LibCSPMTestPanic es) = "Unexpected error\n"++es
instance T.TestResultlike LibCSPMTestRunning LibCSPMTestResult where
    testSucceeded (LibCSPMTestResult r1 r2 _ _) = r1 == r2
    testSucceeded (LibCSPMTestPanic _) = False
instance T.Testlike LibCSPMTestRunning LibCSPMTestResult LibCSPMTest where
    runTest topts (IOTestFunction func) = T.runImprovingIO $ T.liftIO func
    testTypeName _ = "Test Cases"

runSections :: IO [Test]
runSections = do
    let 
        testDir = "tests"
        sections = map fst testFunctions
    
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
                        mkTest dir1 f expectedResult =
                            let path = joinPath [testDir, section, dir1, f]
                            in T.Test f (IOTestFunction $ makeTest path test expectedResult)
                        pf = [mkTest "should_pass" f PassedNoWarnings | f <- shouldPassFiles]
                        ff = [mkTest "should_fail" f ErrorOccured | f <- shouldFailFiles]
                        wf = [mkTest "should_warn" f WarningsEmitted | f <- shouldWarnFiles]
                    in return [testGroup section (pf++ff++wf)]
                Nothing -> return []
        ) sections
    return $ concat fs

makeTest :: FilePath -> (FilePath -> TestM a) -> RunResult -> IO LibCSPMTestResult
makeTest fp test expectedResult =
    catch (do
        s <- initTestState
        res <- tryM $ runTestM s $ do
                    test fp
                    getState lastWarnings
        return $! case res of 
                    Left (SourceError e) -> LibCSPMTestResult expectedResult ErrorOccured e []
                    Right [] -> LibCSPMTestResult expectedResult PassedNoWarnings [] []
                    Right ws -> LibCSPMTestResult expectedResult WarningsEmitted [] ws
    ) (\ (e :: SomeException) -> return $ LibCSPMTestPanic (show e))

testFunctions = [
        ("parser", parserTest),
        ("typechecker", typeCheckerTest),
        ("prettyprinter", prettyPrinterTest),
        ("evaluator", evaluatorTest),
        ("desugar", desugarTest)
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

desugarTest :: FilePath -> TestM ()
desugarTest fp = do
    tms <- disallowErrors $ do
        ms <- parseFile fp
        rms <- CSPM.renameFile ms
        typeCheckFile rms
    dsms <- desugarFile tms
    return ()

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
        evalExpr :: String -> Type -> TestM Value
        evalExpr s t = do
            tce <- disallowErrors $ do
                e <- parseExpression s
                rne <- renameExpression e
                tce <- ensureExpressionIsOfType t rne
                desugarExpression tce
            evaluateExpression tce
    
    CSPMFile ds <- disallowErrors $ do
        ms <- parseFile fp
        rms <- CSPM.renameFile ms
        tms <- typeCheckFile rms
        dsms <- desugarFile tms
        bindFile dsms
        return $ unAnnotate dsms

    -- Extract all declarations of the form "test...", which should be of
    -- patterns of type :: Bool
    mapM_ (\ d ->
        case d of 
            PatBind p _ ->
                case unAnnotate p of
                    PVar n -> do
                        let s = show n
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
