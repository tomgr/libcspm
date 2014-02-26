module CSPM.HaskellEvaluator.GhcJIT (
    bindAndLoadModule,
    evaluateExpr,
) where

import Control.Monad
import Control.Monad.Trans
import Data.Word
import System.Directory
import System.FilePath
import System.IO
import System.Random

import qualified DynFlags as G
import qualified ErrUtils as G
import qualified GHC as G
import qualified GHC.Paths as G ( libdir )
import qualified Outputable as G

import CSPM.HaskellEvaluator.ConstantCode
import CSPM.HaskellEvaluator.Monad
import Util.Exception
import Util.MonadicPrettyPrint

evaluateExpr :: String -> TranslationMonad G.HValue
evaluateExpr code = do
    --liftIO $ putStrLn code
    runGHCAndBindPendingCode $ G.compileExpr code

bindAndLoadModule :: TranslationMonad Doc -> TranslationMonad ()
bindAndLoadModule source = do
    pending <- gets pendingCodeToLoad
    newPending <- return pending $$ source
    modify (\ st -> st { pendingCodeToLoad = newPending })

ghcLogHandler :: G.LogAction
ghcLogHandler dflags severity srcSpan style msg = 
    case severity of
        G.SevError -> panic ("GHC error:\n"++printDoc)
        G.SevFatal -> panic ("GHC error:\n"++printDoc)
-- TODO: remove me when we're done debugging
        --G.SevWarning -> putStrLn printDoc
        _ -> return ()
    where
        cntx = G.initSDocContext dflags style
        locMsg = G.mkLocMessage severity srcSpan msg
        printDoc = show (G.runSDoc locMsg cntx)

defaultEvaluatorState :: IO G.HscEnv
defaultEvaluatorState = do
    G.defaultErrorHandler panic (G.FlushOut (return ())) $ do
        G.runGhc (Just G.libdir) $ do
            flags <- G.getSessionDynFlags
            G.defaultCleanupHandler flags $ do
                let extensions = [G.Opt_FlexibleContexts]
                    startFlags = flags {
                        G.optLevel = 3,
                        G.hscTarget = G.defaultObjectTarget,
                        G.ghcLink = G.LinkInMemory,
                        G.log_action = ghcLogHandler
                    }
                    extendedFlags = foldl G.xopt_set startFlags extensions
                G.setSessionDynFlags extendedFlags
                G.setContext [
                    G.IIDecl $ G.ImportDecl {
                            G.ideclName = G.noLoc (G.mkModuleName "Data.Map"),
                            G.ideclPkgQual = Nothing,
                            G.ideclSource = False,
                            G.ideclSafe = False,
                            G.ideclImplicit = False,
                            G.ideclQualified = True,
                            G.ideclAs = Just (G.mkModuleName "M"),
                            G.ideclHiding = Nothing
                        },
                    G.IIDecl $ G.simpleImportDecl (G.mkModuleName "Text.PrettyPrint.HughesPJ"),
                    G.IIDecl $ G.simpleImportDecl (G.mkModuleName "Prelude")
                    ]
                G.getSession

-- | Runs a program in the current GHC state, binding any waiting modules if
-- necessary.
runGHCAndBindPendingCode :: G.Ghc a -> TranslationMonad a
runGHCAndBindPendingCode program = do
    session <- gets evaluatorState
    case session of 
        Just _ -> return ()
        Nothing -> do
            session <- liftIO $ defaultEvaluatorState
            modify (\ st -> st { evaluatorState = Just session })

    pendingCode <- gets pendingCodeToLoad
    b <- isEmpty (return pendingCode)
    when (not b) $ do
        -- Load the pending code
        loadModule pendingCode
        newCode <- empty
        modify (\ st -> st { pendingCodeToLoad = newCode })

    runGHC program

-- | Executes a program in the GHC monad. Does not bind any waiting modules.
runGHC :: G.Ghc a -> TranslationMonad a
runGHC program = do
    Just ghcSession <- gets evaluatorState

    (a, ghcSession') <- liftIO $ do
        G.defaultErrorHandler panic (G.FlushOut (return ())) $ do
            G.runGhc (Just G.libdir) $ do
                flags <- G.getSessionDynFlags
                G.defaultCleanupHandler flags $ do
                    G.setSession ghcSession
                    a <- program
                    st' <- G.getSession
                    return (a, st') 

    modify (\ st -> st { evaluatorState = Just ghcSession' })
    return a

-- | Actually load a module into GHC.
loadModule :: Doc -> TranslationMonad ()
loadModule moduleSource = do
    temporaryDirectory <- liftIO $ getTemporaryDirectory

    moduleNumber <- gets nextModuleNumber
    modify (\ st -> st { nextModuleNumber = nextModuleNumber st + 1 })
    random <- liftIO $ getStdRandom random :: TranslationMonad Word64

    (generatedFilePath, generatedHandle) <-
        liftIO $ openTempFile temporaryDirectory $
            "TranslatedCSPM"++show random++"M"++show moduleNumber++"M.hs"

    finally (do
        let moduleName = dropExtension (takeFileName generatedFilePath)

        loadedModules <- gets loadedModules

        generatedCode <- renderStyle (Style PageMode 1000000 1.5) $
            text moduleHeader
            $$ text "module" <+> text moduleName <+> text "where"
            $$ text standardModuleImports
            $$ vcat (mapM (\ m -> text "import" <+> text m) loadedModules)
            $$ return moduleSource

        --liftIO $ putStrLn generatedCode
        liftIO $ writeFile "Test.hs" generatedCode
        liftIO $ hPutStr generatedHandle generatedCode
        liftIO $ hClose generatedHandle

        runGHC $ do
            imports <- G.getContext
            dflags <- G.getSessionDynFlags
            G.setSessionDynFlags dflags {
                    G.importPaths = [takeDirectory generatedFilePath]
                }
            G.setTargets =<< sequence [G.guessTarget generatedFilePath Nothing]
            G.load G.LoadAllTargets
            G.setContext $
                G.IIDecl (G.simpleImportDecl (G.mkModuleName moduleName))
                : imports

        modify (\st -> st { loadedModules = moduleName : loadedModules })
        )
        --(liftIO $ removeFile generatedFilePath)
        (return ())
