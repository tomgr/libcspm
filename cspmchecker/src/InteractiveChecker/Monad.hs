{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Monad where

import Control.Monad.State.Strict
import System.Console.Haskeline
import System.Directory
import System.FilePath

import CSPM
import Util.Exception
import Util.PrettyPrint

data ICheckerState = ICheckerState {
        settingsDirectory :: FilePath,
        cspmSession :: CSPMSession,
        currentFilePath :: Maybe FilePath
    }

initICheckerState :: IO ICheckerState
initICheckerState = do
    settingsDirectory <- getAppUserDataDirectory "cspmchecker"
    createDirectoryIfMissing True $ joinPath [settingsDirectory, "interactive"]
    sess <- newCSPMSession
    return $ ICheckerState settingsDirectory sess Nothing

resetCSPM :: IChecker ()
resetCSPM = do
    sess <- liftIO $ newCSPMSession
    modify (\st -> st { cspmSession = sess })

type IChecker = StateT ICheckerState IO

runIChecker :: ICheckerState -> IChecker a -> IO a
runIChecker st a = runStateT a st >>= return . fst

getState :: (ICheckerState -> a) -> IChecker a
getState = gets

modifyState :: (ICheckerState -> ICheckerState) -> IChecker ()
modifyState = modify

instance CSPMMonad IChecker where
    getSession = gets cspmSession
    setSession s = modify (\ st -> st { cspmSession = s })
    handleWarnings = panic "Cannot handle warnings here in a pure IChecker Monad"

instance CSPMMonad (InputT IChecker) where
    setSession = lift . setSession
    getSession = lift getSession
    handleWarnings ms = printError $ show $ prettyPrint ms

printError :: String -> InputT IChecker ()
printError s = outputStrLn $ "\ESC[1;31m\STX"++s++"\ESC[0m\STX" 

