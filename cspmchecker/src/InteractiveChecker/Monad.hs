{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TypeSynonymInstances #-}
module Monad where

import Control.Monad.State
import System.Console.Haskeline
import System.Directory
import System.FilePath

import CSPM
import Util.Exception
import Util.PrettyPrint

data ICheckerState p ops = ICheckerState {
        settingsDirectory :: FilePath,
        cspmSession :: CSPMSession p ops,
        currentFilePath :: Maybe FilePath
    }

initICheckerState :: CSPLike () p ops => IO (ICheckerState () ops)
initICheckerState = do
    settingsDirectory <- getAppUserDataDirectory "cspmchecker"
    createDirectoryIfMissing True $ joinPath [settingsDirectory, "interactive"]
    sess <- newCSPMSession
    return $ ICheckerState settingsDirectory sess Nothing

resetCSPM :: CSPLike () p ops => IChecker () ops ()
resetCSPM = do
    sess <- liftIO $ newCSPMSession
    modify (\st -> st { cspmSession = sess })

newtype IChecker p ops a = IChecker {
        unIChecker :: StateT (ICheckerState p ops) IO a
    }
    deriving (Functor, Monad, MonadException, MonadIO, MonadState (ICheckerState p ops))

runIChecker :: ICheckerState p ops -> IChecker p ops a -> IO a
runIChecker st a = runStateT (unIChecker a) st >>= return . fst

instance CSPMMonad IChecker p ops where
    getSession = gets cspmSession
    setSession s = modify (\ st -> st { cspmSession = s })
    handleWarnings ws =
        liftIO $ putStrLn $ "\ESC[1;31m\STX"++show (prettyPrint ws)++"\ESC[0m\STX" 

printError :: String -> InputT (IChecker p ops) ()
printError s = outputStrLn $ "\ESC[1;31m\STX"++s++"\ESC[0m\STX" 
