{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Sfdri.Monad where

import Control.Monad.State
import System.Console.Haskeline
import System.Directory
import System.FilePath
import qualified System.Console.Haskeline as Haskeline

import CSPM
import Util.Exception

data SfdriState = SfdriState {
        settingsDirectory :: FilePath,
        cspmSession :: CSPMSession,
        currentFilePath :: Maybe FilePath
    }

initSfdriState :: IO SfdriState
initSfdriState = do
    settingsDirectory <- getAppUserDataDirectory "sfdr"
    createDirectoryIfMissing True $ joinPath [settingsDirectory, "interactive"]
    sess <- newCSPMSession
    return $ SfdriState settingsDirectory sess Nothing

resetCSPM :: Sfdri ()
resetCSPM = do
    sess <- liftIO $ newCSPMSession
    modify (\st -> st { cspmSession = sess })

type Sfdri = StateT SfdriState IO

runSfdri :: SfdriState -> Sfdri a -> IO a
runSfdri st a = runStateT a st >>= return . fst

getState :: (SfdriState -> a) -> Sfdri a
getState = gets

modifyState :: (SfdriState -> SfdriState) -> Sfdri ()
modifyState = modify

instance CSPMMonad Sfdri where
    getSession = gets cspmSession
    setSession s = modify (\ st -> st { cspmSession = s })

instance CSPMMonad (InputT Sfdri) where
  setSession = lift . setSession
  getSession = lift getSession
