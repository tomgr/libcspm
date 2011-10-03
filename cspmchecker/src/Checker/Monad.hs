{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Monad where

import Control.Monad.State

import CSPM
import Util.Exception
import Util.PrettyPrint

data SfdrState = SfdrState {
        cspmSession :: CSPMSession,
        lastWarnings :: [ErrorMessage]
    }

initSfdrState :: IO SfdrState
initSfdrState = do
    sess <- newCSPMSession
    return $ SfdrState sess []

resetCSPM :: Sfdr ()
resetCSPM = do
    sess <- liftIO $ newCSPMSession
    modify (\st -> st { cspmSession = sess, lastWarnings = [] })

type Sfdr = StateT SfdrState IO

runSfdr :: SfdrState -> Sfdr a -> IO a
runSfdr st a = runStateT a st >>= return . fst

getState :: (SfdrState -> a) -> Sfdr a
getState = gets

modifyState :: (SfdrState -> SfdrState) -> Sfdr ()
modifyState = modify

instance CSPMMonad Sfdr where
    getSession = gets cspmSession
    setSession s = modify (\ st -> st { cspmSession = s })
    handleWarnings ws = modify (\ st -> st { lastWarnings = ws })
