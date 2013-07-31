{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Monad where

import Control.Monad.State

import CSPM
import Util.Exception
import Util.PrettyPrint

data CheckerState = CheckerState {
        cspmSession :: CSPMSession,
        lastWarnings :: [ErrorMessage]
    }

initCheckerState :: IO CheckerState
initCheckerState = do
    sess <- newCSPMSession False
    return $ CheckerState sess []

resetCSPM :: Checker ()
resetCSPM = do
    sess <- liftIO $ newCSPMSession False
    modify (\st -> st { cspmSession = sess, lastWarnings = [] })

type Checker = StateT CheckerState IO

runChecker :: CheckerState -> Checker a -> IO a
runChecker st a = runStateT a st >>= return . fst

getState :: (CheckerState -> a) -> Checker a
getState = gets

modifyState :: (CheckerState -> CheckerState) -> Checker ()
modifyState = modify

instance CSPMMonad Checker where
    getSession = gets cspmSession
    setSession s = modify (\ st -> st { cspmSession = s })
    handleWarnings ws = modify (\ st -> st { lastWarnings = ws })
