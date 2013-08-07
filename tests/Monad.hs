{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Monad where

import Control.Monad.State

import CSPM
import Util.Exception
import Util.PrettyPrint

data TestState = TestState {
        cspmSession :: CSPMSession,
        lastWarnings :: [ErrorMessage]
    }

initTestState :: IO TestState
initTestState = do
    sess <- newCSPMSession defaultEvaluatorOptions
    return $ TestState sess []

resetCSPM :: TestM ()
resetCSPM = do
    sess <- liftIO $ newCSPMSession defaultEvaluatorOptions
    modify (\st -> st { cspmSession = sess, lastWarnings = [] })

type TestM = StateT TestState IO

runTestM :: TestState -> TestM a -> IO a
runTestM st a = runStateT a st >>= return . fst

getState :: (TestState -> a) -> TestM a
getState = gets

modifyState :: (TestState -> TestState) -> TestM ()
modifyState = modify

instance CSPMMonad TestM where
    getSession = gets cspmSession
    setSession s = modify (\ st -> st { cspmSession = s })
    handleWarnings ws = modify (\ st -> st { lastWarnings = ws })
