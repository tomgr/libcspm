{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Test.Monad where

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
    sess <- newCSPMSession
    return $ TestState sess []

resetCSPM :: Test ()
resetCSPM = do
    sess <- liftIO $ newCSPMSession
    modify (\st -> st { cspmSession = sess, lastWarnings = [] })

type Test = StateT TestState IO

runTestM :: TestState -> Test a -> IO a
runTestM st a = runStateT a st >>= return . fst

getState :: (TestState -> a) -> Test a
getState = gets

modifyState :: (TestState -> TestState) -> Test ()
modifyState = modify

instance CSPMMonad Test where
    getSession = gets cspmSession
    setSession s = modify (\ st -> st { cspmSession = s })
    handleWarnings ws = modify (\ st -> st { lastWarnings = ws })
