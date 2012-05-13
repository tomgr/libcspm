{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, StandaloneDeriving, TypeSynonymInstances #-}
module Monad where

import Control.Monad.State

import CSPM
import CSPM.Operators.CSP
import Util.Exception
import Util.PrettyPrint

data TestState p ops = TestState {
        cspmSession :: CSPMSession p ops,
        lastWarnings :: [ErrorMessage]
    }

initTestState :: IO (TestState () UnCompiledCSPOp)
initTestState = do
    sess <- newCSPMSession
    return $ TestState sess []

resetCSPM :: Test () UnCompiledCSPOp ()
resetCSPM = do
    sess <- liftIO $ newCSPMSession
    modify (\st -> st { cspmSession = sess, lastWarnings = [] })

type TestM = Test () UnCompiledCSPOp

newtype Test p ops a = Test { unTest :: StateT (TestState p ops) IO a }
    deriving (Monad, MonadIO, MonadIOException)

deriving instance MonadState (TestState () UnCompiledCSPOp) (Test () UnCompiledCSPOp)

runTestM :: TestState () UnCompiledCSPOp -> Test () UnCompiledCSPOp a -> IO a
runTestM st a = runStateT (unTest a) st >>= return . fst

getState :: (TestState () UnCompiledCSPOp -> a) -> Test () UnCompiledCSPOp a
getState = gets

modifyState :: (TestState () UnCompiledCSPOp -> TestState () UnCompiledCSPOp) -> 
    Test () UnCompiledCSPOp ()
modifyState = modify

instance CSPMMonad Test () UnCompiledCSPOp where
    getSession = gets cspmSession
    setSession s = modify (\ st -> st { cspmSession = s })
    handleWarnings ws = modify (\ st -> st { lastWarnings = ws })
