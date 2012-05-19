{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TypeSynonymInstances #-}
module Monad where

import Control.Monad.State

import CSPM
import Util.Exception
import Util.PrettyPrint

data CheckerState c ops = CheckerState {
        cspmSession :: CSPMSession c ops,
        lastWarnings :: [ErrorMessage]
    }

initCheckerState :: CSPLike () p ops => IO (CheckerState () ops)
initCheckerState = do
    sess <- newCSPMSession
    return $ CheckerState sess []

resetCSPM :: CSPLike () p ops => Checker () ops ()
resetCSPM = do
    sess <- liftIO $ newCSPMSession
    modify (\st -> st { cspmSession = sess, lastWarnings = [] })

newtype Checker p ops a = Checker {
       unChecker :: StateT (CheckerState p ops) IO a
    }
    deriving (Monad, MonadIO, MonadIOException, MonadState (CheckerState p ops))

runChecker :: CheckerState p ops -> Checker p ops a -> IO a
runChecker st a = runStateT (unChecker a) st >>= return . fst

instance CSPMMonad Checker p ops where
    getSession = gets cspmSession
    setSession s = modify (\ st -> st { cspmSession = s })
    handleWarnings ws = modify (\ st -> st { lastWarnings = ws })
