{-# LANGUAGE GeneralizedNewtypeDeriving, 
			FlexibleInstances, MultiParamTypeClasses #-} 
module Util.Monad where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.State

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

data TygerError =
	CSPMTypeCheckError String
	| CSPMParseError String
	| TygerUnknownError String
	| Panic String

instance Show TygerError where
	show (CSPMTypeCheckError s) = s
	show (CSPMParseError s) = s
	show (TygerUnknownError s) = 
		"An unknown error occured. More information: "++s
	show (Panic s) = "Internal inconsistency error! More information: "++s
		
instance Error TygerError where
	strMsg = TygerUnknownError

-- Main monad used
newtype Tyger a = Tyg {
	runTyg :: ErrorT TygerError IO a
} deriving (Monad, MonadIO, MonadError TygerError)

class Monad m => MonadTyger m where
	panic :: String -> m a
	debugOutput :: String -> m ()
instance MonadTyger Tyger where
	panic = throwError . Panic
	debugOutput = liftIO . putStrLn
instance MonadTyger m => MonadTyger (StateT s m) where
	panic = lift . panic
	debugOutput = lift . debugOutput
instance (MonadTyger m, Error e) => MonadTyger (ErrorT e m) where
	panic = lift . panic
	debugOutput = lift . debugOutput
		
runTyger :: Tyger a -> IO (Either TygerError a)
runTyger = runErrorT . runTyg
