{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables,
    TypeSynonymInstances #-}
module Util.Exception (
    Exception,
    LibCSPMException(..),
    throwException,
    MonadIOException(..),
    panic, throwSourceError,
    mkErrorMessage, mkWarningMessage, 
    ErrorMessage(..), ErrorMessages,
)
where

import Control.Exception
import Control.Monad.State
import qualified Control.Monad.State.Strict as ST
import Data.Typeable
import Data.List

#if __GLASGOW_HASKELL__ < 705
import Prelude hiding (catch)
#endif

import {-# SOURCE #-} Util.Annotated
import Util.PrettyPrint

type ErrorMessages = [ErrorMessage]

-- | An error message that resulted from something in the user's input.
data ErrorMessage =
    ErrorMessage {
        -- | Used for sorting into order.
        location :: SrcSpan,
        -- | The message.
        message :: Doc
    }
    | WarningMessage {
        -- | Used for sorting into order.
        location :: SrcSpan,
        -- | The message.
        message :: Doc
    }

-- | Given a 'SrcSpan' and a pretty printed 'Doc' creates an 'ErrorMessage'.
mkErrorMessage :: SrcSpan -> Doc -> ErrorMessage
mkErrorMessage l d = ErrorMessage l d

-- | Constructs a warning from a 'SrcSpan' and a pretty printed 'Doc',
-- prepending @Warning: @ to the 'Doc'.
mkWarningMessage :: SrcSpan -> Doc -> ErrorMessage
mkWarningMessage l d = WarningMessage l (text "Warning" <> colon <+> d)

instance Eq ErrorMessage where
    m1 == m2 = location m1 == location m2
instance Ord ErrorMessage where
    compare m1 m2 = compare (location m1) (location m2)

instance PrettyPrintable ErrorMessages where
    prettyPrint ms = vcat . punctuate (text "\n") . map prettyPrint . sort $ ms
instance PrettyPrintable ErrorMessage where
    prettyPrint m = 
        hang (prettyPrint (location m) <> colon) 4 (message m)

instance Show ErrorMessage where
    show m = show (prettyPrint m)

-- | Exceptions that cause LibCSPM to abort whatever it is doing. 
data LibCSPMException =
    -- | An unexpected internal error
    Panic String
    -- | An error in the user's input occured
    | SourceError ErrorMessages
    -- | An error occured. Normally this is caught by the application and 
    -- then turned into a SourceError.
    | UserError
    deriving Typeable

instance Show LibCSPMException where
    show (Panic str) = "Internal inconsitancy error: "++str
    show (SourceError ms) = show (prettyPrint ms)
    show (UserError) = "An unknown error occured."

instance Exception LibCSPMException

-- | Throw an error message as a 'SourceError'.
throwSourceError :: ErrorMessages -> a
throwSourceError = throwException . SourceError

-- | Given a string causes a 'Panic' to be thrown.
panic :: String -> a
panic = throwException . Panic

-- | Throws an arbitrary 'Exception'.
throwException :: Exception e => e -> a
throwException = throw
 
-- | A class to allow catching of SourceErrors in arbitrary monads.
class Monad m => MonadIOException m where
    -- | Runs the action, catching any non-fatal LibCSPMExecptions (i.e. non-
    -- Panic).
    tryM :: MonadIOException m => m a -> m (Either LibCSPMException a)
    tryM prog = do
        ev <- tryM' prog
        case ev of
            Left (e@(Panic x)) -> throwException e
            _ -> return ev

    -- | Runs the action, catching all exceptions including panics.
    tryM' :: MonadIOException m => m a -> m (Either LibCSPMException a)

    -- | Converts arbitrary exceptions to panics, rethrowing them.
    convertExceptionsToPanics :: MonadIOException m => m a -> m a

    -- | Runs the action, running the finaliser if an exception is thrown. The
    -- exception is always rethrown.
    finally :: MonadIOException m => m a -> m () -> m a
    finally prog finaliser = do
        result <- tryM' $ do
            r <- prog
            finaliser
            return r
        case result of
            Left err -> finaliser >> throwException err
            Right result -> return result
    
instance MonadIOException IO where
    tryM' prog = do
        r <- try prog
        case r of
            Right a -> return $ Right a
            Left (e :: LibCSPMException) -> return $ Left e

    convertExceptionsToPanics prog = do
        r <- try prog
        case r of
            Left (e :: SomeException) -> throwException $
                case fromException e :: Maybe LibCSPMException of
                    Just e -> e
                    Nothing -> Panic (show e)
            Right a -> return a

instance MonadIOException m => MonadIOException (StateT s m) where
    tryM' prog = 
        StateT $ \st -> do
            x <- tryM' (runStateT prog st)
            case x of
                Right (a, s) -> return $ (Right a, s)
                Left e -> return $ (Left e, st)
    convertExceptionsToPanics prog = do
        StateT $ \st -> convertExceptionsToPanics (runStateT prog st)

instance MonadIOException m => MonadIOException (ST.StateT s m) where
    tryM' prog = 
        ST.StateT $ \st -> do
            x <- tryM' (ST.runStateT prog st)
            case x of
                Right (a, s) -> return $ (Right a, s)
                Left e -> return $ (Left e, st)
    convertExceptionsToPanics prog = do
        ST.StateT $ \st -> convertExceptionsToPanics (ST.runStateT prog st)
