{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}
module Util.Exception (
    Exception,
    SfdrException(..),
    throwException,
    tryM,
    panic, throwSourceError,
    mkErrorMessage, ErrorMessage, ErrorMessages,
)
where

import Control.Exception
import Control.Monad.State
import Data.Typeable
import List
import Prelude hiding (catch)

import {-# SOURCE #-} Util.Annotated
import Util.PrettyPrint

type ErrorMessages = [ErrorMessage]

-- | An error message that resulted from something in the user's input.
data ErrorMessage =
    ErrorMessage {
        -- | Used for sorting into order
        location :: SrcSpan,
        -- | The message
        message :: Doc
    }

mkErrorMessage :: SrcSpan -> Doc -> ErrorMessage
mkErrorMessage l d = ErrorMessage l d

instance Eq ErrorMessage where
    m1 == m2 = location m1 == location m2
instance Ord ErrorMessage where
    compare m1 m2 = compare (location m1) (location m2)

instance PrettyPrintable ErrorMessages where
    prettyPrint ms = vcat . punctuate (text "\n") . map prettyPrint . sort $ ms
instance PrettyPrintable ErrorMessage where
    prettyPrint (ErrorMessage l m) = 
        hang (prettyPrint l <> colon) 4 m

instance Show ErrorMessage where
    show m = show (prettyPrint m)

-- | Exceptions that cause Sfdr to abort whatever it is doing. 
data SfdrException =
    -- | An unexpected internal error
    Panic String
    -- | An error in the user's input occured
    | SourceError ErrorMessages
    -- | An error occured. Normally this is caught by the application and 
    -- then turned into a SourceError.
    | UserError
    deriving Typeable

instance Show SfdrException where
    show (Panic str) = "Internal inconsitancy error: "++show str
    show (SourceError ms) = show (prettyPrint ms)
    show (UserError) = "An unknown error occured."

instance Exception SfdrException

throwSourceError :: ErrorMessages -> a
throwSourceError = throwException . SourceError

panic :: String -> a
panic = throwException . Panic

throwException :: Exception e => e -> a
throwException = throw
 
-- | A class to allow catching of SourceErrors in arbitrary monads.
class Monad m => MonadIOException m where
    tryM :: (MonadIOException m) => m a -> m (Either SfdrException a)
    
instance MonadIOException IO where
    tryM prog = do
        r <- try prog
        case r of
            Left (e@(Panic s)) -> throwException e
            _ -> return r

instance MonadIOException m => MonadIOException (StateT s m) where
    tryM prog = 
        StateT $ \st -> do
            x <- tryM (runStateT prog st)
            case x of
                Right (a, s) -> return $ (Right a, s)
                Left e -> return $ (Left e, st)
