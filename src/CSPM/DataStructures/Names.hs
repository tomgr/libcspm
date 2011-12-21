{-# LANGUAGE DeriveDataTypeable #-}
-- | Names used by the evaluator. This is heavily inspired by GHC.
module CSPM.DataStructures.Names (
    OccName(..),
    UnRenamedName(..),
    Name(..),
    NameType,

    mkExternalName, mkInternalName, mkWiredInName, mkFreshInternalName,
    isNameDataConstructor,
) where

import Control.Monad.Trans
import Data.IORef
import Data.Supply
import Data.Typeable
import System.IO.Unsafe

import Util.Annotated
import Util.PrettyPrint

-- | A name that occurs in the source code somewhere.
data OccName = 
    OccName String
    deriving (Eq, Ord, Show, Typeable)

instance PrettyPrintable OccName where
    prettyPrint (OccName s) = text s

-- | A name that has not yet been renamed. Created by the parser.
data UnRenamedName =
    UnQual OccName
    deriving (Eq, Ord, Show, Typeable)

instance PrettyPrintable UnRenamedName where
    prettyPrint (UnQual n) = prettyPrint n

-- | A renamed name and is the exclusive type used after the renamer.
data Name =
    Name {
        -- | The type of this name.
        nameType :: NameType,
        -- | The original occurence of this name (used for error messages).
        nameOccurrence :: !OccName,
        -- | Where this name was defined. If this occurs in a pattern, then it
        -- will be equal to the location of the pattern, otherwise it will be
        -- equal to the location of the definition that this name binds to.
        nameDefinition :: !SrcSpan,
        -- | The unique identifier for this name. Inserted by the renamer.
        nameUnique :: !Int,
        -- | Is this name a type constructor, i.e. a datatype or a channel?
        -- Only External names can be.
        nameIsConstructor :: Bool
    }
    deriving Typeable

data NameType =
    -- | An externally visible name (like a top level definition).
    External
    -- | A name created by the renamer, but from the users' source (e.g. from
    -- a lambda).
    | Internal
    -- | A built in name.
    | WiredIn

instance Eq Name where
    n1 == n2 = nameUnique n1 == nameUnique n2

instance Ord Name where
    compare n1 n2 = compare (nameUnique n1) (nameUnique n2)

instance PrettyPrintable Name where
    prettyPrint n = prettyPrint (nameOccurrence n)

instance Show Name where
    show n = show (prettyPrint n)

nameUniqueSupply :: IORef (Supply Int)
nameUniqueSupply = unsafePerformIO (do
    s <- newNumSupply
    newIORef s)

takeNameUnique :: MonadIO m => m Int
takeNameUnique = do
    s <- liftIO $ readIORef nameUniqueSupply
    let (s1, s2) = split2 s
    liftIO $ writeIORef nameUniqueSupply s2
    return $ supplyValue s1

mkExternalName :: MonadIO m => OccName -> SrcSpan -> Bool -> m Name
mkExternalName o s b = do
    u <- takeNameUnique
    return $ Name External o s u b

mkInternalName :: MonadIO m => OccName -> SrcSpan -> m Name
mkInternalName o s = do
    u <- takeNameUnique
    return $ Name Internal o s u False

mkFreshInternalName :: MonadIO m => m Name
mkFreshInternalName = mkInternalName (OccName "<fresh>") Unknown

mkWiredInName :: MonadIO m => OccName -> m Name
mkWiredInName o = do
    u <- takeNameUnique
    return $ Name WiredIn o Unknown u False

-- | Does the given Name correspond to a data type or a channel definition.
isNameDataConstructor :: Name -> Bool
isNameDataConstructor n = nameIsConstructor n
