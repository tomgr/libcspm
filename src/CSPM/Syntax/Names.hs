{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
    TypeSynonymInstances #-}
-- | Names used by the evaluator. This is heavily inspired by GHC.
module CSPM.Syntax.Names (
    -- * Data Types
    OccName(..),
    UnRenamedName(..),
    Name(..),
    NameType(..),
    -- * Construction Helpers
    mkExternalName, mkInternalName, mkWiredInName, mkFreshInternalName,
    -- * Utility Functions
    isNameDataConstructor,
) where

import Prelude hiding ((<>))

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import Data.Hashable
import Data.IORef
import Data.Supply
import Data.Typeable
import System.IO.Unsafe

import Util.Annotated
import qualified Util.MonadicPrettyPrint as M
import Util.PrettyPrint

-- | A name that occurs in the source code somewhere.
data OccName = OccName B.ByteString
    deriving (Eq, Ord, Show, Typeable)

instance PrettyPrintable OccName where
    prettyPrint (OccName s) = bytestring s
instance (Applicative m, Monad m) => M.MonadicPrettyPrintable m OccName where
    prettyPrint (OccName s) = M.bytestring s

-- | A name that has not yet been renamed. Created by the parser.
data UnRenamedName =
    UnQual OccName
    | Qual {
        unRenamedNameModuleName :: OccName,
        unRenamedNameMemberName :: UnRenamedName
    }
    deriving (Eq, Ord, Show, Typeable)

instance PrettyPrintable UnRenamedName where
    prettyPrint (UnQual n) = prettyPrint n
    prettyPrint (Qual mn n) = prettyPrint mn <> text "::" <> prettyPrint n

instance (Applicative m, Monad m) => M.MonadicPrettyPrintable m UnRenamedName where
    prettyPrint (UnQual n) = M.prettyPrint n
    prettyPrint (Qual mn n) = M.prettyPrint mn M.<> M.text "::" M.<> M.prettyPrint n

-- | A renamed name and is the exclusive type used after the renamer. Names
-- are guaranteed to be unique, meaning that two names are equal iff they
-- refer to the same binding instance. For example, consider the following CSPM
-- code:
--
-- @
--      f = 1
--      g = let f = 2 within (f, f)
-- @
--
-- This will be renamed to:
--
-- @
--      f0 = 1
--      g = let f1 = 2 within (f1, f1)
-- @
--
data Name =
    Name {
        -- | The type of this name.
        nameType :: NameType,
        -- | The original occurence of this name (used for error messages).
        nameFullyQualified :: !UnRenamedName,
        -- | Where this name was defined. If this occurs in a pattern, then it
        -- will be equal to the location of the pattern, otherwise it will be
        -- equal to the location of the definition that this name binds to.
        nameDefinition :: !SrcSpan,
        -- | The unique identifier for this name. Inserted by the renamer.
        nameUnique :: !Int,
        -- | Is this name a type constructor, i.e. a datatype or a channel?
        nameIsConstructor :: Bool
    }
    deriving Typeable

data NameType =
    -- | An externally visible name (like a top level definition).
    ExternalName
    -- | A name created by the renamer, but from the users' source (e.g. from
    -- a lambda).
    | InternalName
    -- | A built in name.
    | WiredInName
    deriving Eq

instance Eq Name where
    n1 == n2 = nameUnique n1 == nameUnique n2

instance Hashable Name where
    hashWithSalt s n = s `hashWithSalt` nameUnique n
    hash n = nameUnique n

instance Ord Name where
    compare n1 n2 = compare (nameUnique n1) (nameUnique n2)

instance PrettyPrintable Name where
    prettyPrint n = prettyPrint (nameFullyQualified n)
instance (Applicative m, Monad m) => M.MonadicPrettyPrintable m Name where
    prettyPrint n = M.prettyPrint (nameFullyQualified n)

instance Show Name where
    show n = show (prettyPrint n)

nameUniqueSupply :: IORef (Supply Int)
nameUniqueSupply = unsafePerformIO $ do
    s <- newNumSupply
    newIORef s
{-# NOINLINE nameUniqueSupply #-}

takeNameUnique :: MonadIO m => m Int
takeNameUnique = do
    s <- liftIO $ atomicModifyIORef nameUniqueSupply split2
    return $ supplyValue s

mkExternalName :: MonadIO m => UnRenamedName -> SrcSpan -> Bool -> m Name
mkExternalName o s b = do
    u <- takeNameUnique
    return $ Name ExternalName o s u b

mkInternalName :: MonadIO m => UnRenamedName -> SrcSpan -> m Name
mkInternalName o s = do
    u <- takeNameUnique
    return $ Name InternalName o s u False

mkFreshInternalName :: MonadIO m => m Name
mkFreshInternalName = do
    u <- takeNameUnique
    let s = B.pack ('i':show u)
    return $ Name InternalName (UnQual (OccName s)) Unknown u False

mkWiredInName :: MonadIO m => UnRenamedName -> Bool -> m Name
mkWiredInName o b = do
    u <- takeNameUnique
    return $ Name WiredInName o BuiltIn u b

-- | Does the given Name correspond to a data type or a channel definition.
isNameDataConstructor :: Name -> Bool
isNameDataConstructor n = nameIsConstructor n
