module CSPM.HaskellEvaluator.Monad (
    TranslationState(..),
    TranslationMonad(..), gets, modify, withYieldTypes, withDataTypes,
    PatternSideCondition(..),
    typeForYieldType, dataTypeForName,
    DataTypePrefix(..), DataTypeConstructor(..), DataTypeInformation(..),
    FieldSet(..),
    initialTranslationState,
) where

import Control.Monad.State
import qualified Data.Map as M
import qualified GHC as G

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import Util.Exception
import Util.PrettyPrint

-- A richer structure for encoding data structures

data DataTypePrefix = DataTypePrefix {
        prefixTypeName :: Name,
        prefixMatchingConstructors :: [Name],
        prefixRemainingFieldCount :: Int,
        prefixRemainingTypes :: [Type]
    }
    deriving Show

data FieldSet =
    SimpleFieldSet {
        simpleFieldSetExpression :: TCExp
    }
    | CompoundFieldSet {
        compoundFieldSetToExtract :: Int,
        compoundFieldSetExpression :: TCExp
    }
    deriving (Eq, Ord, Show)

data DataTypeConstructor = DataTypeConstructor {
        constructorName :: Name,
        constructorFieldCount :: Int,
        constructorFieldTypes :: [Type],
        constructorDecomposedFieldSets :: [FieldSet],
        constructorFieldSetIsTrivial :: [Bool]
    }
    deriving Show

data DataTypeInformation = DataTypeInformation {
        dataTypeType :: Type,
        dataTypeName :: Name,
        -- | Map from constructor name to constructor.
        dataTypeConstructors :: M.Map Name DataTypeConstructor,
        -- | Map from remaining type to prefix.
        dataTypePrefixes :: M.Map [Type] DataTypePrefix
    }
    deriving Show

data PatternSideCondition =
    MatchSideCondition {
        patternToMatch :: TranslationMonad Doc,
        expressionToMatch :: TranslationMonad Doc
    }
    | PredicateSideCondition {
        predicateToMatch :: TranslationMonad Doc
    }

data TranslationState = TranslationState {
        registeredDataTypes :: M.Map Name DataTypeInformation,
        registeredYieldTypes :: M.Map Type Type,
        inTimedSection :: Bool,

        evaluatorState :: Maybe G.HscEnv,
        loadedModules :: [String],
        nextModuleNumber :: Int,
        pendingCodeToLoad :: Doc
    }

initialTranslationState :: TranslationState
initialTranslationState = TranslationState {
        registeredYieldTypes = M.empty,
        registeredDataTypes = M.empty,
        inTimedSection = False,
        evaluatorState = Nothing,
        loadedModules = [],
        nextModuleNumber = 1,
        pendingCodeToLoad = empty
    }

type TranslationMonad = StateT TranslationState IO

typeForYieldType :: Type -> TranslationMonad Type
typeForYieldType typ = do
    yields <- gets registeredYieldTypes
    case M.lookup typ yields of
        Just typ -> return typ
        Nothing -> panic "Could not find yield type"

dataTypeForName :: Name -> TranslationMonad DataTypeInformation
dataTypeForName name = do
    dataTypes <- gets registeredDataTypes
    case M.lookup name dataTypes of
        Just typ -> return typ
        Nothing -> panic $ "Could not find registerd datatype "++show name

withYieldTypes :: [(Type, Type)] -> TranslationMonad a -> TranslationMonad a
withYieldTypes newYields program = do
    modify (\ st -> st { registeredYieldTypes =
            foldr (uncurry M.insert) (registeredYieldTypes st) newYields
        })
    program

withDataTypes :: [DataTypeInformation] -> TranslationMonad a -> TranslationMonad a
withDataTypes newDataTypes program = do
    oldDataTypes <- gets registeredDataTypes
    modify (\ st -> st { registeredDataTypes =
            foldr (\ c -> M.insert (dataTypeName c) c) oldDataTypes newDataTypes
        })
    a <- program

    --modify (\ st -> st { registeredDataTypes = oldDataTypes })
    return a
