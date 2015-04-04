module CSPM.Evaluator.AnalyserMonad (
    AnalyserState, initialAnalyserState,
    AnalyserMonad, runAnalyser, getState,
    withinTimedSection, maybeTimed,
    isProfilerActive,

    DataTypeInformation(..), DataTypeConstructor(..), FieldSet(..),
    dataTypeForName, addDataTypes, channelInformationForName,
) where

import Control.Monad.State
import qualified Data.Map as M

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax hiding (timedSectionTockName)
import CSPM.DataStructures.Types
import CSPM.Prelude
import Util.Exception

data AnalyserState = AnalyserState {
        timedSectionTockName :: Maybe Name,
        timedSectionFunctionName :: Maybe Name,
        profilerActive :: Bool,
        registeredDataTypes :: M.Map Name DataTypeInformation
    }

isProfilerActive :: AnalyserMonad Bool
isProfilerActive = gets profilerActive

initialAnalyserState :: Bool -> IO AnalyserState
initialAnalyserState profilerActive = do
    return $ AnalyserState {
        timedSectionFunctionName = Nothing,
        timedSectionTockName = Nothing,
        profilerActive = profilerActive,
        registeredDataTypes = M.empty
    }

type AnalyserMonad = StateT AnalyserState IO

runAnalyser :: AnalyserState -> AnalyserMonad a -> IO (a, AnalyserState)
runAnalyser st prog = runStateT prog st

getState :: AnalyserMonad AnalyserState
getState = gets id

withinTimedSection :: Name -> Name -> AnalyserMonad a -> AnalyserMonad a
withinTimedSection tn fn prog = do
    oldTn <- gets timedSectionTockName
    oldFn <- gets timedSectionFunctionName
    modify (\ st -> st {
            timedSectionTockName = Just tn,
            timedSectionFunctionName = Just fn
        })
    a <- prog
    modify (\ st -> st {
            timedSectionTockName = oldTn,
            timedSectionFunctionName = oldFn
        })
    return a

maybeTimed ::
    AnalyserMonad a ->
    (Name -> Name -> AnalyserMonad a) ->
    AnalyserMonad a
maybeTimed nonTimedProg timedProg = do
    mtn <- gets timedSectionTockName
    mfn <- gets timedSectionFunctionName
    case (mtn, mfn) of
        (Just tn, Just fn) -> timedProg tn fn
        _ -> nonTimedProg

-- data StaticProcessName = StaticProcessName {
--        -- | The name of the process in question.
--        processName :: Name,
--        parentProcessName :: StaticProcessName,
--        -- | The list of free variables the process depends on.
--        freeVars :: [Name]
--    }

--data EvaluatedProcessName = EvaluatedProcessName {
--        processName :: Name,
--        freeVariableValues :: [Value]
--    }

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
        dataTypeConstructors :: M.Map Name DataTypeConstructor
    }
    deriving Show

channelInformationForName :: Name -> AnalyserMonad DataTypeConstructor
channelInformationForName name = do
    dc <- dataTypeForName (builtInName "Events")
    case M.lookup name (dataTypeConstructors dc) of
        Just c -> return c
        Nothing -> panic $ "Could not find registered channel "++show name

dataTypeForName :: Name -> AnalyserMonad DataTypeInformation
dataTypeForName name = do
    dataTypes <- gets registeredDataTypes
    case M.lookup name dataTypes of
        Just typ -> return typ
        Nothing -> panic $ "Could not find registerd datatype "++show name

addDataTypes :: [DataTypeInformation] -> AnalyserMonad ()
addDataTypes newDataTypes = do
    oldDataTypes <- gets registeredDataTypes
    modify (\ st -> st { registeredDataTypes =
            foldr (\ c -> M.insert (dataTypeName c) c) oldDataTypes newDataTypes
        })
