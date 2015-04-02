module CSPM.Evaluator.AnalyserMonad (
    AnalyserState, initialAnalyserState,
    AnalyserMonad, runAnalyser, getState,
    withinTimedSection, maybeTimed,
    isProfilerActive,
) where

import Control.Monad.State

import CSPM.DataStructures.Names

data AnalyserState = AnalyserState {
        timedSectionTockName :: Maybe Name,
        timedSectionFunctionName :: Maybe Name,
        profilerActive :: Bool
    }

isProfilerActive :: AnalyserMonad Bool
isProfilerActive = gets profilerActive

initialAnalyserState :: Bool -> IO AnalyserState
initialAnalyserState profilerActive = do
    return $ AnalyserState {
        timedSectionFunctionName = Nothing,
        timedSectionTockName = Nothing,
        profilerActive = profilerActive
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
