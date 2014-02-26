{-# LANGUAGE QuasiQuotes #-}
module CSPM.HaskellEvaluator (
    TranslationState, TranslationMonad,
    defaultTranslationState, runFromStateToState,
    bindToEnvironment, evaluateExpression,
    EvaluatorOptions(..), defaultEvaluatorOptions,
    ProfilerOptions(..), defaultProfilerOptions,
    ProfilingData, ProcName, UProc, Value,
) where

import Control.Applicative ((<$>))
import Control.Monad.State

import Unsafe.Coerce

import CSPM.HaskellEvaluator.ConcretiseAmbiguousTypes
import CSPM.HaskellEvaluator.GhcJIT
import CSPM.HaskellEvaluator.Monad
import CSPM.HaskellEvaluator.Prelude
import CSPM.HaskellEvaluator.Translate

import CSPM.DataStructures.Syntax
import CSPM.PrettyPrinter ()
import Util.Exception
import Util.MonadicPrettyPrint

runFromStateToState :: TranslationState -> TranslationMonad a -> 
    IO (a, TranslationState)
runFromStateToState state prog = runStateT prog state

defaultTranslationState :: IO TranslationState
defaultTranslationState = do
    (_, st) <- runFromStateToState initialTranslationState $
        bindInitialEnvironment
    return st

bindToEnvironment :: TCCSPMFile -> TranslationMonad ()
bindToEnvironment file =
    bindAndLoadModule $ translateFile (concretiseAmbiguousTypes file)

evaluateExpression :: TCExp -> TranslationMonad (Either String LibCSPMException)
evaluateExpression expression = do    
    generatedCode <- renderStyle (Style PageMode 1000000 1.5) $
        text "cspm_evaluateExpression"
        <+> parens (translateExpression (concretiseAmbiguousTypes expression))
    act <- unsafeCoerce <$> evaluateExpr generatedCode
    (success, printedValue) <- liftIO $ act
    if success then return $ Left printedValue
        else return $ Right $ PreFormattedSourceError printedValue

data EvaluatorOptions = EvaluatorOptions {
        profilerOptions :: ProfilerOptions
    }

defaultEvaluatorOptions :: EvaluatorOptions
defaultEvaluatorOptions = EvaluatorOptions {
        profilerOptions = defaultProfilerOptions
    }

data ProfilerOptions = ProfilerOptions {}

data ProfilingData = ProfilingData {}

data ProcName = ProcName {}

data UProc = UProc {}

data Value = Value {}

defaultProfilerOptions = ProfilerOptions {}
