{-# LANGUAGE CPP #-}
-- | The CSPM profiler
--
-- This utilises the Haskell profiler to provide time and space profiling for
-- user generated CSPM. Profiling Haskell code simply requires the insertion of
-- {-# SCC name #-} tags into the Haskell code, which create 'cost centres' to
-- which time and space are assigned by the Haskell runtime. However, these cost
-- centres can only be created at compile time, not at runtime, and thus cannot
-- be directly used to profile CSPM code (since an SCC for, for example,
-- a Let binding would include costs for all let bindings in a CSPM file).
--
-- This module implements some tricks to create cost centres dynamically. The
-- main trick is to create a file (see ProfilerThunks) that contains many cost
-- centres that are identified only by integers. For instance:
--
--      push1 fn arg = {-# SCC cspm_1 #-} fn arg
--
-- This returns a new version of fn that in addition to calling the function
-- fn, additionally sets the SCC to cspm_1. At runtime, we assign dynamic cost
-- centres to each CSPM identifier (more precisely, Frame) and use the above
-- to push the cost centres on. This means that the standard Haskell profile
-- will attribute costs correctly to the dynamic cost centres (although it
-- seems you need to be a bit careful to make sure the function isn't optimised
-- in certain ways to prevent this from working).
--
-- This module goes hand-in-hand with the cspmprofiler executable which takes
-- the Haskell profile and a map from cost center to CSPM frame and produces a
-- pretty printed CSPM profile.
module CSPM.Evaluator.Profiler (
    profile, dumpProfilingData,
) where

import Prelude hiding ((<>))

import Control.Monad.Trans
import Data.Array.IArray
import Data.Array.IO
import Data.IORef
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Unsafe

import CSPM.Evaluator.AnalyserMonad
import CSPM.Evaluator.ProfilerThunks
import CSPM.PrettyPrinter
import Util.PrettyPrint

-- | Returns a version of the function such that any time/space the function
-- consumes will be marked as belonging to the given frame.
profile :: MonadIO m => FrameInformation -> (a -> b) -> m (a -> b)

#ifdef CSPM_PROFILING

profile frame fn = do
    tid <- liftIO $ takeNextThunk
    liftIO $ writeArray framesForThunk tid frame
    return $! executeWithinThunk tid fn
{-# NOINLINE profile #-}

#else

profile _ prog = return prog
{-# INLINE profile #-}

#endif

nextThunkIORef :: IORef Int
nextThunkIORef = unsafePerformIO (newIORef 0)
{-# NOINLINE nextThunkIORef #-}

framesForThunk :: IOArray Int FrameInformation
framesForThunk = unsafePerformIO (newArray_ $ bounds thunks)
{-# NOINLINE framesForThunk #-}

-- | Allocates the next thunk identifier
takeNextThunk :: IO Int
takeNextThunk = atomicModifyIORef' nextThunkIORef (\ tid -> (tid+1, tid))

frameForThunk :: MonadIO m => Int -> m FrameInformation
frameForThunk tid = liftIO $ readArray framesForThunk tid

executeWithinThunk :: Int -> (a -> b) -> (a -> b)
executeWithinThunk tid = thunks!tid

-- | Writes all profiling data to a file named <executable name>.cspm in the
-- current directory (to go with the file Haskell writes named
-- <executable name>.prof).
dumpProfilingData :: IO ()
dumpProfilingData = do
    nextTid <- readIORef nextThunkIORef
    usedFrames <- mapM frameForThunk [0..nextTid-1]

    executable <- getExecutablePath
    currentDir <- getCurrentDirectory
    let executableName = dropExtension (takeFileName executable)
        dataFileName = joinPath [currentDir, executableName++".cspm"]
        cMAX_FUNCTION_NAME = 50
        dropFromStart str =
                if ls > cMAX_FUNCTION_NAME then
                    drop (ls - cMAX_FUNCTION_NAME) str
                else str
            where ls = length str
    writeFile dataFileName $ unlines $
        zipWith (\ tid frame -> show tid ++" "++ dropFromStart (show (prettyPrintFrame frame)))
            [0..nextTid-1] usedFrames

-- | Pretty prints the frames for inclusion in the profile name map.
prettyPrintFrame frame = 
    let
        ppParentFrame Nothing = empty
        ppParentFrame (Just frame) | skipFrame frame =
            ppParentFrame (frameParent frame)
        ppParentFrame (Just frame) =
            ppFrame frame <> text "::"

        skipFrame (VariableFrame {}) = True
        skipFrame (LambdaFrame {}) = True
        skipFrame _ = False
    
        ppFrame :: FrameInformation -> Doc
        ppFrame frame@(BuiltinFunctionFrame {}) =
            ppParentFrame (frameParent frame)
            <> prettyPrint (builtinFunctionFrameFunctionName frame)
        ppFrame frame@(FunctionFrame {}) =
            ppParentFrame (frameParent frame)
            <> prettyPrint (functionFrameFunctionName frame)
        ppFrame frame@(LambdaFrame {}) =
            ppParentFrame (frameParent frame)
            <> parens (
                char '\\'
                <> list (map prettyPrint (lambdaFramePatterns frame))
                <+> char '@'
                <+> prettyPrint (lambdaFrameExpression frame)
            )
        ppFrame frame@(PartiallyAppliedFunctionFrame {}) =
            ppParentFrame (frameParent frame)
            <> (prettyPrint (partiallyAppliedFunctionFrameFunctionName frame))
        ppFrame frame@(VariableFrame {}) = 
            ppParentFrame (frameParent frame)
            <> text "ANNON"
            <> parens (list (map prettyPrint (variableFramePatterns frame)))
    in
        ppFrame frame
