{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fprof-auto-top #-}
module CSPM.Evaluator.ProfilerThunks (
    thunks,
)
where

import Data.Array

#ifndef CSPM_PROFILING

thunks :: Array Int ((a -> b) -> (a -> b))
thunks = listArray (0, -1) []

#else

$(mapM genThunk [0..thunkCount])

thunks :: Array Int ((a -> b) -> (a -> b))
thunks = listArray (0, thunkCount) $(listE [varE $ mkName $ "cspm_" ++ show i | i <- [0..thunkCount]])

#endif
