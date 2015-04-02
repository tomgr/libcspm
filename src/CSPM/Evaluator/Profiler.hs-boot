module CSPM.Evaluator.Profiler where

data ProfilerOptions
data ProfilerState

initialProfilerState :: ProfilerOptions -> IO ProfilerState
