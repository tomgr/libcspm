{-# LANGUAGE TemplateHaskell #-}
module CSPM.Evaluator.ProfilerThunksTH (thunkCount, genThunk) where

import Language.Haskell.TH

thunkCount :: Int
thunkCount = 20000
genThunk n = funD (mkName $ "cspm_" ++ show n) [clause [varP fn, varP arg] (normalB (appE (varE fn) (varE arg))) []]
    where
        fn = mkName "fn"
        arg = mkName "arg"