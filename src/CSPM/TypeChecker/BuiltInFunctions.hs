{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module CSPM.TypeChecker.BuiltInFunctions(
    injectBuiltInFunctions
) where

import Control.Monad

import CSPM.DataStructures.Names
import CSPM.DataStructures.Types
import CSPM.Prelude
import CSPM.TypeChecker.Monad hiding (isDeprecated, isTypeUnsafe)

injectBuiltInFunctions :: TypeCheckMonad ()
injectBuiltInFunctions =
    -- We inject all builtins, including transparent and external functions
    -- as everything has been renamed, meaning this won't overwrite any
    -- user defined functions
    mapM_ (\ b -> do
        setType (name b) (typeScheme b)
        when (isDeprecated b) $ 
            markAsDeprecated (name b) (deprecatedReplacement b)
        when (isTypeUnsafe b) $
            markTypeAsUnsafe (name b)) (builtins True)
