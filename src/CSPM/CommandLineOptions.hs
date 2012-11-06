{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
module CSPM.CommandLineOptions (
    Options(..),
    allOptions,
    defaultOptions,
    typeCheckerOptions,
    setOptions,
) where

import CSPM
import qualified CSPM.TypeChecker.Exceptions as TC
import System.Console.GetOpt

-- | The type of options for libcspm.
data Options = Options {
        tcOptions :: TC.ErrorOptions
    }

-- | The default set of options.
defaultOptions :: Options
defaultOptions = Options {
        tcOptions = TC.defaultErrorOptions
    }

-- | All options for the type-checker.
typeCheckerOptions :: (a -> Options) -> (a -> Options -> a) ->
    [OptDescr (a -> a)]
typeCheckerOptions get set =
    let
        setFlag func opts = set opts (func o)
            where o = get opts
    in [
        Option [] ["fno-warn-deprecations"]
            (NoArg (setFlag $ \ o -> o {
                tcOptions = (tcOptions o) {
                        TC.warnDeprecatedNamesUsed = False 
                    }
                }))
            "Disables type-checker warnings for deprecations",
        Option [] ["fno-warn-unchecked-calls"]
            (NoArg (setFlag $ \o -> o {
                tcOptions = (tcOptions o) {
                        TC.warnUnsafeNamesUsed = False 
                    }
                }))
            ("Disables type-checker warnings for function calls that cannot " ++
            "be type-checked")
        ]

-- | All available command line options.
allOptions :: (a -> Options) -> (a -> Options -> a) ->
    [OptDescr (a -> a)]
allOptions get set = typeCheckerOptions get set

-- | Sets the options to the values given.
setOptions :: CSPMMonad m => Options -> m ()
setOptions opts = do
    modifyTypeCheckerErrorOptions (\ _ -> tcOptions opts)
