module CSPM.Symmetry.BuiltInFunctions (
    isNonPolymorphicBuiltinNonSymmetric,
    isPolymorphicBuiltinNonSymmetric,
) where

import qualified Data.Set as S

import CSPM.Prelude
import CSPM.Syntax.Names

isPolymorphicBuiltinNonSymmetric :: Name -> Bool
isPolymorphicBuiltinNonSymmetric =
    let
        s = S.fromList polymorphicNonSymmetricNames
    in flip S.member s

isNonPolymorphicBuiltinNonSymmetric :: Name -> Bool
isNonPolymorphicBuiltinNonSymmetric =
    let
        s = S.fromList nonPolymorphicNonSymmetricNames
    in flip S.member s

polymorphicNonSymmetricNames ::  [Name]
polymorphicNonSymmetricNames = map builtInName [
        "mapToList", "mtransclose", "seq", "show"
    ]

nonPolymorphicNonSymmetricNames :: [Name]
nonPolymorphicNonSymmetricNames =  map builtInName [
        "chase", "chase_nocache", "deter", "model_compress"
    ]
