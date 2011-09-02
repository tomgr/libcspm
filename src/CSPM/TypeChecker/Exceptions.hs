{-# LANGUAGE DeriveDataTypeable #-}
module CSPM.TypeChecker.Exceptions (
    Error, Warning,
    varNotInScopeMessage,
    infiniteUnificationMessage,
    unificationErrorMessage,
    duplicatedDefinitionsMessage,
    incorrectArgumentCountMessage,
    constraintUnificationErrorMessage,
    transparentFunctionNotRecognised,
    externalFunctionNotRecognised,
    deprecatedNameUsed,
    unsafeNameUsed,
)
where

import Data.Typeable
import Data.List (group, sort)
import Prelude

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.PrettyPrinter
import Util.Annotated
import Util.Exception
import Util.PartialFunctions
import Util.PrettyPrint

type Error = Doc
type Warning = Doc

varNotInScopeMessage :: Name -> Error
varNotInScopeMessage n = prettyPrint n <+> text "is not in scope"

incorrectArgumentCountMessage :: Doc -> Int -> Int -> Error
incorrectArgumentCountMessage func expected actual = 
    hang (hang (text "The function") tabWidth func) tabWidth
        (text "was supplied with" <+> int actual <+> 
        text "arguments, but was expecting" <+> int expected)

infiniteUnificationMessage :: Type -> Type -> Error
infiniteUnificationMessage t1 t2 = 
    let [ppt1, ppt2] = prettyPrintTypes [t1, t2] in
    text "Cannot construct the infinite type:" <+> ppt1 <+> equals <+> ppt2

unificationErrorMessage :: [(Type, Type)] -> Error
unificationErrorMessage unificationStack = 
    let 
        hd = head unificationStack
        lt = last unificationStack
        (it1, it2) = hd
        (t1, t2) = lt
        [pit1, pit2, pt1, pt2] = prettyPrintTypes [it1, it2, t1, t2]
    in
        sep [text "Couldn't match expected type" <+> pit1,
            nest 8 (text "with actual type" <+> pit2)]
        $$
        if hd == lt then empty
        else sep [text "whilst matching expected type" <+> pt1,
            nest 8 (text "with actual type" <+> pt2)]

constraintUnificationErrorMessage :: Constraint -> Type -> Error
constraintUnificationErrorMessage c t = 
    hang (hang (text "The type") tabWidth (prettyPrint t)) tabWidth
        (text "does not have the constraint" <+> prettyPrint c)

duplicatedDefinitionsMessage :: [(Name, SrcSpan)] -> [Error]
duplicatedDefinitionsMessage ns = duplicatedDefinitionsMessage' $
    let
        names = map fst ns
        dupNames = (map head . filter (\ g -> length g > 1) . group . sort) names
    in [(n, applyRelation ns n) | n <- dupNames]

duplicatedDefinitionsMessage' :: [(Name, [SrcSpan])] -> [Error]
duplicatedDefinitionsMessage' nlocs = 
    map (\ (n, spans) ->
        hang (text "The variable" <+> prettyPrint n 
                <+> text "has multiple definitions at" <> colon) tabWidth
            (vcat (map prettyPrint spans))) nlocs

transparentFunctionNotRecognised :: Name -> Error
transparentFunctionNotRecognised n =
    text "The transparent function" <+> prettyPrint n <+> 
    text "is not recognised."
externalFunctionNotRecognised :: Name -> Error
externalFunctionNotRecognised n = 
    text "The external function" <+> prettyPrint n <+> 
    text "is not recognised."

deprecatedNameUsed :: Name -> Maybe Name -> Error
deprecatedNameUsed n Nothing = 
    prettyPrint n <+> text "is deprecated."
deprecatedNameUsed n (Just replacement) = 
    prettyPrint n <+> text "is deprecated - use" <+>
    prettyPrint replacement <+> text "instead."

unsafeNameUsed :: Name -> Error
unsafeNameUsed n =
    text "The invocation of" <+> prettyPrint n 
        <+> text "has not been type-checked."
    <+> text "Therefore, a runtime type error may occur here."
