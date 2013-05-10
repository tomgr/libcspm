{-# LANGUAGE DeriveDataTypeable #-}
module CSPM.TypeChecker.Exceptions (
    Error, Warning,
    infiniteUnificationMessage,
    unificationErrorMessage,
    incorrectArgumentCountMessage,
    constraintUnificationErrorMessage,
    deprecatedNameUsed,
    unsafeNameUsed,
    illegalModuleInstanceCycleErrorMessage,
    ErrorOptions(..), defaultErrorOptions,
)
where

import Prelude

import Data.List (nub, sort)
import CSPM.DataStructures.Names
import CSPM.DataStructures.Types
import CSPM.PrettyPrinter
import Util.Exception
import Util.PrettyPrint

type Error = Doc
type Warning = Doc

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
unificationErrorMessage [] = panic "Empty unification stack during error"
unificationErrorMessage unificationStack = 
    let 
        hd = head unificationStack
        lt = last unificationStack
        (it1, it2) = hd
        (t1, t2) = lt
        ts = [it1, it2, t1, t2]
        [pit1, pit2, pt1, pt2] = prettyPrintTypes ts
    in
        sep [text "Couldn't match expected type" <+> pit1,
            nest 8 (text "with actual type" <+> pit2)]
        $$
        (if hd == lt then empty
            else sep [text "whilst matching expected type" <+> pt1,
                nest 8 (text "with actual type" <+> pt2)])
        $$ tabIndent (printOrigins ts)

printOrigins :: [Type] -> Doc
printOrigins ts =
    let 
        typeVarRefs = concatMap (map fst . collectConstraints) ts
        rigidVars = nub $ sort $ filter isRigid typeVarRefs

        printOrigin (RigidTypeVarRef _ _ n) =
            sep [prettyPrint n <+> text
                "is the rigid type variable bound by the type signature at",
                nest 4 (prettyPrint (nameDefinition n))]
    in vcat $ map printOrigin rigidVars

constraintUnificationErrorMessage :: Constraint -> Type -> Error
constraintUnificationErrorMessage c t = 
    hang (hang (text "The type") tabWidth (prettyPrint t)) tabWidth
        (text "does not have the constraint" <+> prettyPrint c)
    $$ tabIndent (printOrigins [t])
    $$ case t of
        TVar v | isRigid v -> 
            let n = rigidName v in
            text "Maybe try adding" <+> prettyPrint c <+> prettyPrint n
            <+> text "to the type-constraint at:"
            $$ nest 4 (prettyPrint (nameDefinition n))
        _ -> empty

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
    $$ text "Therefore, a runtime type error may occur."

illegalModuleInstanceCycleErrorMessage :: Name -> Name -> [Name] -> Error
illegalModuleInstanceCycleErrorMessage mName iName path = 
    fsep [text "The module" <+> prettyPrint mName,
        text "uses a definition in an instance" <+> prettyPrint iName,
        text "of itself, which is not allowed."]
    $$ text "The path by which the module calls its instance is:"
    $$ tabIndent (list (map prettyPrint path))

-- | A datatype used to hold which errors and warnings to actually emit.
data ErrorOptions = ErrorOptions {
        warnDeprecatedNamesUsed :: Bool,
        warnUnsafeNamesUsed :: Bool
    }

defaultErrorOptions :: ErrorOptions
defaultErrorOptions = ErrorOptions {
        warnDeprecatedNamesUsed = True,
        warnUnsafeNamesUsed = True
    }
