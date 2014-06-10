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
import Data.List
import Data.Maybe

import Data.List (nub, sort)
import CSPM.DataStructures.FreeVars
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.PrettyPrinter
import Util.Annotated
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

constraintUnificationErrorMessage :: [(Constraint, Type)] -> Error
constraintUnificationErrorMessage [] = panic "Empty unification stack during error"
constraintUnificationErrorMessage unificationStack = 
    let 
        hd = head unificationStack
        lt = last unificationStack
        (tc, t) = hd
        (itc, it) = lt
        ts = [it, t]
        [pt1, pt2] = prettyPrintTypes ts
    in
        sep [text "The type" <+> pt2,
            nest 8 (text "does not have the constraint" <+> prettyPrint tc)]
        $$
        (if hd == lt then empty
            else sep [text "whilst checking that the type" <+> pt1,
                nest 8 (text "has the constraint" <+> prettyPrint itc)])
        $$ tabIndent (printOrigins ts)
        $$ case it of
            TVar v | isRigid v -> 
                let n = rigidName v in
                text "Maybe try adding" <+> prettyPrint itc <+> prettyPrint n
                <+> text "to the type-constraint at:"
                $$ nest 4 (prettyPrint (nameDefinition n))
            _ -> empty

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

illegalModuleInstanceCycleErrorMessage :: [TCDecl] -> Name -> Name -> [TCDecl] -> Error
illegalModuleInstanceCycleErrorMessage decls mName iName path = 
    let 
        findWithDependency :: [TCDecl] -> Name -> Maybe (TCDecl, Name)
        findWithDependency decls n =
            case filter (\ d -> n `elem` freeVars d) decls of
                [] -> Nothing
                (x:_) -> Just (x, n)

        findInstanceCyclePath :: Maybe Name -> [TCDecl] -> Doc
        findInstanceCyclePath (Just thisName) [_] =
            prettyPrint thisName <+> text "in" <+> prettyPrint iName
            $$ text "which is an instance of" <+> prettyPrint mName
        findInstanceCyclePath thisName (from:to:rest) =
            case unAnnotate from of
                Module mn _ ds1 ds2 -> 
                    let (d, n) = head $ catMaybes $ map (findWithDependency (ds1++ds2))
                                    $ boundNames to
                    in case thisName of
                        Nothing -> 
                            text "Module" <+> prettyPrint mn <+> text "contains"
                                <+> prettyPrint (head (boundNames d))
                                <+> text "which calls"
                            $$ findInstanceCyclePath (Just n) (to:rest)
                        Just thisName -> 
                            prettyPrint thisName <> text ", which is defined in"
                                <+> text "module" <+> prettyPrint mn
                            $$ text "and also contains"
                                <+> prettyPrint (head (boundNames d))
                                <+> text "which calls"
                            $$ findInstanceCyclePath (Just n) (to:rest)
                _ -> 
                    let newName = head (intersect (freeVars from) (boundNames to))
                    in prettyPrint (fromJust thisName) <+> text "which calls"
                        $$ findInstanceCyclePath (Just newName) (to:rest)

    in fsep [text "The module" <+> prettyPrint mName,
        text "uses a definition in an instance" <+> prettyPrint iName,
        text "of itself, which is not allowed."]
    $$ text "The path by which the module calls its instance is:"
    $$ tabIndent (findInstanceCyclePath Nothing path)

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
