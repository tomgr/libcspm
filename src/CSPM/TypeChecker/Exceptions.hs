{-# LANGUAGE DeriveDataTypeable #-}
module CSPM.TypeChecker.Exceptions (
    Error, Warning,
    infiniteUnificationMessage,
    unificationErrorMessage,
    incorrectArgumentCountMessage,
    incorrectModuleArgumentCountMessage,
    constraintUnificationErrorMessage,
    deprecatedNameUsed,
    unsafeNameUsed,
    illegalModuleInstanceCycleErrorMessage,
    ambiguousDataTypeClauseError,
    ambiguousChannelError,
    ErrorOptions(..), defaultErrorOptions,
)
where

import Prelude
import Data.Maybe

import Data.List (intersect, nub, sort)
import CSPM.DataStructures.FreeVars
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.PrettyPrinter
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

type Error = VariableRepresentationMap -> (Doc, VariableRepresentationMap)
type Warning = Doc

noMap result map = (result, map)

incorrectArgumentCountMessage :: Doc -> Int -> Int -> Error
incorrectArgumentCountMessage func expected actual = noMap $
    hang (hang (text "The function") tabWidth func) tabWidth
        (text "was supplied with" <+> int actual <+> 
        text "arguments, but was expecting" <+> int expected)

incorrectModuleArgumentCountMessage :: Doc -> Int -> Int -> Error
incorrectModuleArgumentCountMessage moduleName expected actual = noMap $
    hang (hang (text "The module") tabWidth moduleName) tabWidth
        (text "was supplied with" <+> int actual <+> 
        text "arguments, but was expecting" <+> int expected)
infiniteUnificationMessage :: Type -> Type -> Error
infiniteUnificationMessage t1 t2 vmap = 
    let ([ppt1, ppt2], map') = prettyPrintTypesWithMap vmap [t1, t2] in
    (text "Cannot construct the infinite type:" <+> ppt1 <+> equals <+> ppt2,
        map')

unificationErrorMessage :: Bool -> [(Type, Type)] -> Error
unificationErrorMessage _ [] _ = panic "Empty unification stack during error"
unificationErrorMessage useWhilst unificationStack vmap = 
    let 
        stackToPrint = 
            if length unificationStack <= 4 then
                unificationStack
            else
                take 3 unificationStack ++ [last unificationStack]

        allTypes = concatMap (\ (t1, t2) -> [t1,t2]) stackToPrint
        (ppTypes', vmap') = prettyPrintTypesWithMap vmap allTypes
        intoTuples [] = []
        intoTuples (x:y:xs) = (x, y):intoTuples xs
        ppTypes = intoTuples ppTypes'

        (pit1, pit2) = head ppTypes

        ppExpected (t1, t2) = 
            sep [text "whilst matching expected type" <+> t1,
                nest 8 (text "with actual type" <+> t2)]

        ts = concatMap (\ (t1, t2) -> [t1,t2]) stackToPrint
    in
        (sep [if useWhilst then 
                text "whilst matching expected type" <+> pit1
            else text "Couldn't match expected type" <+> pit1,
            nest 8 (text "with actual type" <+> pit2)]
        $$ vcat (map ppExpected (tail ppTypes))
        $$ tabIndent (printOrigins ts),
        vmap')

constraintUnificationErrorMessage :: [(Constraint, Type)] -> [(Type, Type)] -> Error
constraintUnificationErrorMessage [] _ _ = panic "Empty unification stack during error"
constraintUnificationErrorMessage constraintStack unificationStack vmap = 
    let 
        hd = head constraintStack
        lt = last constraintStack
        (tc, t) = hd
        (itc, it) = lt
        ts = [it, t]
        ([pt1, pt2], vmap') = prettyPrintTypesWithMap vmap ts

        (uniError, vmap'') = unificationErrorMessage True unificationStack vmap'
    in
        (sep [text "The type" <+> pt2,
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
        $$ uniError,
        vmap'')

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

deprecatedNameUsed :: Name -> Maybe Name -> Doc
deprecatedNameUsed n Nothing = 
    prettyPrint n <+> text "is deprecated."
deprecatedNameUsed n (Just replacement) =
    prettyPrint n <+> text "is deprecated - use" <+>
    prettyPrint replacement <+> text "instead."

unsafeNameUsed :: Name -> Doc
unsafeNameUsed n =
    text "The invocation of" <+> prettyPrint n 
        <+> text "has not been type-checked."
    $$ text "Therefore, a runtime type error may occur."

illegalModuleInstanceCycleErrorMessage :: [TCDecl] -> Name -> Name -> [TCDecl] -> Error
illegalModuleInstanceCycleErrorMessage decls mName iName path = noMap $
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

ambiguousDataTypeClauseError :: Name -> TypeScheme -> Error
ambiguousDataTypeClauseError clauseName clauseType = 
    ambiguousTypeError (text "The data type clause" <+> prettyPrint clauseName)
        (prettyPrint clauseName) clauseType

ambiguousChannelError :: Name -> TypeScheme -> Error
ambiguousChannelError channelName channelType =
    ambiguousTypeError (text "The channel" <+> prettyPrint channelName)
        (prettyPrint channelName) channelType

ambiguousTypeError :: Doc -> Doc -> TypeScheme -> Error
ambiguousTypeError headerDoc nameDoc clauseType vmap = 
    let ([d], vmap') = prettyPrintTypeSchemesWithMap vmap [clauseType]

        extractFields (TDotable tl tr) = tl : extractFields tr
        extractFields t = [t]

        ForAll xs typ = clauseType
        fields = init $ extractFields typ

        polymorphicFields = filter (isPolymorphic . fst) (zip fields [1..])

        (prettyTypes, vmap'') =
            prettyPrintTypesWithMap vmap' (map fst polymorphicFields) 

        polymorphicFields' = zip prettyTypes (map snd polymorphicFields)

    in (headerDoc <+> text "has a polymorphic type:"
        $$ tabIndent d
        $$ text "To fix this, constrain the type of the following polymorphic fields of"
            <+> nameDoc <> colon
        $$ tabIndent (vcat (map (\ (t, n) -> 
                text "Field" <+> int n <> colon <+> t
            ) polymorphicFields')),
        vmap'')


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
