module CSPM.HaskellEvaluator.TranslateDataType (
    registerDataTypes,
    translateDataType,
    translateDotApplication,
    translateDotPattern,
)
where

import Control.Applicative (Applicative)
import Control.Monad
import Control.Monad.Trans
import Data.Foldable (foldrM)
import Data.List (groupBy, nub, sort, sortBy)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.Prelude
import CSPM.HaskellEvaluator.Monad
import CSPM.HaskellEvaluator.Translate
import CSPM.HaskellEvaluator.TranslateName
import Util.Annotated
import Util.Exception
import Util.Monad (concatMapM)
import Util.MonadicPrettyPrint
import qualified Util.PrettyPrint as PP

classInstance :: (Applicative m, Monad m) => String -> [m Doc] -> [m Doc] -> [m Doc] -> m Doc
classInstance className args context methods =
    text "instance"
        <+> (if null context then empty
                else parens (list $ sequence context) <+> text "=>")
        <+> text className <+> hsep (mapM parens args) <+> text "where"
    $$ tabIndent (vcat $ sequence methods)

computeYieldTypes :: [DataTypeInformation] -> [(Type, Type)]
computeYieldTypes = concatMap yields
    where
        yields :: DataTypeInformation -> [(Type, Type)]
        yields dataType = 
            map (\ (_, prefix) ->
                (foldr TDotable (dataTypeType dataType) (prefixRemainingTypes prefix),
                TDatatype (prefixTypeName prefix)))
            (M.toList (dataTypePrefixes dataType))

computePrefixes :: Name -> [DataTypeConstructor] -> TranslationMonad [DataTypePrefix]
computePrefixes dataTypeName clauses =
    let
        -- Step 1: for each clause, create a copy of it for every suffix of
        -- constructorFieldTypes.
        copyClause :: DataTypeConstructor -> [ (Name, [Type]) ]
        copyClause clause =
            map (\ ts -> (constructorName clause, ts)) $
            map (\ i -> drop i (constructorFieldTypes clause))
                [0 .. constructorFieldCount clause]

        duplicatedClauses :: [ (Name, [Type]) ]
        duplicatedClauses = concatMap copyClause clauses

        -- Step 2: group the (copied) clauses by their suffix of
        -- constructorFieldTypes.
        prefixGroups :: [[ (Name, [Type]) ]]
        prefixGroups =
            groupBy (\ x y -> snd x == snd y) $
            sortBy (\ x y -> compare (snd x) (snd y)) duplicatedClauses
    in
        -- Step 3: construct the prefixes by mapping over the groups.
        mapM (\ group -> do
            let prefixRemainingTypes = snd (head group)
            prefixTypeName <-
                case prefixRemainingTypes of
                    [] -> return dataTypeName
                    _ -> mkFreshInternalName
            return $! DataTypePrefix {
                prefixTypeName = prefixTypeName,
                prefixMatchingConstructors = map fst group,
                prefixRemainingFieldCount = length prefixRemainingTypes,
                prefixRemainingTypes = prefixRemainingTypes
            }) prefixGroups

computeDataTypeInformation :: TCDecl -> TranslationMonad [DataTypeInformation]
computeDataTypeInformation (An _ _ (DataType dataTypeName clauses)) = do
    let
        computeDataTypeClauseInformation (An _ _ (DataTypeClause constructorName constructorTypeExpression)) =
            let 
                dataTypeFieldTypes (TDot t1 t2) =
                    concatMap dataTypeFieldTypes [t1, t2]
                dataTypeFieldTypes (TSet (TDot t1 t2)) =
                    concatMap dataTypeFieldTypes [TSet t1, TSet t2]
                dataTypeFieldTypes x = [fieldType x]
                fieldType (TTuple ts) = TTuple $ map fieldType ts
                fieldType (TSet t) = t
                constructorFieldTypes =
                    case constructorTypeExpression of
                        Just e -> dataTypeFieldTypes (getType e)
                        Nothing -> []
                constructorDecomposedFieldSets =
                    case constructorTypeExpression of
                        Nothing -> []
                        Just e -> decomposeDataTypeTypeExpression e
                constructorFieldSetIsTrivial =
                    map (\ field ->
                        case field of
                            CompoundFieldSet _ _ -> False
                            SimpleFieldSet exp -> fieldExpressionIsTrivial exp
                        ) constructorDecomposedFieldSets
            in DataTypeConstructor {
                constructorName = constructorName,
                constructorFieldCount = length constructorFieldTypes,
                constructorFieldTypes = constructorFieldTypes,
                constructorDecomposedFieldSets = constructorDecomposedFieldSets,
                constructorFieldSetIsTrivial = constructorFieldSetIsTrivial 
            }

        dataTypeConstructors = map computeDataTypeClauseInformation clauses

    prefixes <- computePrefixes dataTypeName dataTypeConstructors

    return [DataTypeInformation {
        dataTypeName = dataTypeName,
        dataTypeType =
            if dataTypeName == builtInName "Events" then TEvent
            else TDatatype dataTypeName,
        dataTypeConstructors = M.fromList
            [(constructorName c, c) | c <- dataTypeConstructors],
        dataTypePrefixes = M.fromList [(prefixRemainingTypes p, p) | p <- prefixes]
    }]
computeDataTypeInformation _ = return []

-- | Converts channel declarations into a datatype declaration called Events.
-- (They're exactly the same.)
computeEventsDataType :: [TCDecl] -> TranslationMonad [DataTypeInformation]
computeEventsDataType declarations = do
    let
        clauses =
            An Unknown (panic "Dummy annotation")
                (DataTypeClause (builtInName "tau") Nothing)
            : An Unknown (panic "Dummy annotation")
                (DataTypeClause (builtInName "tick") Nothing)
            : concatMap (\ decl ->
                case decl of
                    An _ _ (Channel ns decl) ->
                        [An Unknown (panic "Dummy annotation") (DataTypeClause n decl)
                        | n <- ns]
                    _ -> []
            ) declarations
        dataType = An Unknown (panic "Dummy annotation") $
                    DataType (builtInName "Events") clauses
    
    computeDataTypeInformation dataType

registerDataTypes :: [TCDecl] -> TranslationMonad ()
registerDataTypes declarations = do
    datatypes <- concatMapM computeDataTypeInformation declarations
    eventsDataType <- computeEventsDataType declarations
    let allDatatypes = eventsDataType ++ datatypes
    addDataTypes allDatatypes

isCompoundFieldSet (CompoundFieldSet _ _) = True
isCompoundFieldSet _ = False

complexFieldSets :: DataTypeConstructor -> [TCExp]
complexFieldSets clause = nub $ sort $ map compoundFieldSetExpression $
    filter isCompoundFieldSet $ constructorDecomposedFieldSets clause

translateDataType :: Name -> TranslationMonad Doc
translateDataType n = dataTypeForName n >>= \ dataType ->
    let clauses = map snd $ M.toList $ dataTypeConstructors dataType in
    -- Set definition
    -- Set of all values definition.

    -- Prefix definitions (also defines the main datatype)
    vcat (mapM (translateDataTypePrefix dataType)
        (map snd $ M.toList $ dataTypePrefixes dataType))

    -- Field Set definitions
    $$ vcat (mapM (translateDataTypeClauseFieldSet dataType) clauses)

    -- Set of all things of this type
    $$ translateName n <+> text "::" <+> translateType (TSet (dataTypeType dataType))
    $$ translateName n <+> equals <+> text "CSPM_"
            <> translateDataTypeName (dataTypeName dataType) <> text "SetUnion"
        <+> brackets (list (mapM (\ clause ->
            case constructorDecomposedFieldSets clause of
                [] -> text "cspm_set_fromList" <+> brackets (
                        translateDataTypeClauseName 0 (constructorName clause)
                    )
                fields ->
                    text "CSPM_" <> translateDataTypeName (dataTypeName dataType)
                    <> char '_' <> translateDataTypeName (constructorName clause)
                    <> text "_ProductSet"
                    <+> hsep (zipWithM (\i _ -> translateFieldSetName clause i) [0..] fields)
        ) (filter (\ c -> not (constructorName c `elem` [builtInName "tau", builtInName "tick"]))
            clauses)))

    -- CSPM_Dot instance for the main datatype
    $$ classInstance "CSPM_Dot" [translateDataTypeName n, text "a",
            text "CSPM_ExplicitDot" <+> translateDataTypeName n <+> char 'a'] []
        [text "cspm_dotOn _ x y = CSPM_ExplicitDot x y"]

    -- Default constructor definitions
    $$ vcat (mapM (\ clause ->
        translateName (constructorName clause) <+> text "::"
            <+> translateType (foldr TDotable (dataTypeType dataType)
                    (constructorFieldTypes clause))
        $$ translateName (constructorName clause) <+> equals
        <+> (if constructorFieldCount clause > 0 then text "CSPM_Yield" else empty)
        <+> translateDataTypeClauseName (constructorFieldCount clause)
                (constructorName clause)
        ) clauses)

translateDataTypeClauseFieldSet :: DataTypeInformation -> DataTypeConstructor ->
    TranslationMonad Doc
translateDataTypeClauseFieldSet dataType clause = do
    fieldSetNames <- mapM (\ fieldExp -> do
            nameToBind <- liftIO $ mkFreshInternalName
            return (fieldExp, nameToBind)
        ) (complexFieldSets clause)
    let fieldSetMap = M.fromList fieldSetNames

    vcat (zipWithM (translateFieldSet fieldSetMap clause) [0..]
            (constructorDecomposedFieldSets clause))
        $$ vcat (mapM (\ (exp, n) -> do
            let 
                convertType (TSet t) = TTuple $ map TSet $ convertType' t
                convertType' (TDot t1 t2) = t1 : convertType' t2
                convertType' t = [t]
                arguments = map (\i -> char 'a' <> int i) [0..constructorFieldCount clause-1]
            
            translateName n <+> text "::" <+> translateType (convertType (getType exp))
                $$ translateName n <+> equals
                <+> text "if set == reconstructedSet then"
                <+> parens (list (mapM (\i -> char 's' <> int i) [0..constructorFieldCount clause-1]))
                <+> text "else" <+> translateError (loc exp) (
                        text "hang (text \"The set:\") 4 (cspm_show set)"
                        <+> text "$$ text \"cannot be decomposed into a cartesian product (i.e. it is not rectangular).\""
                        <+> text "$$ hang (text \"The cartesian product is equal to:\") 4"
                            <+> text "(cspm_show (cspm_set_fromList (cspm_set_toList reconstructedSet)))"
                        <+> text "$$ hang (text \"and thus the following values are missing:\") 4"
                            <+> text "(cspm_show (cspm_set_diff reconstructedSet set))"
                    )
                $$ tabIndent (text "where"
                $$ tabIndent (
                    text "tuple_list" <+> equals <+> text "map (\\ "
                        <+> foldr1 (\ a b -> parens (text "CSPM_ExplicitDot" <+> a <+> b)) arguments
                        <+> text "->"
                        <+> parens (list (sequence arguments))
                        <+> text ") $ cspm_set_toList set"
                    $$ text "set" <+> equals <+> translateTypeExpression exp
                    $$ text "reconstructedSet" <+> equals <+>
                        foldr1 (\ i d -> parens (text "CSPM_ExplicitDotProduct" <+> i <+> d))
                            (map (\i -> char 's' <> int i) [0..constructorFieldCount clause-1])
                    $$ vcat (
                            mapM (\ i ->
                                char 's' <> int i <+> equals
                                <+> text "cspm_set_fromList $ map (\\"
                                <+> parens (list (mapM (\i -> char 'a' <> int i)
                                        [0..constructorFieldCount clause-1]))
                                <+> text "->" <+> char 'a' <> int i
                                <> text ") tuple_list"
                                )
                                [0..constructorFieldCount clause-1]
                        )
                    )
                )
            ) fieldSetNames)

translateFieldSetName :: DataTypeConstructor -> Int -> TranslationMonad Doc
translateFieldSetName clause fieldNumber =
    translateName (constructorName clause) <> text "_field_" <> int fieldNumber

translateFieldIsValidName :: DataTypeConstructor -> Int -> TranslationMonad Doc
translateFieldIsValidName clause fieldNumber =
    translateName (constructorName clause) <> text "_isValid_" <> int fieldNumber

translateFieldSet :: M.Map TCExp Name -> DataTypeConstructor -> Int ->
    FieldSet -> TranslationMonad Doc
translateFieldSet complexExpressionMap clause fieldNumber fieldSet =
    let 
        fieldType = constructorFieldTypes clause !! fieldNumber
        fieldIsTrivial = constructorFieldSetIsTrivial clause !! fieldNumber
    in
    translateFieldSetName clause fieldNumber
        <+> text "::" <+> translateType (TSet fieldType)
    $$ translateFieldSetName clause fieldNumber <+> equals <+>
        case fieldSet of
            SimpleFieldSet e -> translateTypeExpression e
            CompoundFieldSet i e ->
                text "let" <+> parens (list (mapM (\i -> char 'a' <> int i) [0..constructorFieldCount clause-1]))
                <+> equals <+> translateName (
                        M.findWithDefault (panic "complex not found") e
                            complexExpressionMap
                    )
                <+> text "in a" <> int i
    $$ translateFieldIsValidName clause fieldNumber
        <+> text "::" <+> translateType fieldType <+> text "-> Bool"
    $$ translateFieldIsValidName clause fieldNumber <+> char 'x' <+> equals
        <+> if fieldIsTrivial then text "True"
            else text "cspm_set_member x" <+> translateFieldSetName clause fieldNumber

clauseForName :: DataTypeInformation -> Name -> DataTypeConstructor
clauseForName dataType clauseName =
    case M.lookup clauseName (dataTypeConstructors dataType) of
        Just x -> x
        Nothing -> panic "Clause not found"

nextPrefix :: DataTypeInformation -> DataTypePrefix -> DataTypePrefix
nextPrefix dataType prefix =
    case M.lookup (tail (prefixRemainingTypes prefix)) (dataTypePrefixes dataType) of
        Just prefix -> prefix
        Nothing -> panic "Could not find remaining tail"

translateDataTypePrefix :: DataTypeInformation -> DataTypePrefix -> TranslationMonad Doc
translateDataTypePrefix dataType prefix =
    let matchingConstructors = map (clauseForName dataType) (prefixMatchingConstructors prefix)
        nextType = head (prefixRemainingTypes prefix)
        remainingDotableType = foldr TDotable (dataTypeType dataType) (tail (prefixRemainingTypes prefix))
        dotableType = foldr TDotable (dataTypeType dataType) (prefixRemainingTypes prefix)
    in 
    text "data" <+> translateDataTypeName (prefixTypeName prefix) <+> equals
        <+> hsep (punctuate (text " |")
                (mapM (translateDataTypeClause (prefixRemainingFieldCount prefix))
                    matchingConstructors)
                )
        <+> text "deriving (Eq, Ord, Typeable)"

    -- Deriving Eq and Ord is fine because, in particular, functions cannot be
    -- in datatypes. Further, all types we define are guaranteed to have such
    -- instances.

    -- Show instanace
    $$ classInstance "CSPM_Show" [translateDataTypeName (prefixTypeName prefix)] []
        (map (makeDataTypeClauseShowDefinition (prefixRemainingFieldCount prefix))
            matchingConstructors)

    -- Set instance
    $$ generateDatatypeSetInstances dataType prefix

    -- Dot instance
    $$ (if prefixRemainingFieldCount prefix == 0 then empty else
        classInstance "CSPM_Dot" [
                translateDataTypeName (prefixTypeName prefix),
                parens (translateType nextType),
                translateType remainingDotableType
            ]
            []
            (map (\ clause ->
                let usedFieldCount = constructorFieldCount clause - prefixRemainingFieldCount prefix
                    fieldIsTrivial = constructorFieldSetIsTrivial clause !! usedFieldCount
                    newDatatype = (if prefixRemainingFieldCount prefix > 1 then text "CSPM_Yield $" else empty)
                        <+> translateDataTypeClauseName (prefixRemainingFieldCount prefix-1) (constructorName clause)
                        <+> hsep (mapM (\ i -> char 'a' <> int i) [1..usedFieldCount])
                        <+> char 'b'
                in text "cspm_dotOn loc" <+> text "a@" <> parens (
                    translateDataTypeClauseName (prefixRemainingFieldCount prefix) (constructorName clause)
                    <+> hsep (mapM (\ i -> char 'a' <> int i)
                        [1..usedFieldCount])
                ) <+> char 'b' <+> equals
                <+> if fieldIsTrivial then newDatatype else
                text "if" <+> translateFieldIsValidName clause usedFieldCount <+> char 'b' <+> text "then"
                <+> newDatatype
                <+> text "else" <+> translateError' "loc" (
                        text "hang (text \"The value:\") 4 (cspm_show b)"
                        <+> text "$$ text \"is not allowed on the right of\" <+> cspm_show a"
                        <+> text "$$ hang (text \"because it is not a member of the set:\") 4 (cspm_show $"
                            <+> translateFieldSetName clause usedFieldCount <+> text ")"
                    )
            ) matchingConstructors)
        )

    -- CSPM_Productions instance
    $$ classInstance "CSPM_Productions" [
            translateDataTypeName (prefixTypeName prefix),
            translateDataTypeName (dataTypeName dataType)
        ]
        []
        (if prefixRemainingFieldCount prefix == 0 then
            [text "cspm_productions x = cspm_set_fromList [x]"]
        else
            map (\ clause ->
                let usedFieldCount = constructorFieldCount clause - prefixRemainingFieldCount prefix
                in text "cspm_productions" <+> parens (
                    translateDataTypeClauseName (prefixRemainingFieldCount prefix) (constructorName clause)
                    <+> hsep (mapM (\ i -> char 'a' <> int i) [1..usedFieldCount])
                )
                <+> equals
-- TODO: factor out
                <+> text "CSPM_" <> translateDataTypeName (dataTypeName dataType)
                    <> char '_' <> translateDataTypeName (constructorName clause)
                    <> text "_ProductSet"
                <+> hsep (mapM (\i -> parens $
                            if i <= usedFieldCount then
                                text "cspm_set_fromList" <+> brackets (char 'a' <> int i)
                            else translateFieldSetName clause (i-1))
                        [1..constructorFieldCount clause])
            ) matchingConstructors
        )

    $$ (if prefixRemainingFieldCount prefix == 0 then empty else
        classInstance "CSPM_RestrictedProductions" [
                translateDataTypeName (prefixTypeName prefix),
                translateType nextType,
                translateDataTypeName (dataTypeName dataType)
            ]
            []
            (map (\ clause ->
                let usedFieldCount = constructorFieldCount clause - prefixRemainingFieldCount prefix
                in text "cspm_restricted_productions" <+> parens (
                    translateDataTypeClauseName (prefixRemainingFieldCount prefix) (constructorName clause)
                    <+> hsep (mapM (\ i -> char 'a' <> int i) [1..usedFieldCount])
                )
                <+> text "b"
                <+> equals
                <+> text "CSPM_" <> translateDataTypeName (dataTypeName dataType)
                    <> char '_' <> translateDataTypeName (constructorName clause)
                    <> text "_ProductSet"
                <+> hsep (mapM (\i -> parens $
                            if i <= usedFieldCount then
                                text "cspm_set_fromList" <+> brackets (char 'a' <> int i)
                            else if i == usedFieldCount+1 then
                                if constructorFieldSetIsTrivial clause !! (i-1) then
                                    text "b"
                                else
                                    text "cspm_set_inter b" <+> translateFieldSetName clause (i-1)
                            else translateFieldSetName clause (i-1))
                        [1..constructorFieldCount clause])
            ) matchingConstructors)
    
        $$ classInstance "CSPM_DropPrefix" [
                translateDataTypeName (prefixTypeName prefix),
                translateDataTypeName (dataTypeName dataType),
                translateType (foldr1 TDot (prefixRemainingTypes prefix))
            ]
            []
            (map (\ clause ->
                let usedFieldCount = constructorFieldCount clause - prefixRemainingFieldCount prefix
                in text "cspm_dropPrefix" <+> parens (
                    translateDataTypeClauseName (prefixRemainingFieldCount prefix) (constructorName clause)
                    <+> hsep (mapM (\ i -> char 'a' <> int i) [1..usedFieldCount])
                )
                <+> text "s"
                <+> equals
                <+> text "case s of"
                $$ tabIndent (
                        text "CSPM_" <> translateDataTypeName (dataTypeName dataType)
                        <> char '_' <> translateDataTypeName (constructorName clause)
                        <> text "_ProductSet"
                        <+> hsep (mapM (\i -> parens $
                            if i <= usedFieldCount then char '_'
                            else char 'a' <> int i) [1..constructorFieldCount clause])
                        <+> text "->"
                        <+> foldr1 (\ x y -> text "CSPM_ExplicitDotProduct" <+> x <+> parens y)
                                (map (\i -> char 'a' <> int i) [usedFieldCount+1..constructorFieldCount clause])
                    $$ text "_ -> cspm_set_fromList $ map (\\ "
                        <+> parens (
                                translateDataTypeClauseName 0 (constructorName clause)
                                <+> hsep (mapM (\i -> char 'a' <> int i) [1..constructorFieldCount clause])
                            )
                        <+> text "->"
                        <+> foldr1 (\ x y -> text "CSPM_ExplicitDot" <+> x <+> parens y)
                                (map (\i -> char 'a' <> int i) [usedFieldCount+1..constructorFieldCount clause])
                        <+> text ") $ cspm_set_toList s"
                    )
            ) matchingConstructors)
        )

dataTypeTypeName :: Type -> Name
dataTypeTypeName (TDatatype n) = n
dataTypeTypeName TEvent = builtInName "Events"
dataTypeTypeName t = panic $ show t ++ " is not a datatype type name."

translateDataTypeClause :: Int -> DataTypeConstructor -> TranslationMonad Doc
translateDataTypeClause remainingFields clause =
    translateDataTypeClauseName remainingFields (constructorName clause)
    <+> hsep (mapM translateType (clauseAlreadyFoundFields remainingFields clause))

clauseAlreadyFoundFieldCount :: DataTypePrefix -> DataTypeConstructor -> Int
clauseAlreadyFoundFieldCount prefix clause =
    constructorFieldCount clause - prefixRemainingFieldCount prefix

clauseAlreadyFoundFields :: Int -> DataTypeConstructor -> [Type]
clauseAlreadyFoundFields remainingFields clause =
    take (constructorFieldCount clause - remainingFields) (constructorFieldTypes clause)

makeDataTypeClauseShowDefinition :: Int -> DataTypeConstructor -> TranslationMonad Doc
makeDataTypeClauseShowDefinition remainingFields clause =
    text "cspm_show"
    <+> parens (translateDataTypeClauseName remainingFields (constructorName clause) <+>
            hsep (mapM (\i -> char 'f' <> int i)
                [1..constructorFieldCount clause - remainingFields]))
    <+> equals
    <+> if constructorName clause == builtInName "tau" then
            text "char 'τ'"
        else if constructorName clause == builtInName "tick" then
            text "char '✓'"
        else
            text "cspm_prettyPrint_dotSep" <+> brackets (list (sequence $
            text "text" <+> doubleQuotes (return $ PP.prettyPrint (constructorName clause))
            : map (\ i -> text "cspm_show" <+> char 'f' <> int i)
                    [1..constructorFieldCount clause - remainingFields]
            ))

decomposeDataTypeTypeExpression :: TCExp -> [FieldSet]
decomposeDataTypeTypeExpression (An _ _ (DotApp e1 e2)) =
    concatMap decomposeDataTypeTypeExpression [e1, e2]
decomposeDataTypeTypeExpression exp@(An _ (Just (TSet typ@(TDot _ _)), _) x) =
        [CompoundFieldSet i exp | i <- [0..fieldCount typ-1]]
    where
        fieldCount (TDot t1 t2) = 1+fieldCount t2
        fieldCount _ = 1
decomposeDataTypeTypeExpression x = [SimpleFieldSet x]

-- | Determine if the expression, which is a type expression, is actually
-- trivial in that it can never fail on a particular value. This is the case
-- when the expression evaluates to the set of all values of a given type. This
-- function approximates this in a conservative fashion.
fieldExpressionIsTrivial :: TCExp -> Bool
fieldExpressionIsTrivial (An _ _ (Var n)) | n `elem` builtinCompleteDataTypeSets = True
fieldExpressionIsTrivial (An _ (Just (TSet (TDatatype _)), _) (Var _)) = True
fieldExpressionIsTrivial (An _ (Just (TSet TEvent), _) (Var _)) = True
fieldExpressionIsTrivial (An _ _ (App (An _ _ (Var f)) [arg]))
    | f `elem` builtinCompleteConstructors = fieldExpressionIsTrivial arg
fieldExpressionIsTrivial (An _ _ (Tuple es)) =
    and (map fieldExpressionIsTrivial es)
fieldExpressionIsTrivial _ = False

-- | The set of prelude names that include all values of their type.
builtinCompleteDataTypeSets :: [Name]
builtinCompleteDataTypeSets = map builtInName [
        "Bool",
        "Char",
        "Events",
        "Int",
        "Proc"
    ]
-- | The set of prelude constructors that construct all values of their type,
-- assuming their argument does.
builtinCompleteConstructors :: [Name]
builtinCompleteConstructors = map builtInName [
        "Seq",
        "Set"
    ]

-- | Translates an expression of the form a.b, attempting to optimise it to
-- remove bounds checking if possible. This is done precisely in the case when
-- the datatype is fully constructed, and there is one field corresponding to
-- each compiled field. This is conservative, but covers a large percentage of
-- the likely to occur optimisable cases.
translateDotApplication :: TCExp -> TranslationMonad Doc
translateDotApplication (exp@(An loc _ (DotApp left right))) =
    let
        leftMostDot (An _ _ (DotApp l r)) = (leftMost, args ++ [r])
            where (leftMost, args) = leftMostDot l
        leftMostDot x = (x, [])

        (leftMostConstructor, arguments) = leftMostDot exp

        findFields :: [Type] -> [TCExp] -> Maybe [TCExp]
        findFields [] _ = panic "Empty type list for find fields"
        findFields [t] [exp] | getType exp == t = Just [exp]
        findFields (t:ts) (exp:exps) | t == getType exp =
            case findFields ts exps of
                Just fs -> Just $ exp : fs
                Nothing -> Nothing
        findFields (t:ts) (An _ typ (DotApp l r) : exps) =
            -- We know t != typ, so we try splitting
            findFields (t:ts) (l:r:exps)
        findFields  _ _ = Nothing

        fallback =
            text "cspm_dotOn"
            <+> translateLocation loc
            <+> translateExpression left
            <+> translateExpression right
    in if isDataTypeOrEvent (getType exp) then
            case leftMostConstructor of
                An _ _ (Var n) -> do
                    dataType <- dataTypeForName (dataTypeTypeName (getType exp))
                    case M.lookup n (dataTypeConstructors dataType) of
                        Just clause | constructorFieldCount clause > 0
                                && and (constructorFieldSetIsTrivial clause) ->
                            case findFields (constructorFieldTypes clause) arguments of
                                Just fs ->
                                    translateDataTypeClauseName 0 (constructorName clause)
                                    <+> hsep (mapM translateExpression fs)
                                _ -> fallback
                        _ -> fallback
                _ -> fallback
        else fallback

ultimateYieldType (TDotable t1 t2) = ultimateYieldType t2
ultimateYieldType t = t

data CompiledDotPattern =
    DataTypeMatch Name [CompiledDotPattern]
    | ExplicitDotMatch CompiledDotPattern CompiledDotPattern
    | TypedField Type (Maybe TCPat)

translateDotPattern :: TCPat -> TranslationMonad (TranslationMonad Doc, [PatternSideCondition])
translateDotPattern pat =
    let
        isPDotApp (An _ _ (PDotApp _ _)) = True
        isPDotApp _ = False

        splitField :: TCPat -> (TCPat, Maybe TCPat)
        splitField (An _ _ (PDotApp l r)) = (l, Just r)
        splitField p = (p, Nothing)

        findFields :: [Type] -> TCPat ->
            TranslationMonad ([TranslationMonad Doc], [PatternSideCondition])
        findFields [] _ = panic "findFields-1: ran out of types"
        findFields (TDot l r : ts) p = do
            (fL:fR:fs, scs) <- findFields (l:r:ts) p
            return (parens (text "CSPM_ExplicitDot" <+> fL <+> fR):fs, scs)
        findFields (t : ts) p = do
            let (first, rest) = splitField p
            case first of
                p@(An _ _ (PVar clauseName)) | nameIsConstructor clauseName -> do
                    dataType <- dataTypeForName $ dataTypeTypeName $ ultimateYieldType $ getType p
                    let Just clause = M.lookup clauseName (dataTypeConstructors dataType)
                        clauseTypes = constructorFieldTypes clause
                        newTypes = clauseTypes ++ ts
                    case rest of
                        Nothing | null newTypes ->
                            return ([translateDataTypeClauseName 0 clauseName], [])
                        Nothing -> panic "findFields: last pattern but remaining fields (1)"
                        Just rest -> do
                            (fs, scs) <- findFields newTypes rest
                            when (length fs < constructorFieldCount clause) $
                                panic "Insufficient fields generated"
                            let fsForDataType = take (constructorFieldCount clause) fs
                                remainingFs = drop (constructorFieldCount clause) fs
                            return (parens (translateDataTypeClauseName 0 clauseName
                                            <+> hsep (sequence fsForDataType))
                                : remainingFs, scs)
                p ->
                    case rest of
                        Just rest | getType p == t -> do
                            (f, scs1) <- translatePattern p
                            (fs, scs2) <- findFields ts rest
                            return (f : fs, scs1++scs2)
                        Just _ -> panic "findFields: pattern has wrong type"
                        Nothing | null ts -> do
                            (f, scs) <- translatePattern p
                            return ([f], scs)
                        Nothing | getType p == foldr1 TDot (t:ts) -> do
                            vars <- liftIO $ replicateM (1+length ts) mkFreshInternalName
                            (pF, pSC) <- translatePattern p
                            let formattedVars = map translateName vars
                                sc = MatchSideCondition pF $ foldr1
                                        (\ x y -> parens $ text "CSPM_ExplicitDot" <+> x <+> y)
                                        formattedVars
                            return (formattedVars, sc : pSC)
                        Nothing -> panic $! "findFields: wrong type with nothing left: "
                            ++show pat ++ "\n"
                            ++show (PP.prettyPrint $ getType pat) ++ "\n"
                            ++show p ++ "\n"
                            ++show (PP.prettyPrint $ getType p) ++ "\n"
                            ++show (PP.prettyPrint $ foldr1 TDot (t:ts))
    in do
        ([f], scs) <- findFields [getType pat] pat
        return (f, scs)

generateDatatypeSetInstances :: DataTypeInformation -> DataTypePrefix -> TranslationMonad Doc
generateDatatypeSetInstances dataType prefix =
    let
        matchingConstructors =
            filter (\ clause -> clauseAlreadyFoundFieldCount prefix clause > 0) $
            map (clauseForName dataType) (prefixMatchingConstructors prefix)
        typeTuple = translateDataTypeName (prefixTypeName prefix)
        prefixExplicit = text "CSPM_Explicit" <> typeTuple <> text "Set"
        prefixExplicitArgumentA = parens $ prefixExplicit <+> char 'a'
        prefixExplicitArgumentB = parens $ prefixExplicit <+> char 'b'
        prefixSetUnion = text "CSPM_" <> typeTuple <> text "SetUnion"
        prefixSetUnionArgumentA = parens $ prefixSetUnion <+> char 'a'
        prefixSetUnionArgumentB = parens $ prefixSetUnion <+> char 'b'
        prefixConstructor :: DataTypeConstructor -> TranslationMonad Doc
        prefixConstructor constructor =
            text "CSPM_" <> typeTuple <> char '_'
            <> translateDataTypeName (constructorName constructor)
            <> text "_ProductSet"
        prefixConstructorArgumentA clause = parens $
            prefixConstructor clause <+> hsep (mapM (\ i -> char 'a' <> int i)
            [1..clauseAlreadyFoundFieldCount prefix clause])
        prefixConstructorArgumentB clause = parens $
            prefixConstructor clause <+> hsep (mapM (\ i -> char 'b' <> int i)
            [1..clauseAlreadyFoundFieldCount prefix clause])
        argument constructor = translateDataTypeClauseName
            (prefixRemainingFieldCount prefix) (constructorName constructor)
        argumentA clause = parens $
            argument clause <+> hsep (mapM (\ i -> char 'a' <> int i)
            [1..clauseAlreadyFoundFieldCount prefix clause])
        argumentB clause = parens $
            argument clause <+> hsep (mapM (\ i -> char 'b' <> int i)
            [1..clauseAlreadyFoundFieldCount prefix clause])
    in

    -- CSPM_Set
    text "instance" <+> text "CSPM_Set" <+> typeTuple <+> text "where"

    $$ tabIndent (
        text "data CSPM_SetType" <+> typeTuple <+> equals
        $$ tabIndent (vcat (punctuate (text " |") $ sequence $
            prefixExplicit <+> parens (text "S.Set" <+> typeTuple)
            : prefixSetUnion <+> brackets (text "CSPM_SetType" <+> typeTuple)
            : map ( \ constructor -> prefixConstructor constructor
                <+> hsep (mapM (translateType . TSet)
                        (clauseAlreadyFoundFields (prefixRemainingFieldCount prefix) constructor)
                    )
                ) matchingConstructors
            )
        )

        $$ text "cspm_set_card" <+> prefixExplicitArgumentA <+> equals <+> text "S.size a"
        $$ text "cspm_set_card" <+> prefixSetUnionArgumentA <+> equals <+> text "sum (map cspm_set_card a)"
        $$ vcat (mapM (\ clause -> 
            text "cspm_set_card" <+> prefixConstructorArgumentA clause <+> equals
            <+> hsep (punctuate (text " *") (
                    mapM (\ i -> text "cspm_set_card a" <> int i)
                        [1..clauseAlreadyFoundFieldCount prefix clause]
                ))
            ) matchingConstructors)

        $$ text "cspm_set_empty" <+> equals <+> prefixExplicit <+> text "S.empty"

        $$ text "cspm_set_diff" <+> prefixExplicitArgumentA <+> prefixExplicitArgumentB <+> equals
            <+> prefixExplicit <+> parens (text "S.difference a b")
        $$ text "cspm_set_diff" <+> prefixSetUnionArgumentA <+> char 'b' <+> equals
            <+> prefixSetUnion <+> text "$ map (\\ s1 -> cspm_set_diff s1 b) a"
        $$ text "cspm_set_diff a" <+> prefixSetUnionArgumentB <+> equals
            <+> text "foldl cspm_set_diff a b"
        $$ text "cspm_set_diff (a@(" <> prefixExplicit <+> text "{})) b" <+> equals
            <+> text "cspm_set_diff a (cspm_set_make_explicit b)"
        $$ text "cspm_set_diff a (b@(" <> prefixExplicit <+> text "{}))" <+> equals
            <+> text "cspm_set_diff (cspm_set_make_explicit a) b"
        $$ (if not (null matchingConstructors) then
            text "cspm_set_diff a b = cspm_set_diff (cspm_set_make_explicit a) (cspm_set_make_explicit b)"
            else empty)

        $$ text "cspm_set_fromList" <+> equals <+> prefixExplicit <+> char '.' <+> text "S.fromList"

        $$ text "cspm_set_inter" <+> prefixExplicitArgumentA <+> prefixExplicitArgumentB
            <+> equals <+> prefixExplicit <+> text "$ S.intersection a b"
        $$ text "cspm_set_inter" <+> prefixSetUnionArgumentA <+> text "b"
            <+> equals <+> prefixSetUnion <+> parens (text "map (cspm_set_inter b) a")
        $$ text "cspm_set_inter a" <+> prefixSetUnionArgumentB
            <+> equals <+> prefixSetUnion <+> parens (text "map (cspm_set_inter a) b")
        $$ text "cspm_set_inter (a@(" <> prefixExplicit <+> text "{})) b" <+> equals
            <+> text "cspm_set_inter a (cspm_set_make_explicit b)"
        $$ text "cspm_set_inter a (b@(" <> prefixExplicit <+> text "{}))" <+> equals
            <+> text "cspm_set_inter (cspm_set_make_explicit a) b"
        $$ (if not (null matchingConstructors) then
            text "cspm_set_inter a b = cspm_set_inter (cspm_set_make_explicit a) (cspm_set_make_explicit b)"
            else empty)

        $$ text "cspm_set_null" <+> prefixExplicitArgumentA <+> equals
            <+> text "S.null a"
        $$ text "cspm_set_null" <+> prefixSetUnionArgumentA <+> equals
            <+> text "and (map cspm_set_null a)"
        $$ vcat (mapM (\ clause -> 
            text "cspm_set_null" <+> prefixConstructorArgumentA clause <+> equals
            <+> hsep (punctuate (text " &&") (
                    mapM (\ i -> text "cspm_set_null a" <> int i)
                        [1..clauseAlreadyFoundFieldCount prefix clause]
                ))
            ) matchingConstructors)

        $$ text "cspm_set_member v" <+> prefixExplicitArgumentA <+> equals
            <+> text "S.member v a"
        $$ text "cspm_set_member v" <+> prefixSetUnionArgumentA <+> equals
            <+> text "or (map (cspm_set_member v) a)"
        $$ vcat (mapM (\ clause -> 
            text "cspm_set_member" <+> argumentA clause <+> prefixConstructorArgumentB clause <+> equals
            <+> hsep (punctuate (text " &&") (
                    mapM (\ i -> text "cspm_set_member a" <> int i<+>char 'b'<>int i)
                        [1..clauseAlreadyFoundFieldCount prefix clause]
                ))
            ) matchingConstructors)
        $$ (if length matchingConstructors > 1 then
            text "cspm_set_member _ _ = False"
            else empty)

        $$ text "cspm_set_toList" <+> prefixExplicitArgumentA <+> equals
            <+> text "S.toList a"
        $$ text "cspm_set_toList" <+> prefixSetUnionArgumentA <+> equals
            <+> text "concatMap cspm_set_toList a"
        $$ vcat (mapM (\ clause -> 
            text "cspm_set_toList" <+> prefixConstructorArgumentB clause <+> equals
            <+> brackets (
                    argumentA clause <+> char '|' <+> list (mapM (\ i ->
                        char 'a' <> int i <+> text "<- cspm_set_toList" <+> char 'b' <> int i
                    ) [1..clauseAlreadyFoundFieldCount prefix clause])
                )
            ) matchingConstructors)

        $$ text "cspm_set_union" <+> prefixExplicitArgumentA <+> prefixExplicitArgumentB
            <+> equals <+> prefixExplicit <+> text "$ S.union a b"
        $$ text "cspm_set_union" <+> prefixSetUnionArgumentA <+> prefixSetUnionArgumentB
            <+> equals <+> prefixSetUnion <+> parens (text "a ++ b")
        $$ text "cspm_set_union" <+> prefixSetUnionArgumentA <+> text "b"
            <+> equals <+> prefixSetUnion <+> parens (text "b:a")
        $$ text "cspm_set_union" <+> text "a" <+> prefixSetUnionArgumentB
            <+> equals <+> prefixSetUnion <+> parens (text "a:b")
        $$ (if not (null matchingConstructors) then
            text "cspm_set_union" <+> text "a" <+> text "b" <+> equals
            <+> prefixSetUnion <+> brackets (list (mapM text ["a", "b"]))
            else empty)
        )

    ---- CSPM_Show (CSPM_Set)
    $$ text "instance"
        <+> text "CSPM_Show (CSPM_SetType" <+> typeTuple <> char ')'
        <+> text "where"

    $$ tabIndent (
        text "cspm_show" <+> prefixSetUnionArgumentA <+> equals
            <+> text "cspm_show_union_set a"
        $$ text "cspm_show" <+> prefixExplicitArgumentA <+> equals
            <+> text "braces $ cspm_prettyPrint_list $ map cspm_show $ S.toList a"
        $$ vcat (mapM (\ clause -> 
            text "cspm_show" <+> prefixConstructorArgumentA clause <+> equals
            <+> text "cspm_prettyPrint_dotSep $"
            <+> text "text" <+> doubleQuotes (return $ PP.prettyPrint (constructorName clause))
            <+> char ':'
            <+> brackets (list (mapM (\ i -> text "cspm_show a" <> int i)
                    [1..clauseAlreadyFoundFieldCount prefix clause]))
            ) matchingConstructors)
        )

    ---- CSPM_Ord (CSPM_Set)
    $$ text "instance"
        <+> text "CSPM_Ord (CSPM_SetType" <+> typeTuple <> char ')'
        <+> text "where"

     $$ tabIndent (
        text "cspm_compare" <+> parens (text "s1@(" <> prefixSetUnion <+> text "{})")
            <+> text "s2" <+> equals
            <+> text "cspm_compare (cspm_set_make_explicit s1) s2"
        $$ text "cspm_compare s1" <+> parens (text "s2@(" <> prefixSetUnion <+> text "{})")
            <+> equals <+> text "cspm_flipOrder (cspm_compare s1 s1)"
        $$ text "cspm_compare" <+> prefixExplicitArgumentA <+> prefixExplicitArgumentB <+> equals
        $$ tabIndent (
            text "if a == b then Just EQ"
            $$ text "else if S.isProperSubsetOf a b then Just LT"
            $$ text "else if S.isProperSubsetOf b a then Just GT"
            $$ text "else Nothing"
        )
        $$ vcat (mapM (\ clause -> 
            text "cspm_compare" <+> prefixConstructorArgumentA clause
            <+> prefixConstructorArgumentB clause <+> equals
            <+> text "cspm_set_product_order" <+> brackets (list (
                    mapM (\i -> text "cspm_compare a" <> int i <+> text "b" <> int i)
                    [1..clauseAlreadyFoundFieldCount prefix clause]
                ))
            ) matchingConstructors)
        $$ text "cspm_compare s1" <+> parens (text "s2@(" <> prefixExplicit <+> text "{})")
            <+> equals <+> text "cspm_compare (cspm_set_make_explicit s1) s2"
        $$ text "cspm_compare" <+> parens (text "s1@(" <> prefixExplicit <+> text "{})")
            <+> text "s2" <+> equals <+> text "cspm_flipOrder $ cspm_compare s2 s1"
        $$ (if length matchingConstructors > 1 then
            text "cspm_compare s1 s2 = cspm_compare (cspm_set_make_explicit s1) (cspm_set_make_explicit s2)"
            else empty)
        )
