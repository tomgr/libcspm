module CSPM.Syntax.DataTypeAnalyser (
    DataTypeInformation(..), DataTypeConstructor(..), FieldSet(..),
    dataTypesForDeclarations,
) where

import qualified Data.Map as M

import CSPM.Syntax.Names
import CSPM.Syntax.AST
import CSPM.Syntax.Types
import CSPM.Prelude
import Util.Annotated
import Util.Exception

data FieldSet =
    -- | Indicates the field set is deduced by evaluating the given expression.
    SimpleFieldSet {
        simpleFieldSetExpression :: TCExp
    }
    -- | Represents a more complex field set. The only may to extract the field
    -- set is to evaluate the given expression, and then decompose the resulting
    -- valueset into a cartesian product, and then extract the indexed field.
    | CompoundFieldSet {
        compoundFieldSetToExtract :: Int,
        compoundFieldSetExpression :: TCExp
    }
    deriving (Eq, Ord, Show)

data DataTypeConstructor = DataTypeConstructor {
        -- | The name of the constructor.
        constructorName :: Name,
        -- | The number of fields of the construtor.
        constructorFieldCount :: Int,
        -- | The types of the fields of the construtor.
        constructorFieldTypes :: [Type],
        -- | A description of the set of all values allowed for each field.
        constructorDecomposedFieldSets :: [FieldSet],
        -- | Indicates if each field set is `trivial` in the sense that it
        -- permits any value of the appropriate type. For example, in
        --
        --  datatype X = Y.Bool.{0..1}
        --
        -- The first field (Bool) is trivial, because any boolean is in Bool,
        -- whereas the second field is not trivial, since there are integers
        -- that are not in {0,1}.
        constructorFieldSetIsTrivial :: [Bool]
    }
    deriving Show

data DataTypeInformation = DataTypeInformation {
        dataTypeType :: Type,
        dataTypeName :: Name,
        -- | Map from constructor name to constructor.
        dataTypeConstructors :: M.Map Name DataTypeConstructor
    }
    deriving Show

-- | Computes all datatypes that are given in the declarations.
dataTypesForDeclarations :: [TCDecl] -> [DataTypeInformation]
dataTypesForDeclarations ds =
    computeEventsDataType ds
    ++ concatMap computeDataTypeInformation ds

computeDataTypeInformation :: TCDecl -> [DataTypeInformation]
computeDataTypeInformation (An _ _ (DataType dataTypeName clauses)) =
    let
        computeDataTypeClauseInformation
                (An _ _ (DataTypeClause constructorName constructorTypeExpression _)) =
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
    in [DataTypeInformation {
        dataTypeName = dataTypeName,
        dataTypeType =
            if dataTypeName == builtInName "Events" then TEvent
            else TDatatype dataTypeName,
        dataTypeConstructors = M.fromList
            [(constructorName c, c) | c <- dataTypeConstructors]
    }]
computeDataTypeInformation _ = []

-- | Converts channel declarations into a datatype declaration called Events.
-- (They're exactly the same.)
computeEventsDataType :: [TCDecl] -> [DataTypeInformation]
computeEventsDataType declarations =
    let
        clauses = concatMap (\ decl ->
                case decl of
                    An _ _ (Channel ns decl ta) ->
                        [An Unknown (panic "Dummy annotation") (DataTypeClause n decl ta)
                        | n <- ns]
                    _ -> []
            ) declarations
        dataType = An Unknown (panic "Dummy annotation") $
                    DataType (builtInName "Events") clauses
    in computeDataTypeInformation dataType

decomposeDataTypeTypeExpression :: TCExp -> [FieldSet]
decomposeDataTypeTypeExpression (An _ _ (DotApp e1 e2)) =
    concatMap decomposeDataTypeTypeExpression [e1, e2]
decomposeDataTypeTypeExpression exp@(An _ (TSet typ@(TDot _ _), _) x) =
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
fieldExpressionIsTrivial (An _ (TSet (TDatatype _), _) (Var _)) = True
fieldExpressionIsTrivial (An _ (TSet TEvent, _) (Var _)) = True
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
