module CSPM.HaskellEvaluator.TranslateName (
    translateName,
    translateDataTypeClauseName,
    translateDataTypeName,
) where

import CSPM.DataStructures.Names
import CSPM.HaskellEvaluator.Monad
import Util.MonadicPrettyPrint

translateName :: Name -> TranslationMonad Doc
translateName n | isNameDataConstructor n =
    text "gen_" <> int (nameUnique n) <> text "_Default"
translateName n = text "gen_" <> int (nameUnique n)

translateDataTypeClauseName :: Int -> Name -> TranslationMonad Doc
translateDataTypeClauseName remainingFields n =
    text "Gen_" <> int (nameUnique n) <> text "_Remaining" <> int remainingFields

translateDataTypeName :: Name -> TranslationMonad Doc
translateDataTypeName n = text "Type_Gen_" <> int (nameUnique n)
