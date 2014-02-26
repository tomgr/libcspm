module CSPM.HaskellEvaluator.Prelude (
    bindInitialEnvironment,
    translateBuiltIn,
) where

import qualified Data.Map as M

import CSPM.DataStructures.Names
import CSPM.HaskellEvaluator.ConstantCode
import CSPM.HaskellEvaluator.GhcJIT
import CSPM.HaskellEvaluator.Monad
import CSPM.HaskellEvaluator.TranslateName (translateName)
import CSPM.Prelude
import Util.Annotated
import Util.Exception
import Util.MonadicPrettyPrint

bindInitialEnvironment :: TranslationMonad ()
bindInitialEnvironment = do
    bindAndLoadModule $
        text constantCode
        $$ translatePrelude
        $$ vcat (mapM generateTrivialSetInstance [
                text "Bool", text "Char"
            ])
        $$ vcat (mapM generateTupleInstances [2..5])

translatePrelude :: TranslationMonad Doc
translatePrelude = do
    translateName (builtInName "show") <+> text ":: CSPM_Show a => a -> [Char]"
    $$ translateName (builtInName "show") <+> text "=" <+> text "show . cspm_show"
    $$ translateName (builtInName "Bool") <+> text ":: CSPM_SetType Bool"
    $$ translateName (builtInName "Bool") <+> equals <+> text "cspm_set_fromList [True, False]"
    $$ translateName (builtInName "Char") <+> text ":: CSPM_SetType Char"
    $$ translateName (builtInName "Char") <+> equals <+> text "cspm_set_fromList [minBound :: Char ..]"
    $$ translateName (builtInName "transpose")
        <+> text ":: (CSPM_Set (a, b), CSPM_Set (b, a)) => CSPM_SetType (a, b) -> CSPM_SetType (b, a)"
    $$ translateName (builtInName "transpose") <+> equals
        <+> text "cspm_set_fromList . map (\\ (a, b) -> (b, a)) . cspm_set_toList"
    $$ translateName (builtInName "mapUpdateMultiple") <+> text "::"
        <+> text "Ord k => M.Map k v -> [(k, v)] -> M.Map k v"
    $$ translateName (builtInName "mapUpdateMultiple") <+> equals
        <+> text "foldr (uncurry M.insert)"
    $$ translateName (builtInName "mapLookup") <+> text "::"
        <+> text "Ord k => Doc -> M.Map k v -> k -> v"
    $$ translateName (builtInName "mapLookup") <+> text "location m k ="
    $$ tabIndent (
            text "case M.lookup k m of"
            $$ tabIndent (
                text "Just v -> v"
                $$ text "Nothing -> cspm_panic location (text \"\")"
            )
        )

builtinMap :: M.Map Name (TranslationMonad Doc)
builtinMap = M.fromList $ [
        (builtInName "card", text "cspm_set_card"),
        (builtInName "concat", text "concat"),
        (builtInName "diff", text "cspm_set_diff"),
        (builtInName "elem", text "elem"),
        (builtInName "empty", text "cspm_set_null"),
        (builtInName "emptyMap", text "M.empty"),
        (builtInName "error", text "cspm_user_error"),
        (builtInName "extensions", text "cspm_extensions"),
        (builtInName "False", text "False"),
        (builtInName "false", text "False"),
        (builtInName "inter", text "cspm_set_inter"),
        (builtInName "Inter", text "cspm_set_inters"),
        (builtInName "Int", text "CSPM_Integers"),
        (builtInName "length", text "length"),
        (builtInName "Map", text "CSPM_AllMaps"),
        (builtInName "mapFromList", text "M.fromList"),
        (builtInName "mapMember", text "flip M.member"),
        (builtInName "mapToList", text "M.toList"),
        (builtInName "mapUpdate", text "\\ m k v -> M.insert k v m"),
        (builtInName "member", text "cspm_set_member"),
        (builtInName "productions", text "cspm_productions"),
        (builtInName "null", text "null"),
        (builtInName "seq", text "cspm_set_toList"),
        (builtInName "set", text "cspm_set_fromList"),
        (builtInName "Set", text "CSPM_Powerset"),
        (builtInName "Seq", text "CSPM_AllSequences"),
        (builtInName "True", text "True"),
        (builtInName "true", text "True"),
        (builtInName "union", text "cspm_set_union"),
        (builtInName "Union", text "cspm_set_unions . cspm_set_toList")
    ]
    ++ [(name, translateName name)
            | name <- map builtInName [
                "Bool",
                "Char",
                "Events",
                "mapLookup",
                "mapUpdateMultiple",
                "show",
                "transpose"]
        ]

translateBuiltIn :: SrcSpan -> Name -> TranslationMonad Doc
translateBuiltIn location name | name == builtInName "tail" =
    text "cspm_tail"
    <+> parens (text "text" <+> (doubleQuotes (prettyPrint location)))
translateBuiltIn location name | name == builtInName "head" =
    text "cspm_head"
    <+> parens (text "text" <+> (doubleQuotes (prettyPrint location)))
translateBuiltIn location name | name == builtInName "mapLookup" =
    translateName (builtInName "mapLookup")
    <+> parens (text "text" <+> (doubleQuotes (prettyPrint location)))
translateBuiltIn _ name =
    case M.lookup name builtinMap of
        Just doc -> doc
        _ -> panic $ "Not supported builtin "++show name
