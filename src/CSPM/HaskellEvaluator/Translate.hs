module CSPM.HaskellEvaluator.Translate (
    translateFile,
    translateExpression,
    translateTypeExpression,
    translatePattern,
    translateType,
    translateLocation,
    translateError, translateError',
    PatternSideCondition(..),
) where

import Control.Applicative (Applicative)
import Control.Monad.State
import Data.List (nub, sort)

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types
import CSPM.HaskellEvaluator.Monad
import CSPM.HaskellEvaluator.Prelude
import {-# SOURCE #-} CSPM.HaskellEvaluator.TranslateDataType
import CSPM.HaskellEvaluator.TranslateName
import CSPM.Prelude
import CSPM.PrettyPrinter ()
import Util.Annotated
import Util.Exception
import Util.MonadicPrettyPrint
import qualified Util.PartialFunctions as PF
import qualified Util.PrettyPrint as PP

translateLocation :: (Applicative m, Monad m) => SrcSpan -> m Doc
translateLocation location =
    parens (text "text" <+> doubleQuotes (prettyPrint location))

translateError :: (Applicative m, Monad m) => SrcSpan -> m Doc -> m Doc
translateError location message =
    text "cspm_panic" <+> translateLocation location <+> parens message

translateError' :: (Applicative m, Monad m) => String -> m Doc -> m Doc
translateError' location message =
    text "cspm_panic" <+> text location <+> parens message

translateFile :: TCCSPMFile -> TranslationMonad Doc
translateFile (An _ _ (CSPMFile ds)) = do
    registerDataTypes ds
    vcat (mapM translateDecl ds)
        $$ translateDataType (builtInName "Events")

translateFunctionPatternMatchError :: SrcSpan -> [Int] -> Name ->
    TranslationMonad Doc -> TranslationMonad Doc
translateFunctionPatternMatchError location argumentCountByGroup name formattedName =
    if argumentCount > 0 then
        translateName name
        <+> hsep (mapM (\ i -> text "arg_" <> int i) [1..argumentCount])
        <+> char '='
        <+> translateError location (
                text "text" <+> doubleQuotes (text "Pattern match failure whilst attempting to evaluate:")
                <+> text "$$" <+> text "text" <+> doubleQuotes formattedName
                    <+> text "<>"
                    <+> hsep (punctuate (text " <>") $
                        mapM (\ is -> text "parens" <+> parens (
                            text "cspm_prettyPrint_list" <+> brackets (list (
                                mapM (\ i -> text "cspm_show" <+> text "arg_" <> int i) is)))
                        ) argumentIndicesByGroup
                    )
                )
    else empty
    where
        argumentCount = sum argumentCountByGroup
        argumentIndicesByGroup =
            zipWith (\ group start -> [start..start+group-1])
                argumentCountByGroup (scanl (+) 1 argumentCountByGroup)

translatePatternMatchError :: SrcSpan -> TCPat -> TranslationMonad Doc
translatePatternMatchError location pattern = 
    text "value" <+> text "->" <+> translateError location (
        text "text" <+> doubleQuotes (text "Pattern match failure: Value")
        <+> text "$$ " <+> parens (
            text "cspm_prettyPrint_tabIndent" <+> parens (text "cspm_show value"))
        <+> text "$$ " <+> text "text \"Does not match the pattern:\""
        <+> text "$$ " <+> parens (
                text "cspm_prettyPrint_tabIndent" <+> parens (text "text"
                <+> doubleQuotes (text (show (PP.prettyPrint pattern)))))
        )

translateDecl :: TCDecl -> TranslationMonad Doc
translateDecl (An location (Just symbolTable, _) (FunBind name matches _)) =
    translateName name <+> text "::" <+>
        translateTypeScheme (PF.apply symbolTable name)
    $+$ vcat (mapM (\ m -> translateName name <+> translateMatch m) matches)
    $$ translateFunctionPatternMatchError location argumentCountByGroup name (prettyPrint name)
    where
            argumentCountByGroup =
                case head matches of
                    An _ _ (Match ps _) -> map length ps
translateDecl (An location (Just symbolTable, _) (PatBind (An _ _ (PVar name)) rightHandSide _)) = do
    translateName name <+> text "::" <+> translateTypeScheme boundVarType
    $+$ translateName name <+> text "=" <+> translateExpression rightHandSide
    where
            [(_, boundVarType)] = symbolTable
translateDecl (An location (Just symbolTable, _) (PatBind pattern rightHandSide _)) = do
    (translatedPattern, sideConditions) <- translatePattern pattern
    translateName name <+> text "::" <+> translateTypeScheme boundVarType
        $+$ translateName name <+> text "="
        $$ tabIndent (
            text "case" <+> translateExpression rightHandSide <+> text "of"
            $$ tabIndent (
                    translatedPattern
                    <+> case sideConditions of
                        [] -> empty
                        _ -> char '|' <+> formatPatternSideConditions sideConditions
                    <+> text "->" <+> translateName name
                    $$ translatePatternMatchError location pattern
                ))
        where
            [(name, boundVarType)] = symbolTable
translateDecl (dataType@(An _ _ (DataType n cs))) = translateDataType n
translateDecl (An _ (Just [(_, boundVarType)], _) (SubType name cs)) =
    translateName name <+> text "::" <+> translateTypeScheme boundVarType
    $+$ translateName name <+> text "=" <+> text "cspm_set_unions $ concat" <+>
        brackets (list (
            mapM (\ (An _ _ (DataTypeClause n mexp)) ->
                case mexp of
                    Just exp ->
                        text "map cspm_productions $"
                        <+> brackets (text "cspm_dotOn" <+> translateName n
                        <+> text "x" <+> text "| x <- cspm_set_toList" <+> translateTypeExpression exp)
                    Nothing -> brackets (text "cspm_productions" <+> translateName n)
                ) cs)
        )
    where ForAll _ (TSet (TDatatype dataTypeName)) = boundVarType
translateDecl (An _ (Just [(_, boundVarType)], _) (NameType name e)) =
    translateName name <+> text "::" <+> translateTypeScheme boundVarType
    $+$ translateName name <+> text "=" <+> translateTypeExpression e
translateDecl (An _ _ (TimedSection _ _ _)) = panic "timed sections are not supported"
translateDecl (An _ _ (Channel _ _)) = empty
translateDecl (An _ _ (External _)) = empty
translateDecl (An _ _ (Transparent _)) = empty
translateDecl (An _ _ (Assert _)) = empty
translateDecl (An _ _ (PrintStatement _)) = empty

translateTypeScheme :: TypeScheme -> TranslationMonad Doc
translateTypeScheme (ForAll constraints t) =
    (case typeVars of
        [] -> empty
        _ -> text "forall" <+> hsep (mapM translateTypeVar typeVars) <+> char '.'
    )
    <+> (case requiredClasses of 
        [] -> empty
        _ -> parens (list (sequence requiredClasses)) <+> text "=>"
    )
    <+> translateType t
    where
        typeVars = nub $ sort $ map fst constraints
        explicitClasses, implicitClasses, requiredClasses :: [TranslationMonad Doc]
        requiredClasses = explicitClasses ++ implicitClasses
        explicitClasses =
            concatMap (\ (tvar, cs) -> concatMap (translateConstraint tvar) cs)
                constraints
        implicitClasses =
            -- For each argument we need a show instance for the pretty printer
            map (\ (tvar, _) -> text "CSPM_Show" <+> translateTypeVar tvar)
                constraints

translateTypeVar :: TypeVar -> TranslationMonad Doc
translateTypeVar tv = text "gen_type_" <> return (PP.prettyPrint tv)

translateConstraint :: TypeVar -> Constraint -> [TranslationMonad Doc]
translateConstraint tv CEq = [
        text "Eq" <+> translateTypeVar tv,
        text "CSPM_Set" <+> translateTypeVar tv
    ]
translateConstraint tv COrd = [text "CSPM_Ord" <+> translateTypeVar tv] 
translateConstraint tv CSet = [
    text "CSPM_Set" <+> translateTypeVar tv,
    text "CSPM_Show (CSPM_SetType" <+> translateTypeVar tv <+> text ")"]
-- CInputable
-- CYieldable

translateType :: Type -> TranslationMonad Doc
translateType TBool = text "Bool"
translateType (TDotable t1 t2) =
    parens (text "CSPM_Yield" <+> translateType t1 <+> translateType t2
        <+> translateType (foldr1 TDot arguments)
        <+> translateType ultimateReturnType)
    where
        (arguments, ultimateReturnType) = splitDotable (TDotable t1 t2)
translateType (TDatatype n) = translateDataTypeName n
translateType TChar = text "Char"
translateType (TDot t1 t2) =
    parens (text "CSPM_ExplicitDot" <+> parens (translateType t1)
        <+> parens (translateType t2))
translateType (TExtendable rt arg) =
    parens (text "CSPM_Yield b c" <+> translateType (TVar arg) <+> translateType rt)
translateType TEvent = translateDataTypeName (builtInName "Events")
translateType (TFunction args returnType) =
    -- This converts functions with no arguments into patterns instead.
    parens (hcat $ punctuate (text " -> ") (mapM translateType (args ++ [returnType])))
translateType TInt = text "Int"
--translateType TProc = text "CSPM_Proc"
translateType (TMap k v) =
    parens (text "M.Map" <+> translateType k <+> translateType v)
translateType (TSet t) = parens (text "CSPM_SetType" <+> translateType t)
translateType (TSeq t) = brackets (translateType t)
translateType (TTuple ts) = parens (list (mapM translateType ts))
translateType (TVar tv) = translateTypeVar (typeVar tv)
translateType _ = panic "type not supported"

translateMatch :: TCMatch -> TranslationMonad Doc
translateMatch (An _ _ (Match patterns rightHandSide)) = do
    translatedPatterns <- mapM translatePattern (concat patterns)
    let
        compiledPatterns = map fst translatedPatterns
        sideConditions = concatMap snd translatedPatterns
    
    hsep (sequence compiledPatterns)
        <+> case sideConditions of
                [] -> empty
                _ -> char '|' <+> formatPatternSideConditions sideConditions
        <+> char '='
        <+> translateExpression rightHandSide

formatPatternSideConditions :: [PatternSideCondition] -> TranslationMonad Doc
formatPatternSideConditions conditions =
    list (mapM (\ condition ->
        case condition of
            MatchSideCondition p e -> p <+> text "<-" <+> e
            PredicateSideCondition p -> p) conditions)

formatListComprehensionSideConditions :: TranslationMonad Doc ->
    [PatternSideCondition] -> TranslationMonad Doc
formatListComprehensionSideConditions firstGenerator conditions =
    list (sequence $ firstGenerator :
        map (\ condition ->
            case condition of
                MatchSideCondition p e -> p <+> text "<-" <+> brackets e
                PredicateSideCondition p -> p) conditions)

translatePattern :: TCPat -> TranslationMonad (TranslationMonad Doc, [PatternSideCondition])
translatePattern pat@(An _ _ (PDotApp _ _)) = translateDotPattern pat
translatePattern (An _ _ (PCompList start Nothing _)) = do
    translatedPatterns <- mapM translatePattern start
    let
        compiledPatterns = map fst translatedPatterns
        sideConditions = concatMap snd translatedPatterns
    return (brackets (list (sequence compiledPatterns)), sideConditions)
translatePattern (An _ _ (PCompList start (Just (middle, [])) _)) = do
    translatedPatterns <- mapM translatePattern start
    (lastPattern, extraSideConditions) <- translatePattern middle
    let
        compiledPatterns = map fst translatedPatterns
        sideConditions = concatMap snd translatedPatterns
    return (parens (hcat (punctuate (char ':') (sequence (
            compiledPatterns ++ [lastPattern])))),
        sideConditions ++ extraSideConditions)
translatePattern (An _ _ (PCompList start (Just (middle, end)) _)) = do
    translatedPatterns <- mapM translatePattern start
    (middlePattern, middleSideConditions) <- translatePattern middle
    translatedEnd <- mapM translatePattern end
    remainderName <- liftIO $ mkFreshInternalName
    let
        compiledPatterns = map fst translatedPatterns
        sideConditions = concatMap snd translatedPatterns

        compiledEndPatterns = map fst translatedEnd
        endSideConditions = concatMap snd translatedEnd

        newSideCondition = MatchSideCondition
            (text "Just"
                <+> parens (middlePattern <> comma <+>
                        brackets (list (sequence compiledEndPatterns))))
            (text "cspm_split_last"
                <+> int (length compiledEndPatterns)
                <+> translateName remainderName)
    return (parens (hcat (punctuate (char ':') (sequence (
            compiledPatterns ++ [translateName remainderName])))),
        sideConditions ++ [newSideCondition] ++ middleSideConditions ++
        endSideConditions)
translatePattern (An _ _ (PDoublePattern left right)) = do
    remainderName <- liftIO $ mkFreshInternalName
    (leftCompiled, leftSideConditions) <- translatePattern left
    (rightCompiled, rightSideConditions) <- translatePattern right
    return (translateName remainderName <+> char '@' <+> parens leftCompiled,
        MatchSideCondition rightCompiled (translateName remainderName)
        : leftSideConditions ++ rightSideConditions)
translatePattern (An _ _ (PLit literal)) = return (translateLiteral literal, [])
translatePattern (An _ _ (PSet [])) = do
    setName <- liftIO $ mkFreshInternalName
    return (translateName setName, [
            PredicateSideCondition $ text "cspm_set_null" <+> translateName setName
        ])
translatePattern (An _ _ (PSet [p])) = do
    setName <- liftIO $ mkFreshInternalName
    (pCompiled, pSideConditions) <- translatePattern p
-- TODO: Maybe optimise.
    return (translateName setName, [
        MatchSideCondition (brackets pCompiled)
            (text "cspm_set_toList" <+> translateName setName)
        ])
translatePattern (An _ _ (PTuple ps)) = do
    translatedPatterns <- mapM translatePattern ps
    let
        compiledPatterns = map fst translatedPatterns
        sideConditions = concatMap snd translatedPatterns
    return (parens (list (sequence compiledPatterns)), sideConditions)
translatePattern (An _ _ (PVar name)) | isNameDataConstructor name = return
    (translateDataTypeClauseName 0 name, [])
translatePattern (An _ _ (PVar name)) = return (translateName name, [])
translatePattern (An _ _ PWildCard) = return (char '_', [])
translatePattern x = panic $ "translatePattern: not supported: " ++ show x

translateTypeExpression :: TCExp -> TranslationMonad Doc
translateTypeExpression (An _ _ (Tuple es)) =
    let size = length es
    in parens (text "CSPM_Tuple" <> int size <> text "Product"
        <+> hsep (mapM (\ exp -> translateTypeExpression exp) es))
translateTypeExpression (An _ _ (DotApp e1 e2)) =
    parens (text "CSPM_ExplicitDotProduct" <+> parens (translateTypeExpression e1)
        <+> translateTypeExpression e2)
translateTypeExpression x = translateExpression x

translateExpression :: TCExp -> TranslationMonad Doc
translateExpression exp =
    parens (translateExpression' exp <+> text "::" <+> translateType (getType exp))

translateExpression' :: TCExp -> TranslationMonad Doc
translateExpression' (An _ _ (App function args)) =
    translateExpression function <+>
        hsep (mapM (\ arg -> translateExpression arg) args)
translateExpression' (An _ _ (BooleanBinaryOp op left right)) =
    if op `elem` [And, Or, Equals, NotEquals] then
        translateExpression left <+> translateOp op
        <+> translateExpression right
    else if op `elem` [] then
        translateOp op <+> translateExpression left
        <+> translateExpression right
    else text "cspm_compare" <+> translateExpression left
        <+> translateExpression right <+> translateOp op
    where
        translateOp And = text "&&"
        translateOp Or = text "||"
        translateOp Equals = text "=="
        translateOp NotEquals = text "/="
        translateOp LessThan = text "== Just LT"
        translateOp GreaterThan = text "== Just GT"
        translateOp LessThanEq = text "`elem` [Just LT, Just EQ]"
        translateOp GreaterThanEq = text "`elem` [Just GT, Just EQ]"
translateExpression' (An _ _ (BooleanUnaryOp Not exp)) =
    text "not" <+> translateExpression exp
translateExpression' (An _ _ (Concat left right)) =
    translateExpression left <+> text "++" <+> translateExpression right
translateExpression' (exp@(An _ _ (DotApp _ _))) =
    translateDotApplication exp
translateExpression' (An _ _ (If condition thenBranch elseBranch)) =
    text "if" <+> translateExpression condition <+> text "then"
        <+> translateExpression thenBranch
    $$ text "else" <+> translateExpression elseBranch
translateExpression' (An location (Just lambdaType, _) (Lambda patterns right)) = do
    translatedPatterns <- mapM translatePattern patterns
    functionName <- liftIO $ mkFreshInternalName
    let
        compiledPatterns = map fst translatedPatterns
        sideConditions = concatMap snd translatedPatterns

    text "let"
        $+$ tabIndent (vcat $ sequence [
            translateName functionName <+> text "::" <+> translateType lambdaType,
            translateName functionName
                <+> hsep (sequence compiledPatterns)
                <+> case sideConditions of
                        [] -> empty
                        _ -> char '|' <+> formatPatternSideConditions sideConditions
                <+> equals
                <+> translateExpression right,
            translateFunctionPatternMatchError location [length patterns] functionName
                (text "<lambda>")
            ])
        $+$ text "in" <+> translateName functionName
translateExpression' (An location _ (Let decls exp)) =
    text "let"
    $+$ tabIndent (vcat (mapM translateDecl decls))
    $+$ text "in" <+> translateExpression exp
translateExpression' (An _ (Just t, _) (List items)) =
    brackets (list (mapM translateExpression items))
translateExpression' (An _ _ (ListComp [item] statements)) = brackets $
    translateExpression item
    <+> char '|' <+> list (mapM translateStatement statements)
translateExpression' (An _ _ (ListComp items statements)) =
    text "concat $" <+> brackets (
        brackets (list (mapM translateExpression items))
        <+> char '|' <+> list (mapM translateStatement statements))
translateExpression' (An _ _ (ListEnumFrom lower)) = brackets $
    translateExpression lower <+> text ".."
translateExpression' (An _ _ (ListEnumFromTo lower upper)) = brackets $
    translateExpression lower <+> text ".." <+> translateExpression upper
translateExpression' (An _ _ (ListEnumFromComp lower statements)) =
    text "concat" <+> brackets (
        brackets (translateExpression lower <+> text "..")
        <+> char '|' <+> list (mapM translateStatement statements))
translateExpression' (An _ _ (ListEnumFromToComp lower upper statements)) =
    text "concat" <+> brackets (
        brackets (translateExpression lower <+> text ".." <+>
            translateExpression upper)
        <+> char '|' <+> list (mapM translateStatement statements))
translateExpression' (An _ _ (Lit (Int i))) = int i
translateExpression' (An _ _ (Lit literal)) = translateLiteral literal
translateExpression' (An _ _ (ListLength list)) =
    text "length" <+> translateExpression list
translateExpression' (An _ _ (Map keyValuePairs)) =
    text "M.fromList"
    <+> brackets (list (mapM (\ (key, value) -> parens (
            translateExpression key <> comma <+> translateExpression value)
            ) keyValuePairs))
translateExpression' (An loc _ (MathsBinaryOp op left right)) =
    if op `elem` [Divide, Mod] then
        translateOp op
        <+> parens (text "text" <+> doubleQuotes (prettyPrint loc))
        <+> translateExpression left <+> translateExpression right
    else translateExpression left <+> translateOp op
        <+> translateExpression right
    where
        translateOp Divide = text "cspm_div"
        translateOp Mod = text "cspm_mod"
        translateOp Minus = text "-"
        translateOp Plus = text "+"
        translateOp Times = text "*"
translateExpression' (An _ _ (MathsUnaryOp Negate exp)) =
    char '-' <+> translateExpression exp
translateExpression' (An _ _ (Tuple args)) =
    parens (list (mapM translateExpression args))
translateExpression' (An _ _ (Set [])) = text "cspm_set_empty"
translateExpression' (An _ _ (Set es)) =
    text "cspm_set_fromList" <+> brackets (list (mapM translateExpression es))
translateExpression' (An _ _ (SetComp [item] statements)) = 
    text "cspm_set_fromList" <+> brackets (
        translateExpression item
        <+> char '|' <+> list (mapM translateSetStatement statements)
    )
translateExpression' (An _ _ (SetComp items statements)) = 
    text "cspm_set_fromList $ concat" <+> brackets (
        brackets (list (mapM translateExpression items))
        <+> char '|' <+> list (mapM translateSetStatement statements)
    )
translateExpression' (An _ _ (SetEnum items)) =
    text "cspm_set_unions" <+> brackets (list (mapM (\ item ->
            text "cspm_productions" <+> translateExpression item
        ) items))
translateExpression' (An _ _ (SetEnumComp items statements)) =
    text "cspm_set_unions $ concat" <+> brackets (
        brackets (list (mapM (\ item ->
            text "cspm_productions" <+> translateExpression item
            ) items))
        <+> char '|' <+> list (mapM translateSetStatement statements)
        )
translateExpression' (An _ _ (SetEnumFrom lb)) =
    text "CSPM_IntSetFrom" <+> translateExpression lb
translateExpression' (An _ _ (SetEnumFromTo lb ub)) =
    text "cspm_set_fromList" <+> brackets (
        translateExpression lb <+> text ".." <+> translateExpression ub)
translateExpression' (An _ _ (SetEnumFromComp lb statements)) = 
    text "cspm_set_unions" <+> brackets (
        text "CSPM_IntSetFrom" <+> translateExpression lb
        <+> char '|' <+> list (mapM translateSetStatement statements)
    )
translateExpression' (An _ _ (SetEnumFromToComp lb ub statements)) = 
    text "cspm_set_fromList $ concat" <+> brackets (
        brackets (translateExpression lb <+> text ".." <+> translateExpression ub)
        <+> char '|' <+> list (mapM translateSetStatement statements)
    )
translateExpression' (An loc _ (Var name)) | nameType name == WiredInName =
    translateBuiltIn loc name
translateExpression' (An _ _ (Var name)) = translateName name
translateExpression' e = panic $ "expression not supported: "++show e

translateStatement :: TCStmt -> TranslationMonad Doc
translateStatement (An _ _ (Generator p e)) = do
    (translatedPattern, sideConditions) <- translatePattern p
    formatListComprehensionSideConditions 
        (translatedPattern <+> text "<-" <+> translateExpression e)
        sideConditions
translateStatement (An _ _ (Qualifier e)) = translateExpression e

translateSetStatement :: TCStmt -> TranslationMonad Doc
translateSetStatement (An _ _ (Generator p e)) = do
    (translatedPattern, sideConditions) <- translatePattern p
    formatListComprehensionSideConditions 
        (translatedPattern <+> text "<-" <+>
            text "cspm_set_toList" <+> translateExpression e)
        sideConditions
translateSetStatement (An _ _ (Qualifier e)) = translateExpression e

translateLiteral :: Literal -> TranslationMonad Doc
translateLiteral (Bool True) = text "True"
translateLiteral (Bool False) = text "False"
translateLiteral (Char c) = quotes (char c)
translateLiteral (Int i) | i < 0 = parens (int i)
translateLiteral (Int i) = int i
translateLiteral (String s) = doubleQuotes (text s)
