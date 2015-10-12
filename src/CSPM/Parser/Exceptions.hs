module CSPM.Parser.Exceptions (
    invalidPatternErrorMessage,
    invalidLetDeclarationErrorMessage,
    invalidModuleDeclarationErrorMessage,
    invalidFunctionArgsErrorMessage,
    invalidExpressionErrorMessage,
    invalidIncludeErrorMessage,
    invalidTimedSectionDeclarationErrorMessage,
    lexicalErrorMessage,
    parseErrorMessage,
    fileAccessErrorMessage,
    ambiguousTypeAnnotationsError,
    unusedTypeAnnotationsError,
    unknownConstraintError,
    definitionSpanFileError,
    looksLikeRTFErrorMessage,
    duplicateModelOptionsError,
    ambiguousChannelTypeError,
    commentNotClosedErrorMessage,
    invalidSymmetrySpecification,
    
    throwSourceError
)
where

import CSPM.Syntax.Names
import CSPM.Syntax.AST
import CSPM.Parser.Tokens
import CSPM.PrettyPrinter
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

invalidPatternErrorMessage :: PExp -> ErrorMessage
invalidPatternErrorMessage e = mkErrorMessage (loc e) $
    hang (prettyPrint e) tabWidth (text "is not a valid pattern")
    
invalidLetDeclarationErrorMessage :: PDecl -> ErrorMessage
invalidLetDeclarationErrorMessage d = mkErrorMessage (loc d) $
    hang (prettyPrint d) tabWidth (text "is not a valid declaration in a let expression")

invalidModuleDeclarationErrorMessage :: PDecl -> ErrorMessage
invalidModuleDeclarationErrorMessage d = mkErrorMessage (loc d) $
    hang (prettyPrint d) tabWidth (text "is not a valid declaration in a module")

invalidTimedSectionDeclarationErrorMessage :: PDecl -> ErrorMessage
invalidTimedSectionDeclarationErrorMessage d = mkErrorMessage (loc d) $
    hang (prettyPrint d) tabWidth (text "is not a valid declaration in a timed section")

invalidExpressionErrorMessage :: PExp -> ErrorMessage
invalidExpressionErrorMessage e = mkErrorMessage (loc e) $
    hang (prettyPrint e) tabWidth (text "is not a valid expression")

invalidFunctionArgsErrorMessage :: PSType -> ErrorMessage
invalidFunctionArgsErrorMessage t = mkErrorMessage (loc t) $
    hang (prettyPrint t) tabWidth
        (text "is not a valid type for a function argument list")

invalidIncludeErrorMessage :: SrcSpan -> ErrorMessage
invalidIncludeErrorMessage srcspan = 
    mkErrorMessage srcspan (text "Invalid include directive")

lexicalErrorMessage :: SrcSpan -> ErrorMessage
lexicalErrorMessage srcspan = mkErrorMessage srcspan (text "Lexical error")

commentNotClosedErrorMessage :: SrcSpan -> ErrorMessage
commentNotClosedErrorMessage srcspan =
    mkErrorMessage srcspan $! text "Unclosed multiline comment opened here."

parseErrorMessage :: LToken -> ErrorMessage
parseErrorMessage tok = mkErrorMessage (locatedLoc tok) $
    text "Unexpected token" <+> prettyPrint tok

fileAccessErrorMessage :: FilePath -> ErrorMessage
fileAccessErrorMessage fp = mkErrorMessage Unknown $
    text "Could not open the file" <+> quotes (text fp)

ambiguousTypeAnnotationsError :: UnRenamedName -> [SrcSpan] -> ErrorMessage
ambiguousTypeAnnotationsError n spans = mkErrorMessage Unknown $
    hang (text "The variable" <+> prettyPrint n 
            <+> text "has multiple type annotations at" <> colon)
        tabWidth (vcat (map prettyPrint spans))

unusedTypeAnnotationsError :: UnRenamedName -> SrcSpan -> ErrorMessage
unusedTypeAnnotationsError n span = mkErrorMessage span $
    text "The type annotation for" <+> prettyPrint n <+> text "is unused."

unknownConstraintError :: String -> SrcSpan -> ErrorMessage
unknownConstraintError s loc = mkErrorMessage loc $
    text "The constraint" <+> text s <+> text "is unknown."

definitionSpanFileError :: PExp -> PExp -> SrcSpan -> ErrorMessage
definitionSpanFileError left right errLoc = mkErrorMessage errLoc $
    text "The definition:"
    $$ tabIndent (prettyPrint left <+> char '=' <+> prettyPrint right)
    $$ text "starts in the file" <+> bytestring (srcSpanFile (loc left))
    $$ text "but ends in the file" <+> bytestring (srcSpanFile (loc right))
        <> char '.'

looksLikeRTFErrorMessage :: FilePath -> ErrorMessage
looksLikeRTFErrorMessage fp = mkErrorMessage Unknown $
    text "The file" <+> quotes (text fp)
    $$ text "looks like a file in rich-text format (RTF). Only plain-text files are accepted."

duplicateModelOptionsError :: [PModelOption] -> ErrorMessage
duplicateModelOptionsError opts = mkErrorMessage (loc (head opts)) $
    text "The option:"
    <+> case unAnnotate (head opts) of
            TauPriority _ -> text ":[tau priority over]:"
            PartialOrderReduce _ -> text ":[partial order reduce]"
            SymmetryReduce {} -> text ":[symmetry reduce]:"
    <+> text "has been specified several times at:"
    $$ list (map (prettyPrint . loc) opts)

ambiguousChannelTypeError :: PDecl -> [UnRenamedName] -> [SrcSpan] -> ErrorMessage
ambiguousChannelTypeError chanDec anNs spans = mkErrorMessage (loc chanDec) $
    hang (text "The channel declaration for" <+> list (map prettyPrint anNs)
            $$ text "has multiple type annotations at" <> colon)
        tabWidth (vcat (map prettyPrint spans))

invalidSymmetrySpecification :: PExp -> ErrorMessage
invalidSymmetrySpecification symSpec = mkErrorMessage (loc symSpec) $
    text "The symmetry specification:"
    $$ tabIndent (prettyPrint symSpec)
    $$ text "is invalid. It must be of the format T, or diff(T, {| c_1, ..., c_n |})."
