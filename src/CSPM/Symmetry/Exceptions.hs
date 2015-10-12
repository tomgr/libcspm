{-# LANGUAGE FlexibleContexts #-}
module CSPM.Symmetry.Exceptions (
    explainNonSymmetryReason,
) where

import CSPM.PrettyPrinter
import CSPM.Symmetry.DependencyGraph
import CSPM.Symmetry.Monad
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

ppRecursive :: NonSymmetryReason -> Doc
ppRecursive (BuiltinNonSymmetric s) = text "which is not symmetric."
ppRecursive r =
    text "which is not symmetric because:"
    $$ prettyPrint r

instance PrettyPrintable NonSymmetryReason where
    prettyPrint (ExpContainsConstant (An loc _ e) n) =
        text "it contains the expression" <+> parens (prettyPrint loc)
        $$ tabIndent (prettyPrint e)
        $$ text "which is a constant of the type."
    prettyPrint (PatContainsConstant (An loc _ p) n) =
        text "it contains the pattern" <+> parens (prettyPrint loc)
        $$ tabIndent (prettyPrint p)
        $$ text "which is a constant of the type."
    prettyPrint (DependsOnNonSymmetricItem cc r) =
        (case cc of
            CallsName loc n -> text "it references" <+> prettyPrint n
                <+> parens (prettyPrint loc)
            ContainedWithinModule mn loc ->
                text "it is located within the module" <+> prettyPrint mn
                <+> parens (prettyPrint loc)
            ContainedWithinTimedSection loc ->
                text "it is located within a timed section"
                <+> parens (prettyPrint loc)
                )
        $$ ppRecursive r
    prettyPrint (BuiltinNonSymmetric n) =
        prettyPrint n <+> text "is a non-symmetric builtin function."
    prettyPrint (InstantiatesNonSymmetricPolymorphicItem (CallsName loc n) _ t r) =
        text "it uses the type" <+> prettyPrint t
        $$ text "in an argument to the polymorphic function:"
        $$ prettyPrint n <+> parens (prettyPrint loc)
        $$ ppRecursive r

instance PrettyPrintable SymmetryType where
    prettyPrint (FullySymmetric t) = prettyPrint t
    prettyPrint (PartiallySymmetric t cs) =
        text "diff" <> parens (prettyPrint t <> comma
            <+> (braces . bars . list . map prettyPrint) cs)

reasonLocation :: NonSymmetryReason -> SrcSpan
reasonLocation (ExpContainsConstant e n) = loc e
reasonLocation (PatContainsConstant p n) = loc p
reasonLocation (DependsOnNonSymmetricItem (CallsName loc _) _) = loc
reasonLocation (DependsOnNonSymmetricItem _ r) = reasonLocation r
reasonLocation (InstantiatesNonSymmetricPolymorphicItem (CallsName loc _) _ _ _) = loc
reasonLocation (InstantiatesNonSymmetricPolymorphicItem _ _ _ r) = reasonLocation r
reasonLocation (BuiltinNonSymmetric _) = BuiltIn

explainNonSymmetryReason :: PrettyPrintable (Annotated b a) =>
    Annotated b a -> String -> SymmetryType -> NonSymmetryReason -> ErrorMessage
explainNonSymmetryReason thing thingType requiredType r =
    mkErrorMessage location $
        text thingType
        $$ tabIndent (prettyPrint thing)
        $$ text "is not symmetric in" <+> prettyPrint requiredType <+> text "because:"
        $$ prettyPrint r
    where
        location =
            case reasonLocation r of
                BuiltIn -> loc thing
                l -> l
