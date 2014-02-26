module CSPM.HaskellEvaluator.TranslateDataType where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.HaskellEvaluator.Monad
import Util.MonadicPrettyPrint

registerDataTypes :: [TCDecl] -> TranslationMonad ()
translateDataType :: Name -> TranslationMonad Doc
translateDotApplication :: TCExp -> TranslationMonad Doc
translateDotPattern :: TCPat -> TranslationMonad (TranslationMonad Doc, [PatternSideCondition])
