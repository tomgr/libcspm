module CSPM.Evaluator.Exceptions
where

import Data.Typeable
import Prelude

import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.PrettyPrinter
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

patternMatchFailureMessage :: SrcSpan -> AnPat -> Value -> ErrorMessage
patternMatchFailureMessage l pat v =
	mkErrorMessage l $ 
		hang (hang (text "Pattern match failure: Value") tabWidth
				(prettyPrint v))
			tabWidth (text "does not match the pattern" <+> prettyPrint pat)

funBindPatternMatchFailureMessage :: SrcSpan -> Name -> [[Value]] -> ErrorMessage
funBindPatternMatchFailureMessage l n vss = mkErrorMessage l $
	hang (text "Pattern match failure whilst attempting to evaluate:") tabWidth
		(prettyPrint n <> 
			hcat (map (\ vs -> parens (list (map prettyPrint vs))) vss))
		
typeCheckerFailureMessage :: String -> ErrorMessage
typeCheckerFailureMessage s =
	mkErrorMessage Unknown $
		hang (text "The program caused a runtime error that should have been caught by the typechecker:")
			tabWidth (text s)
