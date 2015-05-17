module CSPM.Syntax.Helpers (
    patternAlwaysMatches,
    fieldBindsNoValues,
    fieldAlwaysMatches,
    fieldsAreIndependent
) where

import CSPM.Syntax.AST
import CSPM.Syntax.Names
import CSPM.Syntax.FreeVars
import qualified Data.Set as S
import Util.Annotated

-- | Returns true if the pattern is guaranteed to match any value
patternAlwaysMatches :: TCPat -> Bool
patternAlwaysMatches (An _ _ (PWildCard)) = True
patternAlwaysMatches (An _ _ (PVar n)) | isNameDataConstructor n = False
patternAlwaysMatches (An _ _ (PVar n)) = True
patternAlwaysMatches (An _ _ (PTuple ps)) = and (map patternAlwaysMatches ps)
patternAlwaysMatches (An _ _ (PDoublePattern p1 p2)) =
    patternAlwaysMatches p1 && patternAlwaysMatches p2
patternAlwaysMatches _ = False

-- | Returns true if the field binds no values.
fieldBindsNoValues :: TCField -> Bool
fieldBindsNoValues (An _ _ (Input (An _ _ PWildCard) _)) = True
fieldBindsNoValues (An _ _ (Output _)) = True
fieldBindsNoValues (An _ _ (NonDetInput (An _ _ PWildCard) _)) = True
fieldBindsNoValues _ = False

-- | True iff a field is guaranteed to match any proposed value. This is true
-- for fields such as ?x or !x, but not of ?0:... etc.
fieldAlwaysMatches :: TCField -> Bool
fieldAlwaysMatches (An _ _ (Input p _)) = patternAlwaysMatches p
fieldAlwaysMatches (An _ _ (Output _)) = True
fieldAlwaysMatches (An _ _ (NonDetInput p _)) = patternAlwaysMatches p

-- | True iff the variables bound by the field are not used by other fields
-- For example, it would be true on ?x?y, but false on ?x?y:f(x).
fieldsAreIndependent :: [TCField] -> Bool
fieldsAreIndependent fields =
        and (map (not . flip S.member fvs) (boundNames fields))
    where
        fvs = S.fromList (freeVars fields)
