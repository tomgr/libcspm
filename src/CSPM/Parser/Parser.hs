{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module CSPM.Parser.Parser (
    parseFile_, parseInteractiveStmt_, parseExpression_
) 
where

-- To generate the corresponding .hs file run the following commands
-- cpp -P src/CSPM/Parser/Parser.ppy > src/CSPM/Parser/Parser.y
-- happy --ghc --coerce --array src/CSPM/Parser/Parser.y
-- rm src/CSPM/Parser/Parser.y

-- i.e.: cpp -P src/CSPM/Parser/Parser.ppy > src/CSPM/Parser/Parser.y && happy --ghc --coerce --array src/CSPM/Parser/Parser.y && rm src/CSPM/Parser/Parser.y

import Data.Char
import Data.List (groupBy, nub, sort, sortBy, (\\))
import qualified Data.Map as M

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.DataStructures.Types hiding (TDot, TChar)
import CSPM.Parser.Exceptions
import CSPM.Parser.Lexer
import CSPM.Parser.Monad
import CSPM.Parser.Tokens
import Prelude hiding (getChar)
import Util.Annotated
import Util.Exception (panic)
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.10

newtype HappyAbsSyn t8 t9 t38 t39 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn6 :: (PCSPMFile) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t8 t9 t38 t39) -> (PCSPMFile)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (PInteractiveStmt) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn t8 t9 t38 t39) -> (PInteractiveStmt)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: t8 -> (HappyAbsSyn t8 t9 t38 t39)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t8 t9 t38 t39) -> t8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: t9 -> (HappyAbsSyn t8 t9 t38 t39)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t8 t9 t38 t39) -> t9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([PDecl]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PDecl])
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([PDecl]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PDecl])
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([PDecl]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PDecl])
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Maybe PDecl) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn t8 t9 t38 t39) -> (Maybe PDecl)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (PDecl) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn t8 t9 t38 t39) -> (PDecl)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (PDecl) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn t8 t9 t38 t39) -> (PDecl)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([PPat]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PPat])
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: ([PExp]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PExp])
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (PAssertion) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn t8 t9 t38 t39) -> (PAssertion)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (Located SemanticProperty) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn t8 t9 t38 t39) -> (Located SemanticProperty)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Located (ModelOption UnRenamedName)) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn t8 t9 t38 t39) -> (Located (ModelOption UnRenamedName))
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (PDataTypeClause) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn t8 t9 t38 t39) -> (PDataTypeClause)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ([PDataTypeClause]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PDataTypeClause])
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ([PDataTypeClause]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PDataTypeClause])
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Located UnRenamedName) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn t8 t9 t38 t39) -> (Located UnRenamedName)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Located UnRenamedName) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn t8 t9 t38 t39) -> (Located UnRenamedName)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: ([Located UnRenamedName]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn t8 t9 t38 t39) -> ([Located UnRenamedName])
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ([Located UnRenamedName]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn t8 t9 t38 t39) -> ([Located UnRenamedName])
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (PSTypeScheme) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn t8 t9 t38 t39) -> (PSTypeScheme)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: ([PSTypeConstraint]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PSTypeConstraint])
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ([PSTypeConstraint]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PSTypeConstraint])
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (PSType) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn t8 t9 t38 t39) -> (PSType)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (PSTypeConstraint) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn t8 t9 t38 t39) -> (PSTypeConstraint)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ([PSType]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PSType])
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ([PSType]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PSType])
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Located Literal) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn t8 t9 t38 t39) -> (Located Literal)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (PPat) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn t8 t9 t38 t39) -> (PPat)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: ([PPat]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PPat])
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: t38 -> (HappyAbsSyn t8 t9 t38 t39)
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn t8 t9 t38 t39) -> t38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: t39 -> (HappyAbsSyn t8 t9 t38 t39)
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn t8 t9 t38 t39) -> t39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (PExp) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn t8 t9 t38 t39) -> (PExp)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: ([PExp]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PExp])
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: ([PExp]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PExp])
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (PExp) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn t8 t9 t38 t39) -> (PExp)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (PExp) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn t8 t9 t38 t39) -> (PExp)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (PField) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn t8 t9 t38 t39) -> (PField)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: ((PExp, PExp)) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn t8 t9 t38 t39) -> ((PExp, PExp))
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: ([(PExp, PExp)]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn t8 t9 t38 t39) -> ([(PExp, PExp)])
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: ([(PExp, PExp)]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn t8 t9 t38 t39) -> ([(PExp, PExp)])
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: ((PExp, PExp)) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn t8 t9 t38 t39) -> ((PExp, PExp))
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: ([(PExp, PExp)]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn t8 t9 t38 t39) -> ([(PExp, PExp)])
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: ([(PExp, PExp)]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn t8 t9 t38 t39) -> ([(PExp, PExp)])
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: ([PField]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PField])
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: ([PField]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PField])
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: ([PExp]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PExp])
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: ([PExp]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PExp])
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: ([PExp]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PExp])
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: ([PExp]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PExp])
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: ([PStmt]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PStmt])
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: (PStmt) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn t8 t9 t38 t39) -> (PStmt)
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: ([PStmt]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PStmt])
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: ([PStmt]) -> (HappyAbsSyn t8 t9 t38 t39)
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn t8 t9 t38 t39) -> ([PStmt])
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyInTok :: (LToken) -> (HappyAbsSyn t8 t9 t38 t39)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t8 t9 t38 t39) -> (LToken)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x15\x01\x78\x02\x00\x00\x5d\x00\x00\x00\x00\x00\x78\x02\xb7\x01\x61\x08\x00\x00\x00\x00\x00\x00\x00\x00\xf3\x01\x00\x00\x00\x00\x00\x00\x78\x02\xb9\x00\x78\x02\x78\x02\x00\x00\x00\x00\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\xb3\x01\x00\x00\xb9\x00\x0c\x02\x0c\x02\x1c\x02\xb2\x01\x00\x00\x65\x01\x78\x02\x00\x00\x00\x00\x02\x02\x00\x00\x00\x00\xf0\x01\x03\x02\x00\x00\x00\x00\xd9\x01\xf7\x02\x0b\x00\xfa\x01\xfa\x01\xfa\x01\xfa\x01\xfa\x01\xfa\x01\xfa\x01\xfa\x01\xdf\xff\xf2\x01\xa7\x07\x08\x00\xc8\x04\x00\x00\xe8\x01\xf1\x01\xe7\x01\xe5\x01\xe0\x01\xd3\x01\xe6\x01\x00\x00\xdb\x01\xe3\x01\x69\x07\x61\x08\xef\xff\xe2\x01\x2b\x07\xf7\xff\x35\x03\x9e\x01\xc7\xff\x83\x00\xc3\x0b\xcc\x01\x61\x08\xa2\x02\xd6\x06\xdd\x01\x00\x00\x84\x01\x37\x01\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x81\x06\xf8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x78\x02\x00\x00\x96\x01\x73\x03\x2c\x06\xed\x0a\xed\x0a\xb0\x0a\x89\x0a\xed\x0a\x94\x09\x0f\x0a\x4c\x0a\xee\x05\xc6\x01\x73\x04\xc7\x01\x00\x00\xc3\x01\xca\x01\x89\x01\xd7\x00\xc7\xff\xc7\xff\xc7\xff\x83\x00\x83\x00\xb2\x0b\xb2\x0b\xb2\x0b\xb2\x0b\xb2\x0b\xb2\x0b\x90\x0b\xa1\x0b\x9e\x08\x23\x08\xd2\x09\x57\x09\x19\x09\xd6\x02\x00\x00\x78\x02\x00\x00\x78\x02\x78\x02\x78\x02\x00\x00\x78\x02\x00\x00\x78\x02\x00\x00\x71\x01\x78\x02\x78\x02\x00\x00\x78\x02\x78\x02\x7d\x01\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x7c\x01\x78\x02\x78\x02\x78\x02\x78\x02\x78\x02\x00\x00\x78\x02\x78\x02\xb9\x00\xc0\x01\x72\x01\xbf\x01\x00\x00\x00\x00\xbe\x01\xba\x01\xa6\x01\x78\x02\x82\x00\x01\x00\x00\x00\x78\x02\xb0\x01\x00\x00\x81\x01\x00\x00\x00\x00\x00\x00\x00\x00\x78\x02\x82\x01\x00\x00\x00\x00\xe5\x07\x00\x00\x00\x00\x00\x00\x85\x01\x00\x00\x10\x00\x80\x01\xa3\x01\x2e\x00\x62\x00\x2e\x00\x46\x01\x78\x02\x9f\x01\x9f\x01\x78\x02\x9d\x01\x78\x02\x9a\x01\x5c\x01\x54\x01\x00\x00\x7a\x01\xe5\x07\xe5\x07\xe5\x07\x00\x00\x78\x02\xe5\x07\xe5\x07\xe5\x07\x00\x00\x00\x00\x78\x02\x00\x00\x73\x01\x42\x01\xe5\x07\x1e\x04\x78\x02\x00\x00\x3b\x01\x79\x01\xe5\x07\x00\x00\xb0\x05\xed\x0a\x78\x02\x78\x02\x00\x00\x78\x02\x33\x01\x78\x02\x78\x02\x78\x02\x2c\x01\x78\x02\x78\x02\x78\x02\x00\x00\xc9\x03\x78\x02\x3d\x01\x00\x00\x3d\x01\x00\x00\x78\x02\x89\x0a\x4c\x0a\x5b\x05\x78\x02\xdb\x08\xdb\x08\x00\x00\x00\x00\x00\x00\xd2\x09\xd2\x09\x78\x02\x00\x00\x38\x01\x78\x02\x00\x00\x00\x00\x78\x02\x55\x01\x06\x05\x78\x02\x25\x01\x00\x00\x34\x01\x00\x00\x09\x00\xb9\x00\x00\x00\x00\x00\x00\x00\x5a\x01\x4a\x01\x00\x00\x00\x00\x04\x00\x44\x00\x23\x01\x5d\x01\x07\x00\x00\x00\x19\x01\xfe\x00\x02\x00\x00\x00\x2e\x00\x2e\x00\x2e\x00\x2e\x00\x2e\x00\x00\x00\x78\x02\x00\x00\x05\x00\x10\x00\x05\x00\x48\x01\x10\x00\x00\x00\x2e\x00\x00\x00\x2e\x00\x00\x00\x4b\x01\x0d\x01\x00\x00\x78\x02\x2a\x01\x09\x01\x78\x02\x00\x00\x00\x00\x78\x02\xb9\x00\xe5\x07\x78\x02\x78\x02\xe5\x07\xf4\x00\x00\x00\xe5\x07\xdb\x08\x78\x02\xed\x00\x00\x00\x00\x00\xdb\x08\x00\x00\xe5\x07\xdb\x08\xd5\x00\x00\x00\xd3\x00\x18\x01\x00\x00\x0c\x01\x00\x00\x00\x00\x2e\x00\x00\x00\x0b\x01\x10\x00\xf2\x00\x10\x00\x2e\x00\x10\x00\xb9\x00\x78\x02\x00\x00\x00\x00\x00\x00\xea\x00\x10\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x69\x00\xcb\x00\x1d\x09\xf7\x00\x88\x0b\x00\x00\x00\x00\x71\x04\x00\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x55\x0d\x9e\x0b\xe4\x08\x49\x0d\x00\x00\x00\x00\x3f\x0d\x3d\x0d\xe0\x00\x1c\x04\xc7\x03\x33\x0d\xd5\x06\x20\x0b\xe3\x0a\x7f\x0a\x42\x0a\x1d\x07\x27\x0d\x00\x00\x00\x00\xd4\x04\x27\x01\x1d\x01\x80\x04\x00\x00\x00\x00\xe6\x00\x2b\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\x00\x00\x00\x00\x00\x00\x00\xb8\x00\x00\x00\x11\x01\xe5\x00\xe4\x00\xae\x00\x8c\x00\xdc\x00\xdb\x00\xd2\x00\x00\x00\xd6\x03\xb8\x00\x00\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xad\x00\x00\x00\xb8\x00\xb8\x00\x00\x00\x00\x00\xb8\x00\x00\x00\xb8\x00\x00\x00\xb8\x00\xb8\x00\xb8\x00\x00\x00\xb8\x00\xb8\x00\xb8\x00\xc2\x00\x00\x00\x00\x00\xab\x00\x23\x0d\x17\x0d\x0d\x0d\x0b\x0d\x01\x0d\xf5\x0c\xf1\x0c\xe5\x0c\xdb\x0c\xd9\x0c\xcf\x0c\xc3\x0c\xbf\x0c\xb3\x0c\xa9\x0c\xa7\x0c\x9d\x0c\x91\x0c\x8d\x0c\x81\x0c\x2d\x02\x7c\x03\x77\x0c\x59\x05\x75\x0c\x6b\x0c\x5f\x0c\x5b\x0c\x4f\x0c\x45\x0c\x43\x0c\x39\x0c\x2d\x0c\x29\x0c\xb8\x00\xaf\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\x06\x00\x00\xba\x05\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\x97\x00\xb8\x00\x00\x00\x00\x00\x81\x00\x00\x00\x00\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\xb8\x00\x00\x00\x1d\x0c\x00\x00\x13\x0c\xa7\x08\x11\x0c\x00\x00\x59\x01\x00\x00\x73\x06\x00\x00\x07\x0c\xfa\x0b\x4c\x05\x00\x00\x64\x04\x3e\x03\x00\x00\x0f\x04\x6a\x08\xf8\x0b\xf6\x0b\x8f\x0b\x00\x00\x71\x00\x79\x0b\x63\x0b\x4d\x0b\xba\x03\x00\x00\x82\x02\x2c\x08\x72\x0b\x00\x00\xa4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xee\x07\x55\x00\x8e\x06\x00\x00\xf0\x0a\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x07\x8e\x00\x00\x00\x00\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7a\x00\xf1\x00\x32\x01\xef\x00\x00\x00\x72\x07\xfe\x01\xad\x01\x39\x07\x00\x00\x75\x00\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb8\x00\xb8\x00\xb8\x00\x00\x00\xbc\x0a\xb8\x00\xb8\x00\xb8\x00\x00\x00\x00\x00\x65\x02\x00\x00\x00\x00\x00\x00\xb8\x00\xb8\x00\xdf\x01\x00\x00\x00\x00\x00\x00\xb8\x00\x00\x00\xb8\x00\xb8\x00\x8c\x0a\x19\x0a\x00\x00\x3c\x01\x00\x00\x2a\x07\x15\x0a\xd6\x09\x00\x00\x9e\x09\x9a\x09\x60\x09\x00\x00\x14\x00\x6a\x01\x57\x00\x00\x00\x52\x00\x00\x00\x21\x01\xb8\x00\xb8\x00\xb8\x00\x5c\x09\xb8\x00\xb8\x00\x00\x00\x00\x00\x00\x00\xb8\x00\xb8\x00\x6e\x08\x00\x00\x00\x00\x02\x01\x00\x00\x00\x00\x30\x08\x00\x00\xb8\x00\xb4\x07\x00\x00\x00\x00\x4b\x00\x00\x00\x00\x00\x5c\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\x00\xd0\x00\xc9\x00\x9f\x00\x3f\x00\x00\x00\x39\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x00\x00\x54\x00\x00\x00\x2b\x00\x00\x00\x00\x00\xf7\x05\x72\x00\x00\x00\xcd\x04\x00\x00\x00\x00\x0f\x05\x46\x0b\x85\x00\x76\x07\xfb\x05\x85\x00\x00\x00\x00\x00\x85\x00\x85\x00\x42\x03\xfb\xff\x00\x00\x00\x00\x85\x00\x00\x00\x85\x00\x85\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\xbc\x05\xbd\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xf3\xff\x00\x00\x00\x00\x00\x00\xf1\xff\x9f\xff\xa0\xff\x4a\xff\x00\x00\xa5\xff\xa1\xff\xaf\xff\xae\xff\xad\xff\xc9\xff\xab\xff\xac\xff\x77\xff\x00\x00\x00\x00\x00\x00\x00\x00\x88\xff\xa7\xff\x00\x00\x00\x00\x00\x00\x4a\xff\x4a\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\x00\x00\xc5\xff\xf7\xff\xc6\xff\xc7\xff\xf8\xff\x00\x00\xef\xff\xee\xff\xea\xff\x00\x00\x00\x00\xc9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\xff\x3f\xff\x00\x00\x41\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x51\xff\x46\xff\x52\xff\x00\x00\x47\xff\x00\x00\x49\xff\x47\xff\x00\x00\x00\x00\x00\x00\x80\xff\x8e\xff\x91\xff\x00\x00\xaa\xff\x00\x00\x00\x00\x00\x00\x4e\xff\x00\x00\x4f\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\xff\x00\x00\xfc\xff\xf0\xff\xf2\xff\x87\xff\x00\x00\xa6\xff\x00\x00\x00\x00\x00\x00\x69\xff\x6a\xff\x6b\xff\x6d\xff\x74\xff\x70\xff\x71\xff\x73\xff\xa5\xff\x46\xff\x00\x00\x00\x00\x55\xff\x46\xff\x56\xff\x00\x00\x81\xff\x8b\xff\x8a\xff\x89\xff\x8c\xff\x8d\xff\x94\xff\x95\xff\x92\xff\x93\xff\x96\xff\x97\xff\x90\xff\x8f\xff\x72\xff\x76\xff\x58\xff\x5a\xff\x5c\xff\x9d\xff\x4d\xff\x00\x00\xc8\xff\x00\x00\x00\x00\x00\x00\x9e\xff\x00\x00\x9a\xff\x00\x00\x7f\xff\x00\x00\x00\x00\x00\x00\x79\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\x00\x00\x00\x00\xf1\xff\x00\x00\xda\xff\x00\x00\xe3\xff\xe4\xff\x00\x00\x00\x00\xe8\xff\x00\x00\x00\x00\xec\xff\xf5\xff\x00\x00\x00\x00\xfa\xff\xd5\xff\xd3\xff\xd2\xff\xd1\xff\xd0\xff\x00\x00\xd7\xff\xd4\xff\xc4\xff\x9c\xff\xed\xff\xeb\xff\xf4\xff\xbd\xff\xdc\xff\xc3\xff\x00\x00\xc7\xff\x00\x00\x00\x00\x00\x00\xe9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe2\xff\x00\x00\x44\xff\x43\xff\x5e\xff\x40\xff\x00\x00\x64\xff\x62\xff\x63\xff\x53\xff\x45\xff\x00\x00\x50\xff\x00\x00\x00\x00\x48\xff\x00\x00\x00\x00\x7d\xff\x00\x00\x4c\xff\x98\xff\xe9\xff\x00\x00\x75\xff\x00\x00\x00\x00\x99\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x85\xff\x00\x00\x00\x00\x00\x00\x86\xff\x00\x00\x84\xff\x00\x00\x67\xff\x68\xff\x00\x00\x00\x00\x6c\xff\x6f\xff\x57\xff\x66\xff\x54\xff\x59\xff\x5b\xff\x00\x00\x7e\xff\x00\x00\x00\x00\x7c\xff\x78\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\x00\x00\xa9\xff\x00\x00\xf1\xff\xe0\xff\xcb\xff\xe5\xff\xcc\xff\xcd\xff\xe6\xff\xe7\xff\x00\x00\x00\x00\x00\x00\xc0\xff\x00\x00\xbe\xff\x00\x00\x00\x00\x00\x00\xb3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\x00\x00\xcf\xff\xb9\xff\xb4\xff\xb8\xff\xb7\xff\xc2\xff\xbb\xff\x00\x00\xb6\xff\x00\x00\xba\xff\x00\x00\x00\x00\xbc\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd9\xff\xdb\xff\x00\x00\xf1\xff\x5d\xff\x00\x00\x00\x00\x60\xff\x00\x00\x7b\xff\x9b\xff\x65\xff\x00\x00\x00\x00\x83\xff\x82\xff\x6e\xff\x7a\xff\x5f\xff\x61\xff\x00\x00\xa3\xff\x00\x00\xa4\xff\xa8\xff\x00\x00\xca\xff\xce\xff\x00\x00\xbf\xff\x00\x00\xb0\xff\xb2\xff\xb5\xff\x00\x00\xc1\xff\xf1\xff\x00\x00\xd8\xff\xdd\xff\xa2\xff\x00\x00\xb1\xff\xdf\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x12\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x12\x00\x12\x00\x44\x00\x0a\x00\x46\x00\x0a\x00\x0a\x00\x09\x00\x0a\x00\x09\x00\x4c\x00\x09\x00\x14\x00\x15\x00\x0f\x00\x10\x00\x18\x00\x0a\x00\x1a\x00\x21\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x13\x00\x46\x00\x25\x00\x48\x00\x27\x00\x13\x00\x19\x00\x29\x00\x2b\x00\x29\x00\x29\x00\x19\x00\x29\x00\x26\x00\x04\x00\x28\x00\x33\x00\x21\x00\x3e\x00\x13\x00\x13\x00\x29\x00\x4b\x00\x27\x00\x3b\x00\x3c\x00\x19\x00\x3d\x00\x49\x00\x40\x00\x2e\x00\x2f\x00\x13\x00\x1a\x00\x45\x00\x46\x00\x04\x00\x48\x00\x19\x00\x4a\x00\x1b\x00\x49\x00\x47\x00\x4e\x00\x47\x00\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x0b\x00\x55\x00\x19\x00\x55\x00\x59\x00\x5a\x00\x55\x00\x5c\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x55\x00\x04\x00\x13\x00\x13\x00\x00\x00\x3c\x00\x16\x00\x03\x00\x19\x00\x19\x00\x1a\x00\x1c\x00\x14\x00\x15\x00\x21\x00\x46\x00\x18\x00\x48\x00\x1a\x00\x21\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x3c\x00\x0f\x00\x25\x00\x12\x00\x27\x00\x13\x00\x04\x00\x12\x00\x2b\x00\x13\x00\x46\x00\x47\x00\x48\x00\x13\x00\x1d\x00\x1e\x00\x33\x00\x20\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x25\x00\x26\x00\x3b\x00\x3c\x00\x25\x00\x26\x00\x0e\x00\x40\x00\x3c\x00\x13\x00\x14\x00\x15\x00\x45\x00\x46\x00\x13\x00\x48\x00\x35\x00\x4a\x00\x46\x00\x47\x00\x48\x00\x4e\x00\x27\x00\x50\x00\x0a\x00\x52\x00\x53\x00\x54\x00\x13\x00\x2e\x00\x2f\x00\x34\x00\x59\x00\x5a\x00\x19\x00\x5c\x00\x01\x00\x02\x00\x03\x00\x04\x00\x3c\x00\x06\x00\x07\x00\x13\x00\x14\x00\x15\x00\x41\x00\x42\x00\x43\x00\x44\x00\x46\x00\x46\x00\x48\x00\x34\x00\x01\x00\x14\x00\x15\x00\x4c\x00\x21\x00\x18\x00\x27\x00\x1a\x00\x12\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x13\x00\x12\x00\x25\x00\x27\x00\x27\x00\x34\x00\x19\x00\x13\x00\x2b\x00\x13\x00\x2e\x00\x2f\x00\x1d\x00\x19\x00\x02\x00\x20\x00\x33\x00\x22\x00\x13\x00\x13\x00\x25\x00\x26\x00\x12\x00\x0d\x00\x3b\x00\x3c\x00\x13\x00\x13\x00\x13\x00\x40\x00\x03\x00\x09\x00\x19\x00\x1d\x00\x45\x00\x46\x00\x20\x00\x48\x00\x13\x00\x4a\x00\x13\x00\x25\x00\x26\x00\x4e\x00\x19\x00\x50\x00\x19\x00\x52\x00\x53\x00\x54\x00\x24\x00\x04\x00\x30\x00\x05\x00\x59\x00\x5a\x00\x12\x00\x5c\x00\x01\x00\x02\x00\x03\x00\x04\x00\x47\x00\x06\x00\x07\x00\x46\x00\x49\x00\x1d\x00\x1e\x00\x09\x00\x20\x00\x4c\x00\x13\x00\x14\x00\x15\x00\x25\x00\x26\x00\x14\x00\x15\x00\x3e\x00\x23\x00\x18\x00\x04\x00\x1a\x00\x13\x00\x14\x00\x15\x00\x12\x00\x1f\x00\x20\x00\x29\x00\x35\x00\x36\x00\x37\x00\x13\x00\x14\x00\x15\x00\x49\x00\x1d\x00\x1e\x00\x2b\x00\x20\x00\x0b\x00\x0c\x00\x0d\x00\x13\x00\x25\x00\x26\x00\x33\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x12\x00\x04\x00\x3b\x00\x3c\x00\x0a\x00\x55\x00\x0a\x00\x40\x00\x35\x00\x36\x00\x37\x00\x1d\x00\x45\x00\x46\x00\x20\x00\x48\x00\x22\x00\x4a\x00\x47\x00\x25\x00\x26\x00\x4e\x00\x28\x00\x50\x00\x09\x00\x52\x00\x53\x00\x54\x00\x47\x00\x12\x00\x12\x00\x48\x00\x59\x00\x5a\x00\x1b\x00\x5c\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1d\x00\x06\x00\x07\x00\x20\x00\x46\x00\x3e\x00\x12\x00\x51\x00\x25\x00\x26\x00\x4d\x00\x49\x00\x09\x00\x12\x00\x49\x00\x14\x00\x15\x00\x1d\x00\x1e\x00\x18\x00\x20\x00\x1a\x00\x33\x00\x4b\x00\x1b\x00\x25\x00\x26\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x1b\x00\x31\x00\x01\x00\x02\x00\x03\x00\x04\x00\x47\x00\x06\x00\x07\x00\x04\x00\x35\x00\x36\x00\x37\x00\x05\x00\x04\x00\x33\x00\x49\x00\x60\x00\x04\x00\x12\x00\x29\x00\x14\x00\x15\x00\x3b\x00\x3c\x00\x18\x00\x2a\x00\x1a\x00\x40\x00\x30\x00\x32\x00\x04\x00\x0f\x00\x45\x00\x46\x00\x46\x00\x48\x00\x49\x00\x4a\x00\x0f\x00\x10\x00\x11\x00\x4e\x00\x13\x00\x50\x00\x08\x00\x52\x00\x53\x00\x54\x00\x08\x00\x08\x00\x08\x00\x33\x00\x59\x00\x5a\x00\x50\x00\x5c\x00\x51\x00\x12\x00\x47\x00\x3b\x00\x3c\x00\x09\x00\x3e\x00\x12\x00\x40\x00\x10\x00\x12\x00\x55\x00\x1d\x00\x45\x00\x46\x00\x20\x00\x48\x00\x22\x00\x4a\x00\x04\x00\x25\x00\x26\x00\x4e\x00\x47\x00\x50\x00\x1b\x00\x52\x00\x53\x00\x54\x00\x09\x00\x09\x00\x12\x00\x1b\x00\x59\x00\x5a\x00\x12\x00\x5c\x00\x01\x00\x02\x00\x03\x00\x04\x00\x11\x00\x06\x00\x07\x00\x09\x00\x1b\x00\x1d\x00\x1e\x00\x04\x00\x20\x00\x1b\x00\x28\x00\x1b\x00\x1b\x00\x25\x00\x26\x00\x14\x00\x15\x00\x05\x00\x19\x00\x18\x00\x09\x00\x1a\x00\x0f\x00\x10\x00\x11\x00\x04\x00\x13\x00\x60\x00\x60\x00\x35\x00\x36\x00\x37\x00\x60\x00\xff\xff\x26\x00\xff\xff\xff\xff\xff\xff\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x06\x00\x07\x00\xff\xff\x33\x00\x34\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3b\x00\x3c\x00\xff\xff\x14\x00\x15\x00\x40\x00\xff\xff\x18\x00\xff\xff\x1a\x00\x45\x00\x46\x00\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\xff\xff\x12\x00\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x59\x00\x5a\x00\x20\x00\x5c\x00\x33\x00\x34\x00\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\x3b\x00\x3c\x00\xff\xff\xff\xff\xff\xff\x40\x00\xff\xff\xff\xff\x32\x00\x33\x00\x45\x00\x46\x00\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\xff\xff\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\xff\xff\xff\xff\xff\xff\xff\xff\x59\x00\x5a\x00\x12\x00\x5c\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x06\x00\x07\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\x14\x00\x15\x00\x0c\x00\xff\xff\x18\x00\xff\xff\x1a\x00\xff\xff\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x08\x00\x33\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x3b\x00\x3c\x00\x13\x00\xff\xff\xff\xff\x40\x00\xff\xff\xff\xff\xff\xff\x1a\x00\x45\x00\x46\x00\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\xff\xff\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\xff\xff\xff\xff\xff\xff\xff\xff\x59\x00\x5a\x00\xff\xff\x5c\x00\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\x0a\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\xff\xff\xff\xff\x5a\x00\x5b\x00\x5c\x00\x08\x00\x5e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x13\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x1a\x00\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x1a\x00\x12\x00\x5a\x00\x5b\x00\x5c\x00\x12\x00\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x22\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\x2b\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\x47\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x1a\x00\x12\x00\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x28\x00\x29\x00\x2a\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\xff\xff\x12\x00\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\x5f\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1d\x00\x1e\x00\x12\x00\x20\x00\x12\x00\x13\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x0c\x00\x1a\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x12\x00\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\x32\x00\x33\x00\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x12\x00\xff\xff\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1d\x00\x1e\x00\x12\x00\x20\x00\x12\x00\x13\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x0c\x00\x1a\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x12\x00\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\x32\x00\x33\x00\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\x49\x00\xff\xff\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x12\x00\xff\xff\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1d\x00\x1e\x00\x12\x00\x20\x00\xff\xff\x13\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x0c\x00\x1a\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x12\x00\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\x32\x00\x33\x00\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\xff\xff\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x05\x00\x06\x00\x13\x00\x08\x00\x09\x00\xff\xff\x12\x00\xff\xff\xff\xff\x1a\x00\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\x1d\x00\x1e\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x25\x00\x26\x00\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x1a\x00\x12\x00\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x12\x00\xff\xff\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1d\x00\x1e\x00\x12\x00\x20\x00\xff\xff\x13\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\x1a\x00\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x2b\x00\x2c\x00\x2d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\xff\xff\xff\xff\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x13\x00\x08\x00\x09\x00\xff\xff\x17\x00\xff\xff\xff\xff\x1a\x00\xff\xff\x12\x00\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\x1d\x00\x20\x00\x21\x00\x20\x00\xff\xff\xff\xff\x25\x00\x26\x00\x25\x00\x26\x00\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x1a\x00\x12\x00\x5a\x00\x5b\x00\x5c\x00\x12\x00\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x22\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x1a\x00\x59\x00\x5a\x00\x5b\x00\x5c\x00\x12\x00\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\xff\xff\x12\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x1d\x00\x1e\x00\xff\xff\x20\x00\x13\x00\x07\x00\x08\x00\x09\x00\x25\x00\x26\x00\xff\xff\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\xff\xff\x12\x00\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x1d\x00\x1e\x00\x12\x00\x20\x00\x13\x00\xff\xff\xff\xff\x16\x00\x25\x00\x26\x00\xff\xff\x1a\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x2b\x00\x2c\x00\x2d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\xff\xff\x12\x00\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x1d\x00\x1e\x00\x12\x00\x20\x00\x13\x00\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x1a\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x12\x00\x22\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\xff\xff\xff\xff\x25\x00\x26\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x1a\x00\x12\x00\x5a\x00\x5b\x00\x5c\x00\x12\x00\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x22\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x1a\x00\x12\x00\x5a\x00\x5b\x00\x5c\x00\x12\x00\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x22\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x1a\x00\x12\x00\x5a\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x1a\x00\x12\x00\x5a\x00\x5b\x00\x5c\x00\x12\x00\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x22\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x1a\x00\x12\x00\x5a\x00\x5b\x00\x5c\x00\x12\x00\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x22\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x12\x00\xff\xff\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x12\x00\xff\xff\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\x1e\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x0a\x00\xff\xff\xff\xff\xff\xff\x4c\x00\x0f\x00\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x12\x00\x55\x00\x56\x00\x57\x00\x1a\x00\xff\xff\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x0a\x00\xff\xff\xff\xff\xff\xff\x4c\x00\x0f\x00\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x54\x00\x12\x00\x56\x00\x57\x00\x1a\x00\x12\x00\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x12\x00\x56\x00\x57\x00\xff\xff\x12\x00\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x0a\x00\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\x13\x00\x52\x00\x53\x00\x12\x00\x55\x00\x56\x00\x57\x00\x1a\x00\xff\xff\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\xff\xff\x50\x00\xff\xff\x52\x00\x53\x00\x54\x00\x12\x00\x56\x00\x57\x00\xff\xff\x12\x00\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\xff\xff\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x12\x00\x46\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x4c\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x1e\x00\x52\x00\x20\x00\xff\xff\x55\x00\x56\x00\x57\x00\x25\x00\x26\x00\x5a\x00\x5b\x00\x5c\x00\xff\xff\x5e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x12\x00\x46\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x4c\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x1e\x00\x12\x00\x20\x00\xff\xff\x55\x00\x56\x00\x57\x00\x25\x00\x26\x00\x5a\x00\x5b\x00\xff\xff\x1d\x00\x5e\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x12\x00\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x55\x00\xff\xff\x57\x00\x25\x00\x26\x00\x5a\x00\x5b\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x12\x00\x46\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x4c\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x1e\x00\x12\x00\x20\x00\xff\xff\x55\x00\xff\xff\xff\xff\x25\x00\x26\x00\x5a\x00\x5b\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x35\x00\x36\x00\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x12\x00\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x1e\x00\xff\xff\x20\x00\xff\xff\x55\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x5b\x00\xff\xff\x04\x00\x05\x00\x06\x00\xff\xff\x08\x00\x09\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x36\x00\x37\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\x12\x00\x04\x00\x05\x00\x06\x00\x1d\x00\x08\x00\x09\x00\x20\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x25\x00\x26\x00\x20\x00\x12\x00\x13\x00\x14\x00\x15\x00\x25\x00\x26\x00\xff\xff\x12\x00\x04\x00\x05\x00\x06\x00\x1d\x00\x08\x00\x09\x00\x20\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x25\x00\x26\x00\x20\x00\x12\x00\x13\x00\x14\x00\x15\x00\x25\x00\x26\x00\xff\xff\x12\x00\x04\x00\x05\x00\x06\x00\x1d\x00\x08\x00\x09\x00\x20\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x25\x00\x26\x00\x20\x00\x12\x00\x13\x00\x14\x00\x15\x00\x25\x00\x26\x00\xff\xff\x12\x00\xff\xff\x05\x00\x06\x00\x1d\x00\x08\x00\x09\x00\x20\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x25\x00\x26\x00\x20\x00\x12\x00\x13\x00\x14\x00\x15\x00\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\x35\x00\xff\xff\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x37\x00\x38\x00\x39\x00\x3a\x00\x4c\x00\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x37\x00\x38\x00\x39\x00\x3a\x00\x4c\x00\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x46\x00\xff\xff\x37\x00\x38\x00\x39\x00\x3a\x00\x4c\x00\x3c\x00\x3d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x12\x00\x46\x00\x12\x00\xff\xff\x12\x00\xff\xff\xff\xff\x4c\x00\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\x1d\x00\x20\x00\x1d\x00\x20\x00\x12\x00\x20\x00\x25\x00\x26\x00\x25\x00\x26\x00\x25\x00\x26\x00\xff\xff\xff\xff\x12\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\x1d\x00\x12\x00\x1d\x00\x20\x00\xff\xff\x20\x00\xff\xff\xff\xff\x25\x00\x26\x00\x25\x00\x26\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\x12\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x12\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x12\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\x1d\x00\x12\x00\x1d\x00\x20\x00\xff\xff\x20\x00\xff\xff\xff\xff\x25\x00\x26\x00\x25\x00\x26\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\x12\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x12\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x12\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\x1d\x00\x12\x00\x1d\x00\x20\x00\xff\xff\x20\x00\xff\xff\xff\xff\x25\x00\x26\x00\x25\x00\x26\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\x12\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x12\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x12\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\x1d\x00\x12\x00\x1d\x00\x20\x00\xff\xff\x20\x00\xff\xff\xff\xff\x25\x00\x26\x00\x25\x00\x26\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\x12\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x12\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x12\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\x1d\x00\x12\x00\x1d\x00\x20\x00\xff\xff\x20\x00\xff\xff\xff\xff\x25\x00\x26\x00\x25\x00\x26\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\x12\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x12\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x12\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\x1d\x00\x12\x00\x1d\x00\x20\x00\xff\xff\x20\x00\xff\xff\xff\xff\x25\x00\x26\x00\x25\x00\x26\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\x12\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\x1d\x00\x12\x00\xff\xff\x20\x00\x25\x00\x26\x00\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\x12\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\x1d\x00\x12\x00\x1d\x00\x20\x00\xff\xff\x20\x00\xff\xff\xff\xff\x25\x00\x26\x00\x25\x00\x26\x00\x1d\x00\x12\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\xc5\x00\x0c\x00\x0d\x00\x0e\x00\x3b\x00\xf4\x00\x10\x00\x11\x00\xc1\x00\x8f\x00\x7a\x00\x62\x01\x7b\x00\x62\x01\x62\x01\x71\x01\x62\x01\x79\x01\x7c\x00\xc7\xff\x12\x00\x13\x00\xd2\x00\xd3\x00\x14\x00\x62\x01\x15\x00\x88\x01\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\xf4\x00\xd7\x00\x43\x00\xd8\x00\x44\x00\xf4\x00\xa3\x01\x63\x01\x45\x00\x63\x01\x63\x01\x9c\x01\x63\x01\x63\x00\x33\x00\xc7\xff\x16\x00\x32\x01\x90\x00\x5f\x01\xf4\x00\x63\x01\xc6\x00\x63\x00\x17\x00\x18\x00\x9a\x01\x6e\x01\xc2\x00\x19\x00\x64\x00\x65\x00\xf4\x00\x96\x01\x1a\x00\x1b\x00\x33\x00\x1c\x00\x5a\x01\x1d\x00\x5c\x01\x75\x01\x72\x01\x1e\x00\x7a\x01\x1f\x00\xf4\x00\x20\x00\x21\x00\x22\x00\x7a\x01\x64\x01\x68\x01\x64\x01\x23\x00\x24\x00\x64\x01\x25\x00\x0c\x00\x0d\x00\x0e\x00\x3b\x00\x8d\x00\x10\x00\x11\x00\x64\x01\xf9\x00\xf4\x00\xf4\x00\x2b\x00\xfa\x00\xf5\x00\x04\x00\x98\x01\xf6\x00\xf7\x00\x99\x01\x12\x00\x13\x00\x87\x01\x58\x01\x14\x00\xfc\x00\x15\x00\x30\x01\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\xfa\x00\x93\x01\x43\x00\x05\x00\x44\x00\x53\x01\xf9\x00\x05\x00\x45\x00\x4b\x01\x58\x01\x5e\x01\xfc\x00\x5f\x01\x06\x00\x46\x00\x16\x00\x07\x00\x06\x00\x4c\x01\x4d\x01\x07\x00\x47\x00\x0a\x00\x17\x00\x18\x00\x5f\x00\x0a\x00\x65\x01\x19\x00\xfa\x00\x2f\x00\xdb\x00\x31\x00\x1a\x00\x1b\x00\xef\x00\x1c\x00\x0b\x01\x1d\x00\x58\x01\x5e\x01\xfc\x00\x1e\x00\x63\x00\x1f\x00\x01\x01\x20\x00\x21\x00\x22\x00\xf4\x00\x64\x00\x65\x00\x24\x01\x23\x00\x24\x00\x69\x01\x25\x00\x0c\x00\x0d\x00\x0e\x00\x3b\x00\xfa\x00\x10\x00\x11\x00\x2f\x00\xdc\x00\x31\x00\x77\x00\x78\x00\x79\x00\x7a\x00\xfb\x00\x7b\x00\xfc\x00\x28\x01\x25\x00\x12\x00\x13\x00\x7c\x00\x8d\x00\x14\x00\xb7\x00\x15\x00\xb9\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\xf4\x00\x05\x00\x43\x00\x63\x00\x44\x00\xc8\x00\x6a\x01\xf4\x00\x45\x00\xd8\x00\x64\x00\x65\x00\x06\x00\x6b\x01\xe2\x00\x07\x00\x16\x00\x26\x00\xd9\x00\xda\x00\x09\x00\x0a\x00\x05\x00\xe7\x00\x17\x00\x18\x00\xf4\x00\xdd\x00\xde\x00\x19\x00\x04\x00\x9c\x01\x6c\x01\x06\x00\x1a\x00\x1b\x00\x07\x00\x1c\x00\xf4\x00\x1d\x00\xf4\x00\x59\x00\x0a\x00\x1e\x00\x56\x01\x1f\x00\x5e\x01\x20\x00\x21\x00\x22\x00\xa5\x01\x33\x00\x5a\x00\x9e\x01\x23\x00\x24\x00\x05\x00\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\xa0\x01\x10\x00\x11\x00\x7b\x00\xa1\x01\x06\x00\x46\x00\x9f\x01\x07\x00\x7c\x00\x2f\x00\xdf\x00\x31\x00\x47\x00\x0a\x00\x12\x00\x13\x00\x90\x00\x93\x01\x28\x00\x33\x00\x15\x00\x2f\x00\x30\x00\x31\x00\x05\x00\x29\x00\x2a\x00\x96\x01\x48\x00\x81\x01\x4a\x00\x2f\x00\x33\x00\x31\x00\x8b\x01\x06\x00\x46\x00\x2b\x00\x07\x00\x68\x00\x69\x00\x6a\x00\xf4\x00\x47\x00\x0a\x00\x16\x00\x58\x01\x59\x01\x5a\x01\x5b\x01\x5c\x01\x05\x00\x98\x01\x17\x00\x18\x00\x62\x01\x6f\x01\x76\x01\x19\x00\x48\x00\x86\x01\x4a\x00\x06\x00\x1a\x00\x1b\x00\x07\x00\x1c\x00\x9e\x00\x1d\x00\x70\x01\x09\x00\x0a\x00\x1e\x00\x3c\x01\x1f\x00\x73\x01\x20\x00\x21\x00\x22\x00\x74\x01\x05\x00\x77\x01\x7d\x01\x23\x00\x24\x00\x80\x01\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x06\x00\x10\x00\x11\x00\x07\x00\x7c\x01\x90\x00\x05\x00\x38\x01\x54\x00\x0a\x00\x3c\x01\x83\x01\xc4\x00\x19\x01\x41\x01\x12\x00\x13\x00\x06\x00\x46\x00\x14\x00\x07\x00\x15\x00\x1b\x01\x45\x01\x46\x01\x47\x00\x0a\x00\xe9\x00\xea\x00\xeb\x00\xec\x00\x49\x01\xed\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x4a\x01\x10\x00\x11\x00\x33\x00\x48\x00\x31\x01\x4a\x00\x4f\x01\x33\x00\x16\x00\x4b\x01\xfb\xff\x33\x00\x2f\x01\x61\x01\x12\x00\x13\x00\x17\x00\x18\x00\x14\x00\x65\x01\x15\x00\x19\x00\x67\x01\xef\x00\x33\x00\xfe\x00\x1a\x00\x1b\x00\x03\x01\x1c\x00\x1a\x01\x1d\x00\x50\x01\x51\x01\x52\x01\x1e\x00\x53\x01\x1f\x00\xff\x00\x20\x00\x21\x00\x22\x00\x00\x01\x01\x01\x04\x01\x16\x00\x23\x00\x24\x00\x0d\x01\x25\x00\x13\x01\x05\x00\x23\x01\x17\x00\x18\x00\x24\x01\x90\x00\xca\x00\x19\x00\x26\x01\xca\x00\xb9\x00\x06\x00\x1a\x00\x1b\x00\x07\x00\x1c\x00\xa1\x01\x1d\x00\x0f\x00\x09\x00\x0a\x00\x1e\x00\xbe\x00\x1f\x00\xbd\x00\x20\x00\x21\x00\x22\x00\xc4\x00\xc8\x00\xca\x00\xcc\x00\x23\x00\x24\x00\x05\x00\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\xcb\x00\x10\x00\x11\x00\xd0\x00\xcd\x00\x06\x00\x46\x00\x33\x00\x07\x00\xce\x00\xe2\x00\xcf\x00\xd1\x00\x47\x00\x0a\x00\x12\x00\x13\x00\xe4\x00\xe5\x00\x14\x00\xe6\x00\x15\x00\x50\x01\x54\x01\x52\x01\x33\x00\x53\x01\xff\xff\xff\xff\x48\x00\x41\x01\x4a\x00\xff\xff\x00\x00\x63\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x00\x00\x10\x00\x11\x00\x00\x00\x16\x00\xd6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x00\x18\x00\x00\x00\x12\x00\x13\x00\x19\x00\x00\x00\x14\x00\x00\x00\x15\x00\x1a\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x05\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x06\x00\x23\x00\x24\x00\x07\x00\x25\x00\x16\x00\x2f\x00\x00\x00\x54\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\xa2\x00\x56\x00\x1a\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x24\x00\x05\x00\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x00\x00\x10\x00\x11\x00\x00\x00\x00\x00\x06\x00\x46\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x0a\x00\x12\x00\x13\x00\x06\x01\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x46\x01\x4a\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x2d\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x00\x00\xbc\x00\x16\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x17\x00\x18\x00\x6b\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x6c\x00\x1a\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x24\x00\x00\x00\x25\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x67\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x00\x00\x00\x00\x85\x00\x86\x00\x87\x00\xe1\x00\x88\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6b\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x6c\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\xbf\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x6b\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x05\x00\x85\x00\x86\x00\x87\x00\x05\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x4f\x00\x00\x00\x07\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x89\x01\x0a\x00\x13\x01\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\xc0\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x6b\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x05\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x9e\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x00\x00\x9f\x00\xa0\x00\xa1\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x00\x00\x05\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x2c\x01\x67\x00\x68\x00\x69\x00\x6a\x00\x06\x00\x46\x00\x05\x00\x07\x00\x34\x01\x6b\x00\x00\x00\x00\x00\x47\x00\x0a\x00\x00\x00\xd4\x00\x6c\x00\x06\x00\x00\x00\x00\x00\x07\x00\x05\x00\x00\x00\x00\x00\x00\x00\x54\x00\x0a\x00\x00\x00\x48\x00\x07\x01\x4a\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x2d\x00\x55\x00\x56\x00\x09\x00\x0a\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x90\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x05\x00\x00\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x06\x00\x46\x00\x05\x00\x07\x00\x43\x01\x6b\x00\x00\x00\x00\x00\x47\x00\x0a\x00\x00\x00\xe6\x00\x6c\x00\x06\x00\x00\x00\x00\x00\x07\x00\x05\x00\x00\x00\x00\x00\x00\x00\x57\x00\x0a\x00\x00\x00\x48\x00\x11\x01\x4a\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x2d\x00\x58\x00\x56\x00\x09\x00\x0a\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x44\x01\x00\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x05\x00\x00\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x06\x00\x46\x00\x05\x00\x07\x00\x00\x00\x6b\x00\x00\x00\x00\x00\x47\x00\x0a\x00\x00\x00\x2c\x00\x6c\x00\x06\x00\x00\x00\x00\x00\x07\x00\x05\x00\x00\x00\x00\x00\x00\x00\x88\x00\x0a\x00\x00\x00\x48\x00\x14\x01\x4a\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x2d\x00\x89\x00\x56\x00\x09\x00\x0a\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x27\x01\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x28\x01\x00\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\xaa\xff\xaa\xff\x34\x00\x35\x00\x6b\x00\x36\x00\x37\x00\x00\x00\x05\x00\x00\x00\x00\x00\x6c\x00\x00\x00\x00\x00\x00\x00\x05\x00\x2f\x00\x38\x00\x31\x00\x06\x00\x91\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x06\x00\x5f\x00\x0a\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x39\x00\x0a\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x6b\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x05\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x8e\x01\x8f\x01\x90\x01\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x7f\x01\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x05\x00\x00\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x06\x00\x46\x00\x05\x00\x07\x00\x00\x00\x6b\x00\x00\x00\x00\x00\x47\x00\x0a\x00\x00\x00\x00\x00\x6c\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x9b\x00\x0a\x00\x00\x00\x48\x00\x15\x01\x4a\x00\x50\x00\x9c\x00\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x86\x01\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x00\x00\x00\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\x00\x00\xa2\x01\x8b\x00\x35\x00\x6b\x00\x36\x00\x37\x00\x00\x00\x40\x01\x00\x00\x00\x00\x6c\x00\x00\x00\x05\x00\x00\x00\x05\x00\x2f\x00\x38\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x06\x00\x07\x00\x2c\x01\x07\x00\x00\x00\x00\x00\x2d\x01\x0a\x00\x60\x00\x0a\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x6b\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x05\x00\x85\x00\x86\x00\x87\x00\x05\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x94\x01\x00\x00\x07\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x8b\x01\x0a\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x6b\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x2a\x01\x85\x00\x86\x00\x87\x00\x05\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x67\x01\x00\x00\x00\x00\x09\x00\x0a\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x00\x00\x05\x00\x85\x00\x86\x00\x87\x00\x2b\x01\x88\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x91\x00\x06\x00\x46\x00\x00\x00\x07\x00\x6b\x00\xf1\x00\xf2\x00\x37\x00\x47\x00\x0a\x00\x00\x00\x6c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x2f\x00\x38\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x1a\x01\x4a\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x0a\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x00\x00\x05\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\x06\x00\x46\x00\x05\x00\x07\x00\x6b\x00\x00\x00\x00\x00\xbb\x00\x47\x00\x0a\x00\x00\x00\x6c\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x00\x00\x48\x00\x2f\x01\x4a\x00\x50\x00\x51\x00\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x00\x00\x05\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x67\x00\x68\x00\x69\x00\x6a\x00\xc3\x00\x06\x00\x46\x00\x05\x00\x07\x00\x6b\x00\x00\x00\x00\x00\x00\x00\x47\x00\x0a\x00\x00\x00\x6c\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x05\x00\x3a\x01\x00\x00\x00\x00\x09\x00\x0a\x00\x00\x00\x48\x00\x49\x00\x4a\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x4f\x01\x00\x00\x00\x00\x09\x00\x0a\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x6b\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x05\x00\x85\x00\x86\x00\x87\x00\x05\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x55\x01\x00\x00\x07\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x8c\x01\x0a\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x7c\x00\x00\x00\x7d\x00\xc7\x00\x7e\x00\x6b\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x05\x00\x85\x00\x86\x00\x87\x00\x05\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\xed\x00\x00\x00\x07\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x7d\x01\x0a\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x6b\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x05\x00\x85\x00\x86\x00\x87\x00\xd4\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\xfc\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x05\x00\x85\x00\x86\x00\x87\x00\x05\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x05\x01\x00\x00\x07\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x80\x01\x0a\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x6b\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x05\x00\x85\x00\x86\x00\x87\x00\x05\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x10\x01\x00\x00\x07\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x83\x01\x0a\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x05\x00\x00\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x1d\x01\x00\x00\x00\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x05\x00\x00\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x5e\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5f\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x67\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x21\x01\x00\x00\x00\x00\x00\x00\x6b\x00\x7f\x00\x80\x00\x05\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x00\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x67\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x22\x01\x7d\x00\x00\x00\x7e\x00\x6b\x00\x7f\x00\x80\x00\x81\x00\x05\x00\x83\x00\x84\x00\x6c\x00\x05\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x00\x00\x00\x00\x07\x00\x84\x01\x0a\x00\x00\x00\x00\x00\x34\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x05\x00\x83\x00\x84\x00\x00\x00\x05\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x00\x00\x00\x00\x07\x00\x35\x01\x0a\x00\x00\x00\x00\x00\x36\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x67\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x6b\x00\x7f\x00\x80\x00\x05\x00\x82\x00\x83\x00\x84\x00\x6c\x00\x00\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x38\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\x7c\x00\x00\x00\x7d\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x05\x00\x83\x00\x84\x00\x00\x00\x05\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x00\x00\x00\x00\x07\x00\x39\x01\x0a\x00\x00\x00\x00\x00\x3d\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x05\x00\x7b\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x06\x00\x46\x00\x7f\x00\x07\x00\x00\x00\x82\x00\x83\x00\x84\x00\x47\x00\x0a\x00\x85\x00\x86\x00\x87\x00\x00\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x4b\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x05\x00\x7b\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x06\x00\x46\x00\x05\x00\x07\x00\x00\x00\x82\x00\x83\x00\x84\x00\x47\x00\x0a\x00\x85\x00\x86\x00\x00\x00\x06\x00\x88\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3e\x01\x0a\x00\x00\x00\x48\x00\x4c\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x05\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x82\x00\x00\x00\x84\x00\x47\x01\x0a\x00\x85\x00\x86\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x05\x00\x7b\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x06\x00\x46\x00\x05\x00\x07\x00\x00\x00\x82\x00\x00\x00\x00\x00\x47\x00\x0a\x00\x85\x00\x86\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x00\x0a\x00\x00\x00\x48\x00\x4d\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x05\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x06\x00\x46\x00\x00\x00\x07\x00\x00\x00\x82\x00\x00\x00\x00\x00\x47\x00\x0a\x00\x00\x00\x86\x00\x00\x00\x8d\x01\x8b\x00\x35\x00\x00\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x4e\x00\x4a\x00\x05\x00\x2f\x00\x38\x00\x31\x00\x00\x00\x00\x00\x00\x00\x05\x00\x77\x01\x8b\x00\x35\x00\x06\x00\x36\x00\x37\x00\x07\x00\x00\x00\x00\x00\x00\x00\x06\x00\x60\x00\x0a\x00\x07\x00\x05\x00\x2f\x00\x38\x00\x31\x00\x08\x01\x0a\x00\x00\x00\x05\x00\x04\x01\x8b\x00\x35\x00\x06\x00\x36\x00\x37\x00\x07\x00\x00\x00\x00\x00\x00\x00\x06\x00\x60\x00\x0a\x00\x07\x00\x05\x00\x2f\x00\x38\x00\x31\x00\x09\x01\x0a\x00\x00\x00\x05\x00\x8a\x00\x8b\x00\x35\x00\x06\x00\x36\x00\x37\x00\x07\x00\x00\x00\x00\x00\x00\x00\x06\x00\x60\x00\x0a\x00\x07\x00\x05\x00\x2f\x00\x38\x00\x31\x00\x0a\x01\x0a\x00\x00\x00\x05\x00\x00\x00\x34\x00\x35\x00\x06\x00\x36\x00\x37\x00\x07\x00\x00\x00\x00\x00\x00\x00\x06\x00\x60\x00\x0a\x00\x07\x00\x05\x00\x2f\x00\x38\x00\x31\x00\x0d\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x0a\x00\x6d\x00\x00\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x7c\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x7c\x00\x73\x00\x74\x00\x00\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x05\x00\x7b\x00\x05\x00\x00\x00\x05\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x06\x00\x07\x00\x06\x00\x07\x00\x05\x00\x07\x00\x0e\x01\x0a\x00\x0f\x01\x0a\x00\x16\x01\x0a\x00\x00\x00\x00\x00\x05\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x01\x0a\x00\x06\x00\x05\x00\x06\x00\x07\x00\x00\x00\x07\x00\x00\x00\x00\x00\x1c\x01\x0a\x00\x1e\x01\x0a\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x00\x00\x1f\x01\x0a\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x05\x00\x00\x00\x07\x00\x91\x00\x0a\x00\x00\x00\x00\x00\x92\x00\x0a\x00\x00\x00\x05\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x93\x00\x0a\x00\x06\x00\x05\x00\x06\x00\x07\x00\x00\x00\x07\x00\x00\x00\x00\x00\x94\x00\x0a\x00\x95\x00\x0a\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x00\x00\x96\x00\x0a\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x05\x00\x00\x00\x07\x00\x97\x00\x0a\x00\x00\x00\x00\x00\x98\x00\x0a\x00\x00\x00\x05\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\x00\x0a\x00\x06\x00\x05\x00\x06\x00\x07\x00\x00\x00\x07\x00\x00\x00\x00\x00\x9a\x00\x0a\x00\x9d\x00\x0a\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x00\x00\xa3\x00\x0a\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x05\x00\x00\x00\x07\x00\xa4\x00\x0a\x00\x00\x00\x00\x00\xa5\x00\x0a\x00\x00\x00\x05\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa6\x00\x0a\x00\x06\x00\x05\x00\x06\x00\x07\x00\x00\x00\x07\x00\x00\x00\x00\x00\xa7\x00\x0a\x00\xa8\x00\x0a\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x00\x00\xa9\x00\x0a\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x05\x00\x00\x00\x07\x00\xaa\x00\x0a\x00\x00\x00\x00\x00\xab\x00\x0a\x00\x00\x00\x05\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xac\x00\x0a\x00\x06\x00\x05\x00\x06\x00\x07\x00\x00\x00\x07\x00\x00\x00\x00\x00\xad\x00\x0a\x00\xae\x00\x0a\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x00\x00\xaf\x00\x0a\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x05\x00\x00\x00\x07\x00\xb0\x00\x0a\x00\x00\x00\x00\x00\xb1\x00\x0a\x00\x00\x00\x05\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x0a\x00\x06\x00\x05\x00\x06\x00\x07\x00\x00\x00\x07\x00\x00\x00\x00\x00\xb3\x00\x0a\x00\xb4\x00\x0a\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x00\x00\xb5\x00\x0a\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x06\x00\x05\x00\x00\x00\x07\x00\xb6\x00\x0a\x00\x00\x00\x00\x00\x45\x00\x0a\x00\x00\x00\x05\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x00\x0a\x00\x06\x00\x05\x00\x06\x00\x07\x00\x00\x00\x07\x00\x00\x00\x00\x00\x5b\x00\x0a\x00\x5c\x00\x0a\x00\x06\x00\x05\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5d\x00\x0a\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x61\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (3, 192) [
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151),
	(152 , happyReduce_152),
	(153 , happyReduce_153),
	(154 , happyReduce_154),
	(155 , happyReduce_155),
	(156 , happyReduce_156),
	(157 , happyReduce_157),
	(158 , happyReduce_158),
	(159 , happyReduce_159),
	(160 , happyReduce_160),
	(161 , happyReduce_161),
	(162 , happyReduce_162),
	(163 , happyReduce_163),
	(164 , happyReduce_164),
	(165 , happyReduce_165),
	(166 , happyReduce_166),
	(167 , happyReduce_167),
	(168 , happyReduce_168),
	(169 , happyReduce_169),
	(170 , happyReduce_170),
	(171 , happyReduce_171),
	(172 , happyReduce_172),
	(173 , happyReduce_173),
	(174 , happyReduce_174),
	(175 , happyReduce_175),
	(176 , happyReduce_176),
	(177 , happyReduce_177),
	(178 , happyReduce_178),
	(179 , happyReduce_179),
	(180 , happyReduce_180),
	(181 , happyReduce_181),
	(182 , happyReduce_182),
	(183 , happyReduce_183),
	(184 , happyReduce_184),
	(185 , happyReduce_185),
	(186 , happyReduce_186),
	(187 , happyReduce_187),
	(188 , happyReduce_188),
	(189 , happyReduce_189),
	(190 , happyReduce_190),
	(191 , happyReduce_191),
	(192 , happyReduce_192)
	]

happy_n_terms = 97 :: Int
happy_n_nonterms = 56 :: Int

happyReduce_3 = happySpecReduce_2  0# happyReduction_3
happyReduction_3 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (annotateList happy_var_2 (CSPMFile happy_var_2)
	)}

happyReduce_4 = happyMonadReduce 4# 1# happyReduction_4
happyReduction_4 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	( do
                                                    d <- convDecl happy_var_2 happy_var_4 
                                                    d <- annotateWithSymbolTable d
                                                    return $ annotate2 happy_var_1 happy_var_4 (Bind [d]))}}}
	) (\r -> happyReturn (happyIn7 r))

happyReduce_5 = happySpecReduce_3  1# happyReduction_5
happyReduction_5 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (annotate2 happy_var_1 happy_var_3 (RunAssertion (annotate2 happy_var_1 happy_var_3 (ASNot happy_var_3)))
	)}}

happyReduce_6 = happySpecReduce_2  1# happyReduction_6
happyReduction_6 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (annotate2 happy_var_1 happy_var_2 (RunAssertion happy_var_2)
	)}}

happyReduce_7 = happyMonadReduce 2# 1# happyReduction_7
happyReduction_7 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { happy_var_2 -> 
	( do
                                                    d <- annotateWithSymbolTable $
                                                        annotate2List happy_var_1 happy_var_2 (External (map unLoc happy_var_2))
                                                    return $ annotate2List happy_var_1 happy_var_2 (Bind [d]))}}
	) (\r -> happyReturn (happyIn7 r))

happyReduce_8 = happyMonadReduce 2# 1# happyReduction_8
happyReduction_8 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { happy_var_2 -> 
	( do
                                                    d <- annotateWithSymbolTable $
                                                        annotate2List happy_var_1 happy_var_2 (Transparent (map unLoc happy_var_2))
                                                    return $ annotate2List happy_var_1 happy_var_2 (Bind [d]))}}
	) (\r -> happyReturn (happyIn7 r))

happyReduce_9 = happySpecReduce_1  1# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (annotate happy_var_1 (Evaluate happy_var_1)
	)}

happyReduce_10 = happySpecReduce_1  2# happyReduction_10
happyReduction_10 happy_x_1
	 =  happyIn8
		 (
	)

happyReduce_11 = happySpecReduce_2  2# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  happyIn8
		 (
	)

happyReduce_12 = happySpecReduce_0  3# happyReduction_12
happyReduction_12  =  happyIn9
		 (
	)

happyReduce_13 = happySpecReduce_2  3# happyReduction_13
happyReduction_13 happy_x_2
	happy_x_1
	 =  happyIn9
		 (
	)

happyReduce_14 = happySpecReduce_0  4# happyReduction_14
happyReduction_14  =  happyIn10
		 ([]
	)

happyReduce_15 = happySpecReduce_1  4# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_16 = happySpecReduce_1  5# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (attachTypeAnnotations $
                                                    combineDecls (reverse happy_var_1)
	)}

happyReduce_17 = happySpecReduce_1  6# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 ([happy_var_1]
	)}

happyReduce_18 = happySpecReduce_3  6# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (case happy_var_3 of 
                                                    Just d -> d:happy_var_1
                                                    Nothing -> happy_var_1
	)}}

happyReduce_19 = happySpecReduce_0  7# happyReduction_19
happyReduction_19  =  happyIn13
		 (Nothing
	)

happyReduce_20 = happySpecReduce_1  7# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (Just happy_var_1
	)}

happyReduce_21 = happyMonadReduce 1# 8# happyReduction_21
happyReduction_21 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut15 happy_x_1 of { happy_var_1 -> 
	( annotateWithSymbolTable happy_var_1)}
	) (\r -> happyReturn (happyIn14 r))

happyReduce_22 = happyMonadReduce 3# 9# happyReduction_22
happyReduction_22 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	( convDecl happy_var_1 happy_var_3)}}
	) (\r -> happyReturn (happyIn15 r))

happyReduce_23 = happySpecReduce_2  9# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (annotate2List happy_var_1 happy_var_2 (Channel (map unLoc happy_var_2) Nothing)
	)}}

happyReduce_24 = happyReduce 4# 9# happyReduction_24
happyReduction_24 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	happyIn15
		 (annotate2 happy_var_1 happy_var_4 (Channel (map unLoc happy_var_2) (Just happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_25 = happyReduce 4# 9# happyReduction_25
happyReduction_25 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut22 happy_x_4 of { happy_var_4 -> 
	happyIn15
		 (annotate2List happy_var_1 happy_var_4 (DataType (unLoc happy_var_2) happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_26 = happyReduce 4# 9# happyReduction_26
happyReduction_26 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut22 happy_x_4 of { happy_var_4 -> 
	happyIn15
		 (annotate2List happy_var_1 happy_var_4 (SubType (unLoc happy_var_2) happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_27 = happySpecReduce_2  9# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (annotate2List happy_var_1 happy_var_2 (External (map unLoc happy_var_2))
	)}}

happyReduce_28 = happySpecReduce_2  9# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (annotate2List happy_var_1 happy_var_2 (Transparent (map unLoc happy_var_2))
	)}}

happyReduce_29 = happySpecReduce_3  9# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (annotate2 happy_var_1 happy_var_3 (Assert (annotate2 happy_var_1 happy_var_3 (ASNot happy_var_3)))
	)}}

happyReduce_30 = happySpecReduce_2  9# happyReduction_30
happyReduction_30 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (annotate2 happy_var_1 happy_var_2 (Assert happy_var_2)
	)}}

happyReduce_31 = happyReduce 4# 9# happyReduction_31
happyReduction_31 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	happyIn15
		 (annotate2 happy_var_1 happy_var_4 (NameType (unLoc happy_var_2) happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_32 = happyReduce 9# 9# happyReduction_32
happyReduction_32 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	case happyOut10 happy_x_5 of { happy_var_5 -> 
	case happyOut10 happy_x_8 of { happy_var_8 -> 
	case happyOutTok happy_x_9 of { happy_var_9 -> 
	happyIn15
		 (annotate2 happy_var_1 happy_var_9 (Module (unLoc happy_var_2) happy_var_3 
                                                    (map checkModuleDecl happy_var_5)
                                                    (map checkModuleDecl happy_var_8))
	) `HappyStk` happyRest}}}}}}

happyReduce_33 = happyReduce 4# 9# happyReduction_33
happyReduction_33 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn15
		 (annotate2 happy_var_1 happy_var_4 (TimedSection Nothing
                                                    Nothing
                                                    (map checkTimedDecl happy_var_3))
	) `HappyStk` happyRest}}}

happyReduce_34 = happyReduce 7# 9# happyReduction_34
happyReduction_34 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	case happyOut10 happy_x_6 of { happy_var_6 -> 
	case happyOutTok happy_x_7 of { happy_var_7 -> 
	happyIn15
		 (annotate2 happy_var_1 happy_var_7 (TimedSection Nothing
                                                    (Just happy_var_3)
                                                    (map checkTimedDecl happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_35 = happySpecReduce_3  9# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (annotate2List' happy_var_1 happy_var_3 (ParsedTypeAnnotation
                                                    (map unLoc happy_var_1) happy_var_3)
	)}}

happyReduce_36 = happyReduce 5# 9# happyReduction_36
happyReduction_36 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	case happyOut17 happy_x_5 of { happy_var_5 -> 
	happyIn15
		 (annotate2List happy_var_1 happy_var_5 (ModuleInstance (unLoc happy_var_2)
                                                        (unLoc happy_var_4) happy_var_5 []
                                                        Nothing)
	) `HappyStk` happyRest}}}}

happyReduce_37 = happySpecReduce_0  10# happyReduction_37
happyReduction_37  =  happyIn16
		 ([]
	)

happyReduce_38 = happySpecReduce_3  10# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (reverse happy_var_2
	)}

happyReduce_39 = happySpecReduce_3  11# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (reverse happy_var_2
	)}

happyReduce_40 = happySpecReduce_3  12# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (annotate2 happy_var_1 happy_var_3 (Refinement happy_var_1 (getRefinesModel happy_var_2) happy_var_3 [])
	)}}}

happyReduce_41 = happyReduce 4# 12# happyReduction_41
happyReduction_41 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	case happyOut20 happy_x_4 of { happy_var_4 -> 
	happyIn18
		 (annotate2 happy_var_1 happy_var_4 (Refinement happy_var_1 (getRefinesModel happy_var_2) happy_var_3 [unLoc happy_var_4])
	) `HappyStk` happyRest}}}}

happyReduce_42 = happySpecReduce_2  12# happyReduction_42
happyReduction_42 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (annotate2 happy_var_1 happy_var_2 (PropertyCheck happy_var_1 (unLoc happy_var_2) Nothing)
	)}}

happyReduce_43 = happySpecReduce_3  12# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (annotate2 happy_var_1 happy_var_3 (PropertyCheck happy_var_1 (unLoc happy_var_2) (Just (getPropModel happy_var_3)))
	)}}}

happyReduce_44 = happySpecReduce_1  13# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (liftLoc happy_var_1 DeadlockFreedom
	)}

happyReduce_45 = happySpecReduce_1  13# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (liftLoc happy_var_1 LivelockFreedom
	)}

happyReduce_46 = happySpecReduce_1  13# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (liftLoc happy_var_1 LivelockFreedom
	)}

happyReduce_47 = happySpecReduce_1  13# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (liftLoc happy_var_1 Deterministic
	)}

happyReduce_48 = happySpecReduce_2  14# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (annotate2 happy_var_1 happy_var_2 (TauPriority happy_var_2)
	)}}

happyReduce_49 = happySpecReduce_3  15# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (annotate2 happy_var_1 happy_var_3 (DataTypeClause (unLoc happy_var_1) (Just happy_var_3))
	)}}

happyReduce_50 = happySpecReduce_1  15# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (annotate happy_var_1 (DataTypeClause (unLoc happy_var_1) Nothing)
	)}

happyReduce_51 = happySpecReduce_1  16# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (reverse happy_var_1
	)}

happyReduce_52 = happySpecReduce_1  17# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 ([happy_var_1]
	)}

happyReduce_53 = happySpecReduce_3  17# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (happy_var_3:happy_var_1
	)}}

happyReduce_54 = happySpecReduce_1  18# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (liftLoc happy_var_1 (UnQual (OccName (getName happy_var_1)))
	)}

happyReduce_55 = happySpecReduce_3  18# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (annotate happy_var_1 (Qual (OccName (getName happy_var_1)) (unLoc happy_var_3))
	)}}

happyReduce_56 = happySpecReduce_1  19# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (liftLoc happy_var_1 (UnQual (OccName (getName happy_var_1)))
	)}

happyReduce_57 = happySpecReduce_1  20# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (reverse happy_var_1
	)}

happyReduce_58 = happySpecReduce_1  21# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 ([happy_var_1]
	)}

happyReduce_59 = happySpecReduce_3  21# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 (happy_var_3:happy_var_1
	)}}

happyReduce_60 = happySpecReduce_1  22# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (liftLoc happy_var_1 (STypeScheme [] [] happy_var_1)
	)}

happyReduce_61 = happySpecReduce_3  22# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (liftLoc happy_var_1 (STypeScheme [] [happy_var_1] happy_var_3)
	)}}

happyReduce_62 = happyReduce 5# 22# happyReduction_62
happyReduction_62 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	happyIn28
		 (liftLoc happy_var_1 (STypeScheme [] happy_var_2 happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_63 = happySpecReduce_1  23# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (reverse happy_var_1
	)}

happyReduce_64 = happySpecReduce_3  24# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 (happy_var_3:happy_var_1
	)}}

happyReduce_65 = happySpecReduce_1  24# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ([happy_var_1]
	)}

happyReduce_66 = happySpecReduce_1  25# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (liftLoc happy_var_1 (STVar (unLoc happy_var_1))
	)}

happyReduce_67 = happySpecReduce_3  25# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (annotate2 happy_var_1 happy_var_3 (STSet happy_var_2)
	)}}}

happyReduce_68 = happySpecReduce_3  25# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (annotate2 happy_var_1 happy_var_3 (STSeq happy_var_2)
	)}}}

happyReduce_69 = happySpecReduce_3  25# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (annotate2 happy_var_1 happy_var_3 (STParen happy_var_2)
	)}}}

happyReduce_70 = happySpecReduce_3  25# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (annotate2 happy_var_1 happy_var_3 (STExtendable happy_var_3 (unLoc happy_var_1))
	)}}

happyReduce_71 = happySpecReduce_3  25# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (annotate2 happy_var_1 happy_var_3 (STDotable happy_var_1 happy_var_3)
	)}}

happyReduce_72 = happySpecReduce_3  25# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (annotate2 happy_var_1 happy_var_3 (STDot happy_var_1 happy_var_3)
	)}}

happyReduce_73 = happySpecReduce_3  25# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (annotate2 happy_var_1 happy_var_3 (STTuple happy_var_2)
	)}}}

happyReduce_74 = happyReduce 4# 25# happyReduction_74
happyReduction_74 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn31
		 (annotate2 happy_var_1 happy_var_4 (STFunction [] happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_75 = happySpecReduce_3  25# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (annotate2 happy_var_1 happy_var_3 (STFunction
                                        (getFuncArgs happy_var_1) happy_var_3)
	)}}

happyReduce_76 = happySpecReduce_2  26# happyReduction_76
happyReduction_76 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_2 (STypeConstraint
                                        (constraintForName (getLoc happy_var_1)
                                            (getName happy_var_1))
                                        (unLoc happy_var_2))
	)}}

happyReduce_77 = happySpecReduce_3  27# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (happy_var_1:(reverse happy_var_3)
	)}}

happyReduce_78 = happySpecReduce_3  28# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 (happy_var_3:happy_var_1
	)}}

happyReduce_79 = happySpecReduce_1  28# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ([happy_var_1]
	)}

happyReduce_80 = happySpecReduce_1  29# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (liftLoc happy_var_1 (Int (getInt happy_var_1))
	)}

happyReduce_81 = happySpecReduce_1  29# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (liftLoc happy_var_1 (Char (getChar happy_var_1))
	)}

happyReduce_82 = happySpecReduce_1  29# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (liftLoc happy_var_1 (String (getString happy_var_1))
	)}

happyReduce_83 = happySpecReduce_1  29# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (liftLoc happy_var_1 (Bool True)
	)}

happyReduce_84 = happySpecReduce_1  29# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (liftLoc happy_var_1 (Bool False)
	)}

happyReduce_85 = happySpecReduce_1  30# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (convPat happy_var_1
	)}

happyReduce_86 = happySpecReduce_1  31# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 ([happy_var_1]
	)}

happyReduce_87 = happySpecReduce_3  31# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (happy_var_3:happy_var_1
	)}}

happyReduce_88 = happyMonadReduce 1# 32# happyReduction_88
happyReduction_88 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	( do
                                    modifyTopFileParserState (
                                        \ st @ (FileParserState { sequenceStack = (c:cs) }) -> 
                                            st { sequenceStack = (c+1):cs })
                                    return happy_var_1)}
	) (\r -> happyReturn (happyIn38 r))

happyReduce_89 = happySpecReduce_1  33# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (happy_var_1
	)}

happyReduce_90 = happySpecReduce_1  34# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (checkExp happy_var_1
	)}

happyReduce_91 = happySpecReduce_1  35# happyReduction_91
happyReduction_91 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (reverse happy_var_1
	)}

happyReduce_92 = happySpecReduce_1  36# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 ([happy_var_1]
	)}

happyReduce_93 = happySpecReduce_3  36# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (happy_var_3:happy_var_1
	)}}

happyReduce_94 = happyMonadReduce 1# 37# happyReduction_94
happyReduction_94 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut44 happy_x_1 of { happy_var_1 -> 
	( do
                                        t <- freshPType
                                        let An l _ e = happy_var_1
                                        return $ An l (Nothing, t) e)}
	) (\r -> happyReturn (happyIn43 r))

happyReduce_95 = happySpecReduce_1  38# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (liftLoc happy_var_1 (Lit (unLoc happy_var_1))
	)}

happyReduce_96 = happySpecReduce_1  38# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (liftLoc happy_var_1 (Var (unLoc happy_var_1))
	)}

happyReduce_97 = happySpecReduce_3  38# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (Tuple happy_var_2)
	)}}}

happyReduce_98 = happySpecReduce_3  38# happyReduction_98
happyReduction_98 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (DotApp happy_var_1 happy_var_3)
	)}}

happyReduce_99 = happyReduce 4# 38# happyReduction_99
happyReduction_99 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_4 (Let (checkLetDecls happy_var_2) happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_100 = happyReduce 6# 38# happyReduction_100
happyReduction_100 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	case happyOut43 happy_x_6 of { happy_var_6 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_6 (If happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_101 = happySpecReduce_3  38# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (Paren happy_var_2)
	)}}}

happyReduce_102 = happyReduce 4# 38# happyReduction_102
happyReduction_102 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_4 (App happy_var_1 happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_103 = happyReduce 4# 38# happyReduction_103
happyReduction_103 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_4 (Lambda happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_104 = happySpecReduce_3  38# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp Equals happy_var_1 happy_var_3)
	)}}

happyReduce_105 = happySpecReduce_3  38# happyReduction_105
happyReduction_105 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp NotEquals happy_var_1 happy_var_3)
	)}}

happyReduce_106 = happySpecReduce_3  38# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp LessThan happy_var_1 happy_var_3)
	)}}

happyReduce_107 = happySpecReduce_3  38# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp GreaterThan happy_var_1 happy_var_3)
	)}}

happyReduce_108 = happySpecReduce_3  38# happyReduction_108
happyReduction_108 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp LessThanEq happy_var_1 happy_var_3)
	)}}

happyReduce_109 = happySpecReduce_3  38# happyReduction_109
happyReduction_109 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp GreaterThanEq happy_var_1 happy_var_3)
	)}}

happyReduce_110 = happySpecReduce_2  38# happyReduction_110
happyReduction_110 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_2 (BooleanUnaryOp Not happy_var_2)
	)}}

happyReduce_111 = happySpecReduce_3  38# happyReduction_111
happyReduction_111 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp Or happy_var_1 happy_var_3)
	)}}

happyReduce_112 = happySpecReduce_3  38# happyReduction_112
happyReduction_112 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp And happy_var_1 happy_var_3)
	)}}

happyReduce_113 = happySpecReduce_2  38# happyReduction_113
happyReduction_113 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_2 (MathsUnaryOp Negate happy_var_2)
	)}}

happyReduce_114 = happySpecReduce_3  38# happyReduction_114
happyReduction_114 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (MathsBinaryOp Plus happy_var_1 happy_var_3)
	)}}

happyReduce_115 = happySpecReduce_3  38# happyReduction_115
happyReduction_115 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (MathsBinaryOp Minus happy_var_1 happy_var_3)
	)}}

happyReduce_116 = happySpecReduce_3  38# happyReduction_116
happyReduction_116 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (MathsBinaryOp Mod happy_var_1 happy_var_3)
	)}}

happyReduce_117 = happySpecReduce_3  38# happyReduction_117
happyReduction_117 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (MathsBinaryOp Divide happy_var_1 happy_var_3)
	)}}

happyReduce_118 = happySpecReduce_3  38# happyReduction_118
happyReduction_118 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (MathsBinaryOp Times happy_var_1 happy_var_3)
	)}}

happyReduce_119 = happySpecReduce_1  38# happyReduction_119
happyReduction_119 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (annotate happy_var_1 (List [])
	)}

happyReduce_120 = happySpecReduce_3  38# happyReduction_120
happyReduction_120 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (List happy_var_2)
	)}}}

happyReduce_121 = happyReduce 5# 38# happyReduction_121
happyReduction_121 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	case happyOut60 happy_x_4 of { happy_var_4 -> 
	case happyOut39 happy_x_5 of { happy_var_5 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_5 (ListComp happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_122 = happyReduce 4# 38# happyReduction_122
happyReduction_122 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut39 happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_4 (ListEnumFrom happy_var_2)
	) `HappyStk` happyRest}}}

happyReduce_123 = happyReduce 5# 38# happyReduction_123
happyReduction_123 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	case happyOut39 happy_x_5 of { happy_var_5 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_5 (ListEnumFromTo happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_124 = happyReduce 6# 38# happyReduction_124
happyReduction_124 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut60 happy_x_5 of { happy_var_5 -> 
	case happyOut39 happy_x_6 of { happy_var_6 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_6 (ListEnumFromComp happy_var_2 happy_var_5)
	) `HappyStk` happyRest}}}}

happyReduce_125 = happyReduce 7# 38# happyReduction_125
happyReduction_125 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	case happyOut60 happy_x_6 of { happy_var_6 -> 
	case happyOut39 happy_x_7 of { happy_var_7 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_7 (ListEnumFromToComp happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}}

happyReduce_126 = happySpecReduce_3  38# happyReduction_126
happyReduction_126 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (Concat happy_var_1 happy_var_3)
	)}}

happyReduce_127 = happySpecReduce_2  38# happyReduction_127
happyReduction_127 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_2 (ListLength happy_var_2)
	)}}

happyReduce_128 = happySpecReduce_3  38# happyReduction_128
happyReduction_128 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (Set happy_var_2)
	)}}}

happyReduce_129 = happyReduce 5# 38# happyReduction_129
happyReduction_129 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	case happyOut60 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_5 (SetComp happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_130 = happyReduce 4# 38# happyReduction_130
happyReduction_130 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_4 (SetEnumFrom happy_var_2)
	) `HappyStk` happyRest}}}

happyReduce_131 = happyReduce 5# 38# happyReduction_131
happyReduction_131 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_5 (SetEnumFromTo happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_132 = happyReduce 6# 38# happyReduction_132
happyReduction_132 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut60 happy_x_5 of { happy_var_5 -> 
	case happyOutTok happy_x_6 of { happy_var_6 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_6 (SetEnumFromComp happy_var_2 happy_var_5)
	) `HappyStk` happyRest}}}}

happyReduce_133 = happyReduce 7# 38# happyReduction_133
happyReduction_133 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	case happyOut60 happy_x_6 of { happy_var_6 -> 
	case happyOutTok happy_x_7 of { happy_var_7 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_7 (SetEnumFromToComp happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}}

happyReduce_134 = happySpecReduce_3  38# happyReduction_134
happyReduction_134 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (SetEnum happy_var_2)
	)}}}

happyReduce_135 = happyReduce 5# 38# happyReduction_135
happyReduction_135 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	case happyOut60 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_5 (SetEnumComp happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_136 = happySpecReduce_1  38# happyReduction_136
happyReduction_136 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (liftLoc happy_var_1 (ExpPatWildCard)
	)}

happyReduce_137 = happySpecReduce_3  38# happyReduction_137
happyReduction_137 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (liftLoc happy_var_1 (ExpPatDoublePattern happy_var_1 happy_var_3)
	)}}

happyReduce_138 = happyReduce 4# 38# happyReduction_138
happyReduction_138 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_4 (Prefix happy_var_1 happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_139 = happySpecReduce_3  38# happyReduction_139
happyReduction_139 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (Prefix happy_var_1 [] happy_var_3)
	)}}

happyReduce_140 = happySpecReduce_3  38# happyReduction_140
happyReduction_140 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (ExternalChoice happy_var_1 happy_var_3)
	)}}

happyReduce_141 = happySpecReduce_3  38# happyReduction_141
happyReduction_141 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (Hiding happy_var_1 happy_var_3)
	)}}

happyReduce_142 = happySpecReduce_3  38# happyReduction_142
happyReduction_142 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (InternalChoice happy_var_1 happy_var_3)
	)}}

happyReduce_143 = happySpecReduce_3  38# happyReduction_143
happyReduction_143 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (Interleave happy_var_1 happy_var_3)
	)}}

happyReduce_144 = happyReduce 5# 38# happyReduction_144
happyReduction_144 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	case happyOut43 happy_x_5 of { happy_var_5 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_5 (GenParallel happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_145 = happyReduce 7# 38# happyReduction_145
happyReduction_145 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	case happyOut43 happy_x_5 of { happy_var_5 -> 
	case happyOut43 happy_x_7 of { happy_var_7 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_7 (AlphaParallel happy_var_1 happy_var_3 happy_var_5 happy_var_7)
	) `HappyStk` happyRest}}}}

happyReduce_146 = happySpecReduce_3  38# happyReduction_146
happyReduction_146 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (Interrupt happy_var_1 happy_var_3)
	)}}

happyReduce_147 = happyReduce 5# 38# happyReduction_147
happyReduction_147 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	case happyOut43 happy_x_5 of { happy_var_5 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_5 (Exception happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_148 = happySpecReduce_3  38# happyReduction_148
happyReduction_148 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (SlidingChoice happy_var_1 happy_var_3)
	)}}

happyReduce_149 = happySpecReduce_3  38# happyReduction_149
happyReduction_149 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (SequentialComp happy_var_1 happy_var_3)
	)}}

happyReduce_150 = happySpecReduce_3  38# happyReduction_150
happyReduction_150 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_3 (GuardedExp happy_var_1 happy_var_3)
	)}}

happyReduce_151 = happyReduce 5# 38# happyReduction_151
happyReduction_151 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	case happyOut43 happy_x_5 of { happy_var_5 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_5 (SynchronisingExternalChoice happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_152 = happyReduce 5# 38# happyReduction_152
happyReduction_152 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	case happyOut43 happy_x_5 of { happy_var_5 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_5 (SynchronisingInterrupt happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_153 = happyReduce 5# 38# happyReduction_153
happyReduction_153 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	case happyOut58 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_5 (Rename happy_var_1 happy_var_3 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_154 = happyReduce 6# 38# happyReduction_154
happyReduction_154 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	case happyOut58 happy_x_4 of { happy_var_4 -> 
	case happyOut43 happy_x_6 of { happy_var_6 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_6 (LinkParallel happy_var_1 happy_var_3 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_155 = happyReduce 4# 38# happyReduction_155
happyReduction_155 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_4 (ReplicatedInterleave happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_156 = happyReduce 4# 38# happyReduction_156
happyReduction_156 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_4 (ReplicatedExternalChoice happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_157 = happyReduce 4# 38# happyReduction_157
happyReduction_157 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_4 (ReplicatedInternalChoice happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_158 = happyReduce 7# 38# happyReduction_158
happyReduction_158 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_5 of { happy_var_5 -> 
	case happyOut43 happy_x_7 of { happy_var_7 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_7 (ReplicatedAlphaParallel happy_var_2 happy_var_5 happy_var_7)
	) `HappyStk` happyRest}}}}

happyReduce_159 = happyReduce 6# 38# happyReduction_159
happyReduction_159 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut60 happy_x_4 of { happy_var_4 -> 
	case happyOut43 happy_x_6 of { happy_var_6 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_6 (ReplicatedParallel happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_160 = happyReduce 7# 38# happyReduction_160
happyReduction_160 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_2 of { happy_var_2 -> 
	case happyOut58 happy_x_3 of { happy_var_3 -> 
	case happyOut60 happy_x_5 of { happy_var_5 -> 
	case happyOut43 happy_x_7 of { happy_var_7 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_7 (ReplicatedLinkParallel happy_var_2 happy_var_3 happy_var_5 happy_var_7)
	) `HappyStk` happyRest}}}}}

happyReduce_161 = happyReduce 4# 38# happyReduction_161
happyReduction_161 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_4 (ReplicatedSequentialComp happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_162 = happyReduce 6# 38# happyReduction_162
happyReduction_162 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut60 happy_x_4 of { happy_var_4 -> 
	case happyOut43 happy_x_6 of { happy_var_6 -> 
	happyIn44
		 (annotate2 happy_var_1 happy_var_6 (ReplicatedSynchronisingExternalChoice
                                        happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_163 = happySpecReduce_2  39# happyReduction_163
happyReduction_163 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn45
		 (annotate2 happy_var_1 happy_var_2 (Input (convPat happy_var_2) Nothing)
	)}}

happyReduce_164 = happyReduce 4# 39# happyReduction_164
happyReduction_164 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn45
		 (annotate2 happy_var_1 happy_var_4 (Input (convPat happy_var_2) (Just (checkExp happy_var_4)))
	) `HappyStk` happyRest}}}

happyReduce_165 = happySpecReduce_2  39# happyReduction_165
happyReduction_165 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn45
		 (annotate2 happy_var_1 happy_var_2 (NonDetInput (convPat happy_var_2) Nothing)
	)}}

happyReduce_166 = happyReduce 4# 39# happyReduction_166
happyReduction_166 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn45
		 (annotate2 happy_var_1 happy_var_4 (NonDetInput (convPat happy_var_2) (Just (checkExp happy_var_4)))
	) `HappyStk` happyRest}}}

happyReduce_167 = happySpecReduce_2  39# happyReduction_167
happyReduction_167 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn45
		 (annotate2 happy_var_1 happy_var_2 (Output (checkExp happy_var_2))
	)}}

happyReduce_168 = happySpecReduce_3  40# happyReduction_168
happyReduction_168 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 ((happy_var_1, happy_var_3)
	)}}

happyReduce_169 = happySpecReduce_1  41# happyReduction_169
happyReduction_169 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (reverse happy_var_1
	)}

happyReduce_170 = happySpecReduce_1  42# happyReduction_170
happyReduction_170 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 ([happy_var_1]
	)}

happyReduce_171 = happySpecReduce_3  42# happyReduction_171
happyReduction_171 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (happy_var_3:happy_var_1
	)}}

happyReduce_172 = happySpecReduce_3  43# happyReduction_172
happyReduction_172 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 ((happy_var_1, happy_var_3)
	)}}

happyReduce_173 = happySpecReduce_1  44# happyReduction_173
happyReduction_173 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (reverse happy_var_1
	)}

happyReduce_174 = happySpecReduce_1  45# happyReduction_174
happyReduction_174 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 ([happy_var_1]
	)}

happyReduce_175 = happySpecReduce_3  45# happyReduction_175
happyReduction_175 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 (happy_var_3:happy_var_1
	)}}

happyReduce_176 = happySpecReduce_1  46# happyReduction_176
happyReduction_176 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (reverse happy_var_1
	)}

happyReduce_177 = happySpecReduce_1  47# happyReduction_177
happyReduction_177 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 ([happy_var_1]
	)}

happyReduce_178 = happySpecReduce_2  47# happyReduction_178
happyReduction_178 happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn53
		 (happy_var_2:happy_var_1
	)}}

happyReduce_179 = happySpecReduce_3  48# happyReduction_179
happyReduction_179 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut57 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 (happy_var_1:(reverse happy_var_3)
	)}}

happyReduce_180 = happySpecReduce_1  49# happyReduction_180
happyReduction_180 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 (reverse happy_var_1
	)}

happyReduce_181 = happySpecReduce_0  50# happyReduction_181
happyReduction_181  =  happyIn56
		 ([]
	)

happyReduce_182 = happySpecReduce_1  50# happyReduction_182
happyReduction_182 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (reverse happy_var_1
	)}

happyReduce_183 = happySpecReduce_3  51# happyReduction_183
happyReduction_183 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn57
		 (happy_var_3:happy_var_1
	)}}

happyReduce_184 = happySpecReduce_1  51# happyReduction_184
happyReduction_184 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 ([happy_var_1]
	)}

happyReduce_185 = happySpecReduce_0  52# happyReduction_185
happyReduction_185  =  happyIn58
		 ([]
	)

happyReduce_186 = happySpecReduce_2  52# happyReduction_186
happyReduction_186 happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_2 of { happy_var_2 -> 
	happyIn58
		 (happy_var_2
	)}

happyReduce_187 = happySpecReduce_3  53# happyReduction_187
happyReduction_187 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn59
		 (annotate2 happy_var_1 happy_var_3 (Generator happy_var_1 happy_var_3)
	)}}

happyReduce_188 = happySpecReduce_3  53# happyReduction_188
happyReduction_188 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn59
		 (annotate2 happy_var_1 happy_var_3 (Generator happy_var_1 happy_var_3)
	)}}

happyReduce_189 = happySpecReduce_1  53# happyReduction_189
happyReduction_189 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn59
		 (annotate happy_var_1 (Qualifier happy_var_1)
	)}

happyReduce_190 = happySpecReduce_1  54# happyReduction_190
happyReduction_190 happy_x_1
	 =  case happyOut61 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (reverse happy_var_1
	)}

happyReduce_191 = happySpecReduce_3  55# happyReduction_191
happyReduction_191 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_1 of { happy_var_1 -> 
	case happyOut59 happy_x_3 of { happy_var_3 -> 
	happyIn61
		 (happy_var_3:happy_var_1
	)}}

happyReduce_192 = happySpecReduce_1  55# happyReduction_192
happyReduction_192 happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	happyIn61
		 ([happy_var_1]
	)}

happyNewToken action sts stk
	= getNextTokenWrapper(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	L _ TEOF -> happyDoAction 96# tk action sts stk;
	L _ (TInteger _) -> cont 1#;
	L _ (TChar _) -> cont 2#;
	L _ (TString _) -> cont 3#;
	L _ (TIdent _) -> cont 4#;
	L _ TNewLine -> cont 5#;
	L _ TFalse -> cont 6#;
	L _ TTrue -> cont 7#;
	L _ TDefineEqual -> cont 8#;
	L _ TComma -> cont 9#;
	L _ TDot -> cont 10#;
	L _ TQuestionMark -> cont 11#;
	L _ TDollar -> cont 12#;
	L _ TExclamationMark -> cont 13#;
	L _ TDoubleDot -> cont 14#;
	L _ TColon -> cont 15#;
	L _ TDrawnFrom -> cont 16#;
	L _ TTie -> cont 17#;
	L _ TPipe -> cont 18#;
	L _ TDoubleAt -> cont 19#;
	L _ TWildCard -> cont 20#;
	L _ TIf -> cont 21#;
	L _ TThen -> cont 22#;
	L _ TElse -> cont 23#;
	L _ TLet -> cont 24#;
	L _ TWithin -> cont 25#;
	L _ TBackSlash -> cont 26#;
	L _ TLambdaDot -> cont 27#;
	L _ TChannel -> cont 28#;
	L _ TDataType -> cont 29#;
	L _ TSubType -> cont 30#;
	L _ TExternal -> cont 31#;
	L _ TTransparent -> cont 32#;
	L _ TNameType -> cont 33#;
	L _ TModule -> cont 34#;
	L _ TExports -> cont 35#;
	L _ TEndModule -> cont 36#;
	L _ TInstance -> cont 37#;
	L _ TScope -> cont 38#;
	L _ TTimed -> cont 39#;
	L _ TOfType -> cont 40#;
	L _ TYield -> cont 41#;
	L _ TYieldStar -> cont 42#;
	L _ TAssert -> cont 43#;
	L _ TDeadlockFree -> cont 44#;
	L _ TLivelockFree -> cont 45#;
	L _ TDivergenceFree -> cont 46#;
	L _ TDeterministic -> cont 47#;
	L _ TTauPriority -> cont 48#;
	L _ (TRefines _) -> cont 49#;
	L _ (TModel _) -> cont 50#;
	L _ TNot -> cont 51#;
	L _ TAssertNot -> cont 52#;
	L _ TAnd -> cont 53#;
	L _ TOr -> cont 54#;
	L _ TEq -> cont 55#;
	L _ TNotEq -> cont 56#;
	L _ TLtEq -> cont 57#;
	L _ TGtEq -> cont 58#;
	L _ TEmptySeq -> cont 59#;
	L _ TLt -> cont 60#;
	L _ TGt -> cont 61#;
	L _ TCloseSeq -> cont 62#;
	L _ TPlus -> cont 63#;
	L _ TMinus -> cont 64#;
	L _ TTimes -> cont 65#;
	L _ TDivide -> cont 66#;
	L _ TMod -> cont 67#;
	L _ TConcat -> cont 68#;
	L _ THash -> cont 69#;
	L _ TLParen -> cont 70#;
	L _ TRParen -> cont 71#;
	L _ TLBrace -> cont 72#;
	L _ TRBrace -> cont 73#;
	L _ TLPipeBrace -> cont 74#;
	L _ TRPipeBrace -> cont 75#;
	L _ TLDoubleSqBracket -> cont 76#;
	L _ TRDoubleSqBracket -> cont 77#;
	L _ TLPipeSqBracket -> cont 78#;
	L _ TRPipeSqBracket -> cont 79#;
	L _ TLSqBracket -> cont 80#;
	L _ TRSqBracket -> cont 81#;
	L _ TExtChoice -> cont 82#;
	L _ TIntChoice -> cont 83#;
	L _ TInterleave -> cont 84#;
	L _ TPrefix -> cont 85#;
	L _ TInterrupt -> cont 86#;
	L _ TSlidingChoice -> cont 87#;
	L _ TRException -> cont 88#;
	L _ TParallel -> cont 89#;
	L _ TSemiColon -> cont 90#;
	L _ TGuard -> cont 91#;
	L _ TLSyncExtChoice -> cont 92#;
	L _ TRSyncExtChoice -> cont 93#;
	L _ TLSyncInterrupt -> cont 94#;
	L _ TRSyncInterrupt -> cont 95#;
	_ -> happyError' tk
	})

happyError_ 96# tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => ParseMonad a -> (a -> ParseMonad b) -> ParseMonad b
happyThen = (>>=)
happyReturn :: () => a -> ParseMonad a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> ParseMonad a
happyReturn1 = happyReturn
happyError' :: () => (LToken) -> ParseMonad a
happyError' tk = parseError tk

parseFile_ = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut6 x))

parseInteractiveStmt_ = happySomeParser where
  happySomeParser = happyThen (happyParse 1#) (\x -> happyReturn (happyOut7 x))

parseExpression_ = happySomeParser where
  happySomeParser = happyThen (happyParse 2#) (\x -> happyReturn (happyOut40 x))

happySeq = happyDontSeq


combineDecls :: [PDecl] -> [PDecl]
combineDecls [] = []
combineDecls ((An loc1 b (FunBind n ms Nothing)):
        (An loc2 c (FunBind n' ms' Nothing)):ds) | n == n' = 
    combineDecls $
        (An (combineSpans loc1 loc2) b (FunBind n (ms++ms') Nothing)):ds
combineDecls (d:ds) = d:combineDecls ds

constraintForName :: SrcSpan -> String -> Constraint
constraintForName _ "Eq" = CEq
constraintForName _ "Ord" = COrd
constraintForName _ "Inputable" = CInputable
constraintForName _ "Set" = CSet
constraintForName _ "Yieldable" = CYieldable
constraintForName loc s = throwSourceError [unknownConstraintError s loc]

attachTypeAnnotations :: [PDecl] -> [PDecl]
attachTypeAnnotations ds =
    let
        extractTypeAnnotations :: Decl UnRenamedName ->
            [(UnRenamedName, PSTypeScheme)]
        extractTypeAnnotations (ParsedTypeAnnotation ns ta) =
            [(n, ta) | n <- ns]
        extractTypeAnnotations _ = []

        extractedAnnotations =
            concatMap (extractTypeAnnotations . unAnnotate) ds

        nameLocs :: [(UnRenamedName, SrcSpan)]
        nameLocs = [(n, loc x) | (n, x) <- extractedAnnotations]

        locsByName :: [(UnRenamedName, [SrcSpan])]
        locsByName =
            map (\ nss -> (fst (head nss), map snd nss)) $
            groupBy (\x y -> fst x == fst y) $
            sortBy (\ x y -> compare (fst x) (fst y)) nameLocs

        dupeErrors = concatMap (\ (n, locs) ->
            case locs of
                [_] -> []
                _ -> [ambiguousTypeAnnotationsError n locs]
            ) locsByName

        nameForDecl  (An _ _ (FunBind n _ _)) = [n]
        nameForDecl (An _ _ (PatBind (An _ _ (PVar n)) _ _)) = [n]
        nameForDecl _ = []

        allNames = nub $ sort $ map fst nameLocs
        usedNames = nub $ sort $ concatMap nameForDecl ds
        unusedNames = allNames \\ usedNames
        unusedErrors = map (\ n ->
                case lookup n nameLocs of
                    Just loc -> unusedTypeAnnotationsError n loc
                    _ -> panic "attachTypeAnnotations: logic error"
            ) unusedNames

        typeAnMap = M.fromList extractedAnnotations
            
        annotationForName n =
            M.findWithDefault (panic "attachedTypeAnnotations: invalid state")
                n typeAnMap

        annotateDecl :: PDecl -> [PDecl]
        annotateDecl (An x y (FunBind n ms Nothing)) | M.member n typeAnMap =
            [An x y (FunBind n ms (Just $ annotationForName n))]
        annotateDecl (An x y (PatBind (p@(An _ _ (PVar n))) e Nothing))
                | M.member n typeAnMap =
            [An x y (PatBind p e (Just $ annotationForName n))]
        annotateDecl (An _ _ (ParsedTypeAnnotation _ _)) = []
        annotateDecl d = [d]
    in if length dupeErrors > 0 then
            throwSourceError dupeErrors
        else if length unusedErrors > 0 then
            throwSourceError unusedErrors
        else concatMap annotateDecl ds

convDecl :: PExp -> PExp -> ParseMonad PDecl
convDecl (lhs @ (An loc1 b lhsexp)) (rhs @ (An loc2 d _)) = 
    let
        span = combineSpans loc1 loc2

        -- REMEMBER: needs to reverse pts
        getPats :: Exp UnRenamedName -> ([[PPat]], UnRenamedName)
        getPats (App f args) = 
                ((map convPat args):ps, n)
            where
                (ps, n) = getPats (unAnnotate f)
        getPats (Var n) = ([], n)
        
        convFunBind exp = 
                FunBind n [An span (dummyAnnotation) (Match (reverse ps) rhs)]
                    Nothing
            where
                (ps, n) = getPats exp

        convPatBind exp = PatBind (convPat exp) rhs Nothing
    in do
        symbTable <- freshPSymbolTable
        case lhsexp of
            App f args  -> return $ An span (Nothing, symbTable) (convFunBind lhsexp)
            _           -> return $ An span (Nothing, symbTable) (convPatBind lhs)

-- | Throws an error if a declaration that is not allowed inside a let 
-- expression is found.
checkLetDecls :: [PDecl] -> [PDecl]
checkLetDecls decls =
    if and (map checkDecl decls) then
        decls
    else panic "checkLetDecls: Invalid state."
    where
        checkDecl :: PDecl -> Bool
        checkDecl (anDecl@(An _ _ decl)) =
            let
                check (FunBind _ _ _) = True
                check (PatBind _ _ _) = True
                check (External a) = True
                check (Transparent a) = True
                check (TimedSection _ _ ds) = and (map checkDecl ds)
                -- We can't allow module instances as this would cause channels
                -- to be declared in let statements, which would lead to Events
                -- being hard to define
                -- check (ModuleInstance _ _ _ _ _) = True
                check _ = throwSourceError [invalidLetDeclarationErrorMessage anDecl]
            in check decl
    
checkModuleDecl :: PDecl -> PDecl
checkModuleDecl (anDecl@(An _ _ (Assert _))) =
    throwSourceError [invalidModuleDeclarationErrorMessage anDecl]
checkModuleDecl (anDecl@(An x y (TimedSection a b ds))) =
    An x y (TimedSection a b (map checkModuleDecl ds))
checkModuleDecl d = d

checkTimedDecl :: PDecl -> PDecl
checkTimedDecl d = d

checkExp :: PExp -> PExp
checkExp (anExp@(An a b exp)) =
    let 
        check :: Exp UnRenamedName -> Exp UnRenamedName
        check (App e es) = App (checkExp e) (map checkExp es)
        check (BooleanBinaryOp op e1 e2) = BooleanBinaryOp op (checkExp e1) (checkExp e2)
        check (BooleanUnaryOp op e) = BooleanUnaryOp op (checkExp e)
        check (Concat e1 e2) = Concat (checkExp e1) (checkExp e2)
        check (DotApp e1 e2) = DotApp (checkExp e1) (checkExp e2)
        check (If e1 e2 e3) = If (checkExp e1) (checkExp e2) (checkExp e3)
        check (Lambda p e) = Lambda p (checkExp e)
        check (Let decls e) = Let decls (checkExp e)
        check (Lit lit) = Lit lit
        check (List es) = List (map checkExp es)
        check (ListComp es stmts) = ListComp (map checkExp es) stmts
        check (ListEnumFrom e) = ListEnumFrom (checkExp e)
        check (ListEnumFromTo e1 e2) = ListEnumFromTo (checkExp e1) (checkExp e2)
        check (ListEnumFromComp e1 stmts) = ListEnumFromComp (checkExp e1) stmts
        check (ListEnumFromToComp e1 e2 stmts) =
            ListEnumFromToComp (checkExp e1) (checkExp e2) stmts
        check (ListLength e) = ListLength (checkExp e)
        check (MathsBinaryOp op e1 e2) = MathsBinaryOp op (checkExp e1) (checkExp e2)
        check (MathsUnaryOp op e) = MathsUnaryOp op (checkExp e)
        check (Paren e) = Paren (checkExp e)
        check (Set es) = Set (map checkExp es)
        check (SetComp es stmts) = SetComp (map checkExp es) stmts
        check (SetEnumFrom e) = SetEnumFrom (checkExp e)
        check (SetEnumFromTo e1 e2) = SetEnumFromTo (checkExp e1) (checkExp e2)
        check (SetEnumFromComp e1 stmts) = SetEnumFromComp (checkExp e1) stmts
        check (SetEnumFromToComp e1 e2 stmts) =
            SetEnumFromToComp (checkExp e1) (checkExp e2) stmts
        check (SetEnum es) = SetEnum (map checkExp es)
        -- We don't need to check inside stmts as they will have been checked
        -- upon creation
        check (SetEnumComp es stmts) = SetEnumComp (map checkExp es) stmts
        check (Tuple es) = Tuple (map checkExp es)
        check (Var qname) = Var qname

        check (AlphaParallel e1 e2 e3 e4) = 
            AlphaParallel (checkExp e1) (checkExp e2) (checkExp e3) (checkExp e4)
        check (Exception e1 e2 e3) = Exception (checkExp e1) (checkExp e2) (checkExp e3)
        check (ExternalChoice e1 e2) = ExternalChoice (checkExp e1) (checkExp e2)
        check (GenParallel e1 e2 e3) = GenParallel (checkExp e1) (checkExp e2) (checkExp e3)
        check (GuardedExp e1 e2) = GuardedExp (checkExp e1) (checkExp e2)
        check (Hiding e1 e2) = Hiding (checkExp e1) (checkExp e2)
        check (InternalChoice e1 e2) = InternalChoice (checkExp e1) (checkExp e2)
        check (Interrupt e1 e2) = Interrupt (checkExp e1) (checkExp e2)
        check (Interleave e1 e2) = Interleave (checkExp e1) (checkExp e2)
        check (LinkParallel e1 ties stmts e2) = 
            LinkParallel (checkExp e1) ties stmts (checkExp e2)
        check (Prefix e1 fields e2) = Prefix (checkExp e1) fields (checkExp e2)
        check (Rename e ties stmts) = Rename (checkExp e) ties stmts
        check (SequentialComp e1 e2) = SequentialComp (checkExp e1) (checkExp e2)
        check (SlidingChoice e1 e2) = SlidingChoice (checkExp e1) (checkExp e2)
        check (SynchronisingExternalChoice e1 e2 e3) =
            SynchronisingExternalChoice (checkExp e1) (checkExp e2) (checkExp e3)
        check (SynchronisingInterrupt e1 e2 e3) =
            SynchronisingInterrupt (checkExp e1) (checkExp e2) (checkExp e3)

        check (ReplicatedAlphaParallel stmts e1 e2) = 
            ReplicatedAlphaParallel stmts (checkExp e1) (checkExp e2)
        check (ReplicatedInterleave stmts e1) = ReplicatedInterleave stmts (checkExp e1)
        check (ReplicatedExternalChoice stmts e1) = ReplicatedExternalChoice stmts (checkExp e1)
        check (ReplicatedInternalChoice stmts e1) = ReplicatedInternalChoice stmts (checkExp e1)
        check (ReplicatedParallel e1 stmts e2) = 
            ReplicatedParallel (checkExp e1) stmts (checkExp e2)
        check (ReplicatedLinkParallel ties tiesStmts stmts e) = 
            ReplicatedLinkParallel ties tiesStmts stmts (checkExp e)
        check (ReplicatedSequentialComp stmts e1) = 
            ReplicatedSequentialComp stmts (checkExp e1)
        check (ReplicatedSynchronisingExternalChoice e1 stmts e3) =
            ReplicatedSynchronisingExternalChoice (checkExp e1) stmts
                (checkExp e3)
        
        check x = throwSourceError [invalidExpressionErrorMessage anExp]
    in
        An a b (check exp)

getFuncArgs :: PSType -> [PSType]
getFuncArgs (An _ _ (STParen s)) = [s]
getFuncArgs (An _ _ (STTuple ts)) = ts
getFuncArgs s = throwSourceError [invalidFunctionArgsErrorMessage s]

dotAppToList :: PExp -> [PExp]
dotAppToList (An a b exp) = 
    let 
        list :: Exp UnRenamedName -> [PExp]
        list (DotApp e1 e2) = (dotAppToList e1) ++ (dotAppToList e2)
        list x = [An a b x]
    in
        list exp

convPat :: PExp -> PPat
convPat (anExp@ (An a b exp)) = 
    let
        trans :: Exp UnRenamedName -> Pat UnRenamedName
        trans (Concat e1 e2) = PConcat (convPat e1) (convPat e2)
        trans (DotApp e1 e2) = PDotApp (convPat e1) (convPat e2)
        trans (List xs) = PList (map convPat xs)
        trans (Lit x) = PLit x
        trans (MathsUnaryOp Negate e) =
            case trans (unAnnotate e) of
                PLit (Int x) -> PLit (Int (-x))
                _ -> throwSourceError [invalidPatternErrorMessage anExp]
        trans (Set xs) = PSet (map convPat xs)
        trans (Paren x) = PParen (convPat x)
        trans (Tuple xs) = PTuple (map convPat xs)
        trans (Var x) = PVar x
        trans (ExpPatWildCard) = PWildCard
        trans (ExpPatDoublePattern e1 e2) = 
            PDoublePattern (convPat e1) (convPat e2)
        trans x = throwSourceError [invalidPatternErrorMessage anExp]
    in
        An a () (trans exp)

-- Helper function to get the contents of tokens
getInt (L _ (TInteger x)) = x
getChar (L _ (TChar c)) = c
getString (L _ (TString s)) = s
getName (L _ (TIdent x)) = x
getRefinesModel (L _ (TRefines x)) = x
getPropModel (L _ (TModel x)) = x

class Locatable a where
    getLoc :: a b -> SrcSpan
    unLoc :: a b -> b
    mkLoc :: SrcSpan -> b -> a b

instance Locatable Located where
    getLoc (L loc _) = loc
    unLoc (L _ b) = b
    mkLoc loc b = L loc b

instance Locatable (Annotated a) where
    getLoc (An loc _ _) = loc
    unLoc (An _ _ b) = b
    mkLoc loc b = An loc dummyAnnotation b

annotate :: (Locatable t1, Locatable t2) => t1 a -> b -> t2 b
annotate t1 b = mkLoc (getLoc t1) b

annotateList :: (Locatable t1, Locatable t2) => [t1 a] -> b -> t2 b
annotateList [] b = mkLoc Unknown b
annotateList ts b = mkLoc (combineSpans (getLoc (head ts)) (getLoc (last ts))) b

annotate2 :: 
    (Locatable t1, Locatable t2, Locatable t3) => t1 a -> t2 b -> c -> t3 c
annotate2 t1 t2 b = mkLoc (combineSpans (getLoc t1) (getLoc t2)) b
annotate2List :: 
    (Locatable t1, Locatable t2, Locatable t3) => t1 a -> [t2 b] -> c -> t3 c
annotate2List t1 t2 b = annotate2 t1 (last t2) b
annotate2List' :: 
    (Locatable t1, Locatable t2, Locatable t3) => [t1 a] -> t2 b -> c -> t3 c
annotate2List' t1 t2 b = annotate2 (last t1) t2 b

annotateWithSymbolTable 
    :: Annotated (Maybe SymbolTable, PSymbolTable) a -> ParseMonad (Annotated (Maybe SymbolTable, PSymbolTable) a)
annotateWithSymbolTable (An l _ a) = do
    symbTable <- freshPSymbolTable
    return $ An l (Nothing, symbTable) a

liftLoc :: (Locatable t1, Locatable t2) => t1 a -> b -> t2 b
liftLoc t1 b = mkLoc (getLoc t1) b

parseError :: LToken -> ParseMonad a
parseError tok = throwSourceError [parseErrorMessage tok]
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n Happy_GHC_Exts.<# (0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where (new_state) = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where (off)    = indexShortOffAddr happyActOffsets st
         (off_i)  = (off Happy_GHC_Exts.+# i)
	 check  = if (off_i Happy_GHC_Exts.>=# (0# :: Happy_GHC_Exts.Int#))
			then (indexShortOffAddr happyCheck off_i Happy_GHC_Exts.==#  i)
			else False
         (action)
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

{-# LINE 130 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
	Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 163 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             (off) = indexShortOffAddr happyGotoOffsets st1
             (off_i) = (off Happy_GHC_Exts.+# nt)
             (new_state) = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where (off) = indexShortOffAddr happyGotoOffsets st
         (off_i) = (off Happy_GHC_Exts.+# nt)
         (new_state) = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
