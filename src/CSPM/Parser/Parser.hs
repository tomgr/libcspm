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

newtype HappyAbsSyn t8 t9 t28 t29 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn6 :: (PCSPMFile) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t8 t9 t28 t29) -> (PCSPMFile)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (PInteractiveStmt) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn t8 t9 t28 t29) -> (PInteractiveStmt)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: t8 -> (HappyAbsSyn t8 t9 t28 t29)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t8 t9 t28 t29) -> t8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: t9 -> (HappyAbsSyn t8 t9 t28 t29)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t8 t9 t28 t29) -> t9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([PDecl]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PDecl])
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([PDecl]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PDecl])
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([PDecl]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PDecl])
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Maybe PDecl) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn t8 t9 t28 t29) -> (Maybe PDecl)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (PDecl) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn t8 t9 t28 t29) -> (PDecl)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (PDecl) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn t8 t9 t28 t29) -> (PDecl)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (PAssertion) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn t8 t9 t28 t29) -> (PAssertion)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Located SemanticProperty) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn t8 t9 t28 t29) -> (Located SemanticProperty)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Located (ModelOption UnRenamedName)) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn t8 t9 t28 t29) -> (Located (ModelOption UnRenamedName))
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (PDataTypeClause) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn t8 t9 t28 t29) -> (PDataTypeClause)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: ([PDataTypeClause]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PDataTypeClause])
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([PDataTypeClause]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PDataTypeClause])
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Located UnRenamedName) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn t8 t9 t28 t29) -> (Located UnRenamedName)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Located UnRenamedName) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn t8 t9 t28 t29) -> (Located UnRenamedName)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ([Located UnRenamedName]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn t8 t9 t28 t29) -> ([Located UnRenamedName])
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ([Located UnRenamedName]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn t8 t9 t28 t29) -> ([Located UnRenamedName])
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Located Literal) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn t8 t9 t28 t29) -> (Located Literal)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (PPat) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn t8 t9 t28 t29) -> (PPat)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: t28 -> (HappyAbsSyn t8 t9 t28 t29)
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn t8 t9 t28 t29) -> t28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: t29 -> (HappyAbsSyn t8 t9 t28 t29)
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn t8 t9 t28 t29) -> t29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (PExp) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn t8 t9 t28 t29) -> (PExp)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (PExp) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn t8 t9 t28 t29) -> (PExp)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (PExp) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn t8 t9 t28 t29) -> (PExp)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (PField) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn t8 t9 t28 t29) -> (PField)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ((PExp, PExp)) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn t8 t9 t28 t29) -> ((PExp, PExp))
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: ([(PExp, PExp)]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn t8 t9 t28 t29) -> ([(PExp, PExp)])
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ([(PExp, PExp)]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn t8 t9 t28 t29) -> ([(PExp, PExp)])
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: ((PExp, PExp)) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn t8 t9 t28 t29) -> ((PExp, PExp))
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ([(PExp, PExp)]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn t8 t9 t28 t29) -> ([(PExp, PExp)])
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ([(PExp, PExp)]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn t8 t9 t28 t29) -> ([(PExp, PExp)])
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: ([PField]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PField])
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: ([PField]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PField])
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: ([PExp]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PExp])
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: ([PExp]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PExp])
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ([PExp]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PExp])
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: ([PExp]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PExp])
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: ([PStmt]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PStmt])
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (PStmt) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn t8 t9 t28 t29) -> (PStmt)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: ([PStmt]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PStmt])
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: ([PStmt]) -> (HappyAbsSyn t8 t9 t28 t29)
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn t8 t9 t28 t29) -> ([PStmt])
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyInTok :: (LToken) -> (HappyAbsSyn t8 t9 t28 t29)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t8 t9 t28 t29) -> (LToken)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x09\x01\x8e\x02\x00\x00\x59\x00\x00\x00\x00\x00\x8e\x02\x36\x01\x4c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x01\x00\x00\x00\x00\x00\x00\x8e\x02\xb1\x00\x8e\x02\x8e\x02\x00\x00\x00\x00\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x2a\x01\x00\x00\xb1\x00\x7f\x01\x7f\x01\x69\x02\x20\x01\x00\x00\x1a\x02\x8e\x02\x00\x00\x00\x00\x71\x01\x00\x00\x00\x00\x5f\x01\x72\x01\x00\x00\x00\x00\x2e\x03\x70\x01\x70\x01\x70\x01\x70\x01\x70\x01\x70\x01\x70\x01\xcb\xff\x11\x02\x9e\x07\x24\x00\xe7\x04\x00\x00\x57\x01\x68\x01\x52\x01\x50\x01\x42\x01\x41\x01\x58\x01\x00\x00\x49\x01\x51\x01\x64\x07\x4c\x08\xef\xff\x4b\x01\x2a\x07\xf7\xff\x68\x03\x0f\x01\xe4\xff\x24\x04\x04\x0b\x35\x01\x4c\x08\xdd\x02\xd9\x06\x4d\x01\x00\x00\xfb\x00\x07\x01\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x88\x06\xf8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x8e\x02\x00\x00\xb9\x01\xa2\x03\x37\x06\xc2\x0a\xc2\x0a\x89\x0a\x50\x0a\xc2\x0a\x6b\x09\xde\x09\x17\x0a\xfd\x05\x3c\x01\x96\x04\x39\x01\x00\x00\x32\x01\x3a\x01\xff\x00\xcc\xff\xe4\xff\xe4\xff\xe4\xff\x24\x04\x24\x04\xf3\x0a\xf3\x0a\xf3\x0a\xf3\x0a\xf3\x0a\xf3\x0a\xd7\x03\xe2\x0a\x85\x08\x12\x08\xa5\x09\x32\x09\xf8\x08\x4c\x01\x00\x00\x8e\x02\x00\x00\x8e\x02\x8e\x02\x8e\x02\x00\x00\x8e\x02\x00\x00\x8e\x02\x00\x00\x61\x01\x8e\x02\x8e\x02\x00\x00\x8e\x02\x8e\x02\xef\x00\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\xee\x00\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x8e\x02\x00\x00\x8e\x02\x8e\x02\xb1\x00\x2f\x01\x26\x01\x00\x00\x00\x00\x1f\x01\x17\x01\x0a\x01\x8e\x02\x01\x00\x00\x00\x8e\x02\x14\x01\x00\x00\xd7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8e\x02\xdc\x00\x00\x00\x00\x00\xd8\x07\x00\x00\x00\x00\x00\x00\xb2\x00\x8e\x02\xf0\x00\xf0\x00\x8e\x02\xb1\x00\xb7\x00\xbb\x00\x00\x00\xdb\x00\xd8\x07\xd8\x07\xd8\x07\x00\x00\x8e\x02\xd8\x07\xd8\x07\xd8\x07\x00\x00\x00\x00\x8e\x02\x00\x00\xd6\x00\xa4\x00\xd8\x07\x45\x04\x8e\x02\x00\x00\xa5\x00\xd9\x00\xd8\x07\x00\x00\xc3\x05\xc2\x0a\x8e\x02\x8e\x02\x00\x00\x8e\x02\x9b\x00\x8e\x02\x8e\x02\x8e\x02\x8f\x00\x8e\x02\x8e\x02\x8e\x02\x00\x00\xf4\x03\x8e\x02\x9c\x00\x00\x00\x9c\x00\x00\x00\x8e\x02\x50\x0a\x17\x0a\x72\x05\x8e\x02\xbe\x08\xbe\x08\x00\x00\x00\x00\x00\x00\xa5\x09\xa5\x09\x8e\x02\x00\x00\x87\x00\x8e\x02\x00\x00\x00\x00\x8e\x02\xb9\x00\x21\x05\x8e\x02\x7d\x00\x00\x00\x9a\x00\x00\x00\x00\x00\x00\x00\xa9\x00\xaf\x00\x00\x00\x00\x00\x00\x00\x8e\x02\x00\x00\x8e\x02\xac\x00\xa1\x00\xb1\x00\xd8\x07\x8e\x02\x8e\x02\xd8\x07\x68\x00\x00\x00\xd8\x07\xbe\x08\x8e\x02\x62\x00\x00\x00\x00\x00\xbe\x08\x00\x00\xd8\x07\xbe\x08\x54\x00\xb1\x00\x00\x00\x00\x00\x69\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x41\x00\x51\x00\x91\x06\x89\x00\x36\x07\x00\x00\x00\x00\x37\x03\x00\x00\xf0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\x0b\x94\x0a\xae\x00\xec\x0b\x00\x00\x00\x00\xe5\x0b\xde\x0b\xa6\x00\xe6\x02\x66\x02\xd7\x0b\xad\x03\x8a\x06\x74\x05\xe9\x04\x98\x04\x30\x03\xd0\x0b\x00\x00\x00\x00\x5c\x0a\xf1\x00\xe7\x00\xf6\x04\x00\x00\x00\x00\x79\x00\xa5\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\xf0\xff\x99\x00\x6b\x00\x63\x00\x81\x00\x42\x00\x61\x00\x5f\x00\x00\x00\x56\x01\xf0\xff\x00\x00\xf0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x00\x00\xf0\xff\xf0\xff\x00\x00\x00\x00\xf0\xff\x00\x00\xf0\xff\x00\x00\xf0\xff\xf0\xff\xf0\xff\x00\x00\xf0\xff\xf0\xff\xf0\xff\x5c\x00\x00\x00\x00\x00\x4d\x00\xc9\x0b\xc2\x0b\xbb\x0b\xb4\x0b\xad\x0b\xa6\x0b\x9f\x0b\x98\x0b\x91\x0b\x8a\x0b\x83\x0b\x7c\x0b\x75\x0b\x6e\x0b\x67\x0b\x60\x0b\x59\x0b\x52\x0b\x4b\x0b\x44\x0b\xc6\x01\x2c\x05\x3d\x0b\x73\x03\x35\x0b\xf5\x0a\xd5\x0a\xce\x0a\xc3\x0a\x9b\x0a\x63\x0a\x2a\x0a\x23\x0a\xf1\x09\xf0\xff\x35\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdf\x02\x00\x00\x46\x06\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\x30\x00\xf0\xff\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\xf0\xff\x00\x00\xea\x09\x00\x00\xb7\x09\x0f\x06\xb0\x09\x00\x00\x21\x00\x00\x00\x5e\x02\x00\x00\x7e\x09\x77\x09\x0e\x02\x00\x00\x06\x02\xcd\x00\x00\x00\xcd\x01\x08\x06\x44\x09\x3d\x09\x0a\x09\x00\x00\x75\x00\x03\x09\xd1\x08\xca\x08\xae\x01\x00\x00\x25\x01\xd6\x05\xe5\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x05\xb3\x02\x00\x00\x98\x08\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\x05\x3d\x00\x00\x00\x00\x00\xf0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x37\x05\x93\x00\x55\x00\x3e\x03\xcf\x05\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\xf0\xff\xf0\xff\x00\x00\x91\x08\xf0\xff\xf0\xff\xf0\xff\x00\x00\x00\x00\x1d\x01\x00\x00\x00\x00\x00\x00\xf0\xff\xf0\xff\x01\x01\x00\x00\x00\x00\x00\x00\xf0\xff\x00\x00\xf0\xff\xf0\xff\x5e\x08\x57\x08\x00\x00\x08\x04\x00\x00\xed\x02\x24\x08\x1d\x08\x00\x00\xe9\x07\xb0\x07\xa9\x07\x00\x00\xf5\xff\xc5\x00\x05\x00\x00\x00\x03\x00\x00\x00\x6d\x00\xf0\xff\xf0\xff\xf0\xff\x76\x07\xf0\xff\xf0\xff\x00\x00\x00\x00\x00\x00\xf0\xff\xf0\xff\x6f\x07\x00\x00\x00\x00\x15\x00\x00\x00\x00\x00\x3d\x07\x00\x00\xf0\xff\xec\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7d\x01\x00\x00\x0c\x01\x39\x00\x00\x00\x55\x04\xf0\xff\x98\x06\xd5\x01\xf0\xff\x00\x00\x00\x00\xf0\xff\xf0\xff\xb6\x01\xfa\xff\x00\x00\x00\x00\xf0\xff\x00\x00\xf0\xff\xf0\xff\x00\x00\x36\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xf3\xff\x00\x00\x00\x00\x00\x00\xf1\xff\xbd\xff\xbe\xff\x68\xff\x00\x00\xc0\xff\xbf\xff\xc8\xff\xc7\xff\xc6\xff\xce\xff\xc4\xff\xc5\xff\x95\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa6\xff\xc2\xff\x00\x00\x00\x00\x00\x00\x68\xff\x68\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\x00\x00\xca\xff\xf7\xff\xcb\xff\xcc\xff\xf8\xff\x00\x00\xef\xff\xee\xff\xea\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\xff\x5d\xff\x00\x00\x5f\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\xff\x64\xff\x70\xff\x00\x00\x65\xff\x00\x00\x67\xff\x65\xff\x00\x00\x00\x00\x00\x00\x9e\xff\xac\xff\xaf\xff\x00\x00\xc3\xff\x00\x00\x00\x00\x00\x00\x6c\xff\x00\x00\x6d\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\xff\x00\x00\xfc\xff\xf0\xff\xf2\xff\xa5\xff\x00\x00\xc1\xff\x00\x00\x00\x00\x00\x00\x87\xff\x88\xff\x89\xff\x8b\xff\x92\xff\x8e\xff\x8f\xff\x91\xff\xc0\xff\x64\xff\x00\x00\x00\x00\x73\xff\x64\xff\x74\xff\x00\x00\x9f\xff\xa9\xff\xa8\xff\xa7\xff\xaa\xff\xab\xff\xb2\xff\xb3\xff\xb0\xff\xb1\xff\xb4\xff\xb5\xff\xae\xff\xad\xff\x90\xff\x94\xff\x76\xff\x78\xff\x7a\xff\xbb\xff\x6b\xff\x00\x00\xcd\xff\x00\x00\x00\x00\x00\x00\xbc\xff\x00\x00\xb8\xff\x00\x00\x9d\xff\x00\x00\x00\x00\x00\x00\x97\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\x00\x00\x00\x00\xf1\xff\x00\x00\x00\x00\xe3\xff\xe4\xff\x00\x00\x00\x00\xe8\xff\x00\x00\xec\xff\xf5\xff\x00\x00\x00\x00\xfa\xff\xda\xff\xd8\xff\xd7\xff\xd6\xff\xd5\xff\x00\x00\xdc\xff\xd9\xff\xc9\xff\xba\xff\xed\xff\xeb\xff\xf4\xff\xe9\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf1\xff\x00\x00\x00\x00\xe2\xff\x00\x00\x62\xff\x61\xff\x7c\xff\x5e\xff\x00\x00\x82\xff\x80\xff\x81\xff\x71\xff\x63\xff\x00\x00\x6e\xff\x00\x00\x00\x00\x66\xff\x00\x00\x00\x00\x9b\xff\x00\x00\x6a\xff\xb6\xff\xe9\xff\x00\x00\x93\xff\x00\x00\x00\x00\xb7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\xff\x00\x00\x00\x00\x00\x00\xa4\xff\x00\x00\xa2\xff\x00\x00\x85\xff\x86\xff\x00\x00\x00\x00\x8a\xff\x8d\xff\x75\xff\x84\xff\x72\xff\x77\xff\x79\xff\x00\x00\x9c\xff\x00\x00\x00\x00\x9a\xff\x96\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\x00\x00\xe0\xff\xd0\xff\xe5\xff\xd1\xff\xd2\xff\xe6\xff\xe7\xff\xdb\xff\x00\x00\xd4\xff\x00\x00\x00\x00\x00\x00\xf1\xff\x7b\xff\x00\x00\x00\x00\x7e\xff\x00\x00\x99\xff\xb9\xff\x83\xff\x00\x00\x00\x00\xa1\xff\xa0\xff\x8c\xff\x98\xff\x7d\xff\x7f\xff\x00\x00\xf1\xff\xcf\xff\xd3\xff\x00\x00\xdd\xff\xdf\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x12\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x12\x00\x12\x00\x1b\x00\x17\x00\x42\x00\x42\x00\x44\x00\x1b\x00\x17\x00\x22\x00\x23\x00\x48\x00\x14\x00\x15\x00\x22\x00\x23\x00\x18\x00\x17\x00\x1a\x00\x17\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x40\x00\x10\x00\x42\x00\x26\x00\x27\x00\x14\x00\x15\x00\x16\x00\x48\x00\x11\x00\x19\x00\x1a\x00\x2f\x00\x10\x00\x3a\x00\x0f\x00\x10\x00\x14\x00\x47\x00\x16\x00\x37\x00\x38\x00\x19\x00\x1a\x00\x45\x00\x3c\x00\x29\x00\x2a\x00\x2b\x00\x00\x00\x41\x00\x42\x00\x03\x00\x44\x00\x0d\x00\x46\x00\x27\x00\x0c\x00\x11\x00\x4a\x00\x17\x00\x4c\x00\x28\x00\x4e\x00\x4f\x00\x50\x00\x01\x00\x11\x00\x12\x00\x13\x00\x55\x00\x56\x00\x28\x00\x58\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x10\x00\x0d\x00\x0e\x00\x0f\x00\x14\x00\x11\x00\x16\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x10\x00\x14\x00\x15\x00\x28\x00\x11\x00\x18\x00\x11\x00\x1a\x00\x11\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x11\x00\x10\x00\x02\x00\x26\x00\x27\x00\x14\x00\x15\x00\x16\x00\x0b\x00\x10\x00\x19\x00\x1a\x00\x2f\x00\x14\x00\x15\x00\x16\x00\x03\x00\x24\x00\x19\x00\x1a\x00\x37\x00\x38\x00\x11\x00\x12\x00\x13\x00\x3c\x00\x29\x00\x2a\x00\x2b\x00\x45\x00\x41\x00\x42\x00\x3a\x00\x44\x00\x29\x00\x46\x00\x0d\x00\x0e\x00\x0f\x00\x4a\x00\x11\x00\x4c\x00\x05\x00\x4e\x00\x4f\x00\x50\x00\x11\x00\x12\x00\x13\x00\x45\x00\x55\x00\x56\x00\x04\x00\x58\x00\x01\x00\x02\x00\x03\x00\x04\x00\x10\x00\x06\x00\x07\x00\x0a\x00\x14\x00\x12\x00\x16\x00\x23\x00\x10\x00\x19\x00\x1a\x00\x44\x00\x14\x00\x15\x00\x16\x00\x14\x00\x15\x00\x19\x00\x1a\x00\x18\x00\x24\x00\x1a\x00\x45\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x1b\x00\x10\x00\x3a\x00\x26\x00\x27\x00\x14\x00\x15\x00\x16\x00\x4d\x00\x10\x00\x19\x00\x1a\x00\x2f\x00\x14\x00\x09\x00\x16\x00\x49\x00\x18\x00\x19\x00\x1a\x00\x37\x00\x38\x00\x45\x00\x47\x00\x1f\x00\x3c\x00\x29\x00\x2a\x00\x2b\x00\x1b\x00\x41\x00\x42\x00\x04\x00\x44\x00\x1b\x00\x46\x00\x11\x00\x12\x00\x13\x00\x4a\x00\x45\x00\x4c\x00\x43\x00\x4e\x00\x4f\x00\x50\x00\x11\x00\x12\x00\x13\x00\x2e\x00\x55\x00\x56\x00\x2c\x00\x58\x00\x01\x00\x02\x00\x03\x00\x04\x00\x5c\x00\x06\x00\x07\x00\x10\x00\x0b\x00\x0c\x00\x0d\x00\x14\x00\x15\x00\x16\x00\x04\x00\x0f\x00\x19\x00\x1a\x00\x10\x00\x14\x00\x15\x00\x08\x00\x14\x00\x18\x00\x16\x00\x1a\x00\x18\x00\x19\x00\x1a\x00\x08\x00\x1f\x00\x20\x00\x29\x00\x2a\x00\x2b\x00\x10\x00\x08\x00\x0a\x00\x27\x00\x14\x00\x15\x00\x16\x00\x05\x00\x10\x00\x19\x00\x1a\x00\x2f\x00\x14\x00\x4c\x00\x16\x00\x4d\x00\x18\x00\x19\x00\x1a\x00\x37\x00\x38\x00\x43\x00\x09\x00\x12\x00\x3c\x00\x29\x00\x2a\x00\x2b\x00\x10\x00\x41\x00\x42\x00\x51\x00\x44\x00\x12\x00\x46\x00\x1b\x00\x04\x00\x43\x00\x4a\x00\x09\x00\x4c\x00\x0a\x00\x4e\x00\x4f\x00\x50\x00\x09\x00\x12\x00\x1b\x00\x1b\x00\x55\x00\x56\x00\x0a\x00\x58\x00\x01\x00\x02\x00\x03\x00\x04\x00\x10\x00\x06\x00\x07\x00\x11\x00\x14\x00\x1b\x00\x16\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x09\x00\x1b\x00\x12\x00\x04\x00\x14\x00\x15\x00\x05\x00\x19\x00\x18\x00\x09\x00\x1a\x00\x5c\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x04\x00\x38\x00\x39\x00\x5c\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x10\x00\x42\x00\x25\x00\x2f\x00\x14\x00\x5c\x00\x16\x00\x48\x00\x18\x00\x19\x00\x1a\x00\x37\x00\x38\x00\xff\xff\xff\xff\xff\xff\x3c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\xff\xff\x44\x00\x45\x00\x46\x00\xff\xff\xff\xff\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\xff\xff\xff\xff\xff\xff\xff\xff\x55\x00\x56\x00\xff\xff\x58\x00\x01\x00\x02\x00\x03\x00\x04\x00\x10\x00\x06\x00\x07\x00\xff\xff\x14\x00\x15\x00\x16\x00\xff\xff\x10\x00\x19\x00\x1a\x00\xff\xff\x14\x00\x12\x00\x16\x00\x14\x00\x15\x00\x19\x00\x1a\x00\x18\x00\xff\xff\x1a\x00\xff\xff\xff\xff\x10\x00\x29\x00\x2a\x00\x2b\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\x15\x00\x16\x00\xff\xff\x10\x00\x19\x00\x1a\x00\x2f\x00\x14\x00\xff\xff\x16\x00\x26\x00\x27\x00\x19\x00\x1a\x00\x37\x00\x38\x00\xff\xff\x3a\x00\xff\xff\x3c\x00\x29\x00\x2a\x00\x2b\x00\xff\xff\x41\x00\x42\x00\xff\xff\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\xff\xff\xff\xff\xff\xff\xff\xff\x55\x00\x56\x00\xff\xff\x58\x00\x01\x00\x02\x00\x03\x00\x04\x00\x10\x00\x06\x00\x07\x00\xff\xff\x14\x00\x15\x00\x16\x00\xff\xff\x10\x00\x19\x00\x1a\x00\xff\xff\x14\x00\x15\x00\x16\x00\x14\x00\x15\x00\x19\x00\x1a\x00\x18\x00\xff\xff\x1a\x00\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x04\x00\x05\x00\x06\x00\xff\xff\x08\x00\x09\x00\x2f\x00\x30\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x10\x00\x2d\x00\x37\x00\x38\x00\x14\x00\xff\xff\x16\x00\x3c\x00\xff\xff\x19\x00\x1a\x00\xff\xff\x41\x00\x42\x00\xff\xff\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\xff\xff\xff\xff\xff\xff\xff\xff\x55\x00\x56\x00\xff\xff\x58\x00\x01\x00\x02\x00\x03\x00\x04\x00\x10\x00\x06\x00\x07\x00\xff\xff\x14\x00\x15\x00\x16\x00\xff\xff\x10\x00\x19\x00\x1a\x00\xff\xff\x14\x00\xff\xff\x16\x00\x14\x00\x15\x00\x19\x00\x1a\x00\x18\x00\xff\xff\x1a\x00\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\xff\xff\xff\xff\x26\x00\x27\x00\xff\xff\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x06\x00\x07\x00\xff\xff\xff\xff\x2f\x00\x30\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x37\x00\x38\x00\x14\x00\x15\x00\xff\xff\x3c\x00\x18\x00\xff\xff\x1a\x00\xff\xff\x41\x00\x42\x00\xff\xff\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x07\x00\x08\x00\x09\x00\x2f\x00\x55\x00\x56\x00\xff\xff\x58\x00\xff\xff\x10\x00\xff\xff\x37\x00\x38\x00\x14\x00\xff\xff\x16\x00\x3c\x00\xff\xff\x19\x00\x1a\x00\xff\xff\x41\x00\x42\x00\xff\xff\x44\x00\xff\xff\x46\x00\xff\xff\xff\xff\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\xff\xff\xff\xff\xff\xff\xff\xff\x55\x00\x56\x00\x08\x00\x58\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\x13\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x10\x00\x1a\x00\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\x18\x00\x19\x00\x1a\x00\x29\x00\x2a\x00\x2b\x00\xff\xff\x26\x00\x27\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xff\xff\xff\xff\x56\x00\x57\x00\x58\x00\x08\x00\x5a\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\x13\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x10\x00\x1a\x00\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\x18\x00\x19\x00\x1a\x00\x29\x00\x2a\x00\x2b\x00\xff\xff\x26\x00\x27\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x1a\x00\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\xff\xff\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1f\x00\x20\x00\x21\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\x43\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x1a\x00\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\xff\xff\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1f\x00\x20\x00\x21\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xff\xff\xff\xff\x56\x00\x57\x00\x58\x00\xff\xff\x5a\x00\x5b\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x31\x00\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\x1a\x00\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x10\x00\x42\x00\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\x48\x00\x18\x00\x19\x00\x1a\x00\xff\xff\x1c\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xff\xff\xff\xff\x56\x00\x57\x00\x58\x00\xff\xff\x5a\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x04\x00\x05\x00\x06\x00\xff\xff\x08\x00\x09\x00\x1a\x00\xff\xff\x3d\x00\x3e\x00\x3f\x00\x40\x00\x10\x00\x42\x00\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\x48\x00\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\xff\xff\x45\x00\xff\xff\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xff\xff\xff\xff\x56\x00\x57\x00\x58\x00\xff\xff\x5a\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\x13\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x0a\x00\x1a\x00\x19\x00\x1a\x00\xff\xff\xff\xff\x10\x00\xff\xff\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\xff\xff\x18\x00\x19\x00\x1a\x00\xff\xff\x29\x00\x2a\x00\x2b\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x48\x00\xff\xff\x4a\x00\x4b\x00\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\xff\xff\x56\x00\x57\x00\x58\x00\xff\xff\x5a\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\xff\xff\x10\x00\x13\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x0a\x00\x1a\x00\x19\x00\x1a\x00\xff\xff\xff\xff\x10\x00\xff\xff\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\xff\xff\x18\x00\x19\x00\x1a\x00\xff\xff\x29\x00\x2a\x00\x2b\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x1a\x00\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\xff\xff\x18\x00\x19\x00\x1a\x00\x10\x00\x1c\x00\x1d\x00\x1e\x00\x14\x00\xff\xff\x16\x00\xff\xff\x18\x00\x19\x00\x1a\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xff\xff\xff\xff\x56\x00\x57\x00\x58\x00\xff\xff\x5a\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\x13\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x10\x00\x1a\x00\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\x18\x00\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\x18\x00\x19\x00\x1a\x00\x29\x00\x2a\x00\x2b\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xff\xff\xff\xff\x56\x00\x57\x00\x58\x00\xff\xff\x5a\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x13\x00\x08\x00\x09\x00\xff\xff\x17\x00\xff\xff\xff\xff\x1a\x00\xff\xff\x10\x00\xff\xff\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x1a\x00\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\x18\x00\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x1a\x00\x55\x00\x56\x00\x57\x00\x58\x00\x10\x00\x5a\x00\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\x17\x00\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xff\xff\xff\xff\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\xff\xff\xff\xff\xff\xff\x10\x00\x13\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x10\x00\x1a\x00\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\x18\x00\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x29\x00\x2a\x00\x2b\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xff\xff\xff\xff\x56\x00\x57\x00\x58\x00\xff\xff\x5a\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\x04\x00\x05\x00\x06\x00\x13\x00\x08\x00\x09\x00\x16\x00\xff\xff\xff\xff\xff\xff\x1a\x00\xff\xff\x10\x00\xff\xff\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xff\xff\xff\xff\x56\x00\x57\x00\x58\x00\xff\xff\x5a\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\xff\xff\x04\x00\x05\x00\x06\x00\x13\x00\x08\x00\x09\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1a\x00\xff\xff\x10\x00\xff\xff\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x1a\x00\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x48\x00\xff\xff\x4a\x00\x4b\x00\x4c\x00\x13\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x1a\x00\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x1a\x00\xff\xff\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x10\x00\xff\xff\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x1a\x00\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x1a\x00\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xff\xff\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xff\xff\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x0a\x00\xff\xff\xff\xff\xff\xff\x48\x00\x0f\x00\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\xff\xff\x51\x00\x52\x00\x53\x00\x1a\x00\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x0a\x00\xff\xff\xff\xff\xff\xff\x48\x00\x0f\x00\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\x50\x00\xff\xff\x52\x00\x53\x00\x1a\x00\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\xff\xff\x52\x00\x53\x00\xff\xff\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x0a\x00\xff\xff\xff\xff\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\x13\x00\x4e\x00\x4f\x00\xff\xff\x51\x00\x52\x00\x53\x00\x1a\x00\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x48\x00\xff\xff\x4a\x00\xff\xff\x4c\x00\xff\xff\x4e\x00\x4f\x00\x50\x00\xff\xff\x52\x00\x53\x00\xff\xff\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x48\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4e\x00\xff\xff\xff\xff\x51\x00\x52\x00\x53\x00\xff\xff\x10\x00\x56\x00\x57\x00\x58\x00\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x48\x00\xff\xff\x05\x00\x06\x00\xff\xff\x08\x00\x09\x00\xff\xff\xff\xff\x51\x00\x52\x00\x53\x00\xff\xff\x10\x00\x56\x00\x57\x00\xff\xff\x14\x00\x5a\x00\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x48\x00\x05\x00\x06\x00\xff\xff\x08\x00\x09\x00\xff\xff\xff\xff\xff\xff\x51\x00\xff\xff\x53\x00\x10\x00\xff\xff\x56\x00\x57\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x48\x00\xff\xff\x10\x00\xff\xff\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\x51\x00\xff\xff\x19\x00\x1a\x00\x10\x00\x56\x00\x57\x00\xff\xff\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\x10\x00\xff\xff\xff\xff\xff\xff\x14\x00\x48\x00\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\x51\x00\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\x57\x00\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\x48\x00\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x42\x00\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\x48\x00\x38\x00\x39\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x10\x00\x42\x00\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\x48\x00\x10\x00\x19\x00\x1a\x00\xff\xff\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\x10\x00\xff\xff\x19\x00\x1a\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\xc2\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\xef\x00\x10\x00\x11\x00\xbe\x00\x8c\x00\x60\x00\x23\x01\xd4\x00\x78\x00\xd5\x00\x60\x00\x56\x01\x61\x00\x62\x00\x79\x00\x12\x00\x13\x00\x61\x00\x62\x00\x14\x00\x55\x01\x15\x00\x21\x01\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x77\x00\x05\x00\x78\x00\x41\x00\x42\x00\x06\x00\x43\x00\x07\x00\x79\x00\xea\x00\x44\x00\x0a\x00\x16\x00\x05\x00\x8d\x00\xcf\x00\xd0\x00\x06\x00\xc3\x00\x07\x00\x17\x00\x18\x00\x51\x00\x0a\x00\xbf\x00\x19\x00\x45\x00\x4f\x01\x47\x00\x2b\x00\x1a\x00\x1b\x00\x04\x00\x1c\x00\x5d\x01\x1d\x00\x0c\x01\x44\x01\x41\x01\x1e\x00\x8a\x00\x1f\x00\x15\x01\x20\x00\x21\x00\x22\x00\x25\x00\x2f\x00\xd7\x00\x31\x00\x23\x00\x24\x00\x19\x01\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x8a\x00\x10\x00\x11\x00\x05\x00\x3e\x01\x3f\x01\x40\x01\x06\x00\x41\x01\x07\x00\xb4\x00\x26\x00\x09\x00\x0a\x00\xb6\x00\x12\x00\x13\x00\xc5\x00\xd5\x00\x14\x00\xd6\x00\x15\x00\xd9\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xda\x00\x05\x00\xdd\x00\x41\x00\x42\x00\x06\x00\x43\x00\x07\x00\xe2\x00\x05\x00\x44\x00\x0a\x00\x16\x00\x06\x00\x43\x00\x07\x00\x04\x00\x62\x01\x44\x00\x0a\x00\x17\x00\x18\x00\x2f\x00\xd8\x00\x31\x00\x19\x00\x45\x00\x54\x01\x47\x00\x61\x01\x1a\x00\x1b\x00\x8d\x00\x1c\x00\xfc\x00\x1d\x00\x3e\x01\x42\x01\x40\x01\x1e\x00\x41\x01\x1f\x00\x5d\x01\x20\x00\x21\x00\x22\x00\x2f\x00\xdb\x00\x31\x00\x59\x01\x23\x00\x24\x00\x33\x00\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x05\x00\x10\x00\x11\x00\x48\x01\x06\x00\x49\x01\x07\x00\x4a\x01\x05\x00\x56\x00\x0a\x00\x4b\x01\x06\x00\x5b\x00\x07\x00\x12\x00\x13\x00\x5c\x00\x0a\x00\x14\x00\x57\x00\x15\x00\x51\x01\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x4e\x01\x05\x00\x8d\x00\x41\x00\x42\x00\x06\x00\x43\x00\x07\x00\x29\x01\x05\x00\x44\x00\x0a\x00\x16\x00\x06\x00\xc1\x00\x07\x00\x2d\x01\x4c\x00\x09\x00\x0a\x00\x17\x00\x18\x00\x32\x01\x36\x01\x04\x01\x19\x00\x45\x00\x22\x01\x47\x00\x37\x01\x1a\x00\x1b\x00\x33\x00\x1c\x00\x3a\x01\x1d\x00\x2f\x00\x30\x00\x31\x00\x1e\x00\x3c\x01\x1f\x00\x3b\x01\x20\x00\x21\x00\x22\x00\x2f\x00\x33\x00\x31\x00\xea\x00\x23\x00\x24\x00\x46\x01\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\xfb\xff\x10\x00\x11\x00\x05\x00\x65\x00\x66\x00\x67\x00\x06\x00\x43\x00\x07\x00\x33\x00\xf1\x00\x44\x00\x0a\x00\x05\x00\x12\x00\x13\x00\xf2\x00\x06\x00\x28\x00\x07\x00\x15\x00\x5e\x01\x09\x00\x0a\x00\xf3\x00\x29\x00\x2a\x00\x45\x00\x32\x01\x47\x00\x05\x00\xf4\x00\xf7\x00\x2b\x00\x06\x00\x43\x00\x07\x00\xf5\x00\x05\x00\x44\x00\x0a\x00\x16\x00\x06\x00\xfe\x00\x07\x00\x04\x01\x2d\x00\x09\x00\x0a\x00\x17\x00\x18\x00\x14\x01\x15\x01\xc7\x00\x19\x00\x45\x00\x37\x01\x47\x00\x17\x01\x1a\x00\x1b\x00\xb6\x00\x1c\x00\xc7\x00\x1d\x00\xba\x00\x0f\x00\xbb\x00\x1e\x00\xc1\x00\x1f\x00\x64\x00\x20\x00\x21\x00\x22\x00\xc5\x00\xc7\x00\xc9\x00\xca\x00\x23\x00\x24\x00\xd1\x00\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x05\x00\x10\x00\x11\x00\xc8\x00\x06\x00\xcb\x00\x07\x00\xcc\x00\x2d\x00\x09\x00\x0a\x00\xcd\x00\xce\x00\x0a\x01\x33\x00\x12\x00\x13\x00\xdf\x00\xe0\x00\x14\x00\xe1\x00\x15\x00\xff\xff\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x33\x00\x70\x00\x71\x00\xff\xff\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x05\x00\x78\x00\x60\x00\x16\x00\x06\x00\xff\xff\x07\x00\x79\x00\x46\x01\x09\x00\x0a\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x00\x00\x1c\x00\x0b\x01\x1d\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x24\x00\x00\x00\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x05\x00\x10\x00\x11\x00\x00\x00\x06\x00\x43\x00\x07\x00\x00\x00\x05\x00\x44\x00\x0a\x00\x00\x00\x06\x00\x20\x01\x07\x00\x12\x00\x13\x00\x57\x01\x0a\x00\x14\x00\x00\x00\x15\x00\x00\x00\x00\x00\x05\x00\x45\x00\xf8\x00\x47\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x51\x00\x0a\x00\x06\x00\x43\x00\x07\x00\x00\x00\x05\x00\x44\x00\x0a\x00\x16\x00\x06\x00\x00\x00\x07\x00\x9f\x00\x53\x00\x59\x01\x0a\x00\x17\x00\x18\x00\x00\x00\x8d\x00\x00\x00\x19\x00\x45\x00\x02\x01\x47\x00\x00\x00\x1a\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x24\x00\x00\x00\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x05\x00\x10\x00\x11\x00\x00\x00\x06\x00\x43\x00\x07\x00\x00\x00\x05\x00\x44\x00\x0a\x00\x00\x00\x06\x00\x43\x00\x07\x00\x12\x00\x13\x00\x44\x00\x0a\x00\x14\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x45\x00\x05\x01\x47\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x45\x00\x06\x01\x47\x00\x5f\x01\x88\x00\x35\x00\x00\x00\x36\x00\x37\x00\x16\x00\xd3\x00\xe4\x00\xe5\x00\xe6\x00\xe7\x00\x05\x00\xe8\x00\x17\x00\x18\x00\x06\x00\x00\x00\x07\x00\x19\x00\x00\x00\x5d\x00\x0a\x00\x00\x00\x1a\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x24\x00\x00\x00\x25\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x05\x00\x10\x00\x11\x00\x00\x00\x06\x00\x43\x00\x07\x00\x00\x00\x05\x00\x44\x00\x0a\x00\x00\x00\x06\x00\x00\x00\x07\x00\x12\x00\x13\x00\x51\x00\x0a\x00\x14\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x45\x00\x0b\x01\x47\x00\x00\x00\x00\x00\x52\x00\x53\x00\x00\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x00\x00\x10\x00\x11\x00\x00\x00\x00\x00\x16\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x00\x18\x00\x12\x00\x13\x00\x00\x00\x19\x00\x14\x00\x00\x00\x15\x00\x00\x00\x1a\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x21\x00\x22\x00\xec\x00\xed\x00\x37\x00\x16\x00\x23\x00\x24\x00\x00\x00\x25\x00\x00\x00\x05\x00\x00\x00\x17\x00\x18\x00\x06\x00\x00\x00\x07\x00\x19\x00\x00\x00\x5d\x00\x0a\x00\x00\x00\x1a\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x21\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x24\x00\xb9\x00\x25\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x68\x00\x00\x00\x00\x00\x06\x00\x43\x00\x07\x00\x05\x00\x69\x00\x44\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x54\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x2b\x01\x09\x00\x0a\x00\x45\x00\x20\x01\x47\x00\x00\x00\x55\x00\x53\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x00\x00\x82\x00\x83\x00\x84\x00\xdd\x00\x85\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x68\x00\x00\x00\x00\x00\x06\x00\x43\x00\x07\x00\x05\x00\x69\x00\x44\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x85\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x3d\x01\x09\x00\x0a\x00\x45\x00\x46\x00\x47\x00\x00\x00\x86\x00\x53\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\xbc\x00\x64\x00\x65\x00\x66\x00\x67\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x68\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x00\x00\x4c\x00\x98\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x00\x99\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\xbd\x00\x64\x00\x65\x00\x66\x00\x67\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x68\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x00\x00\x4c\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x00\x00\x82\x00\x83\x00\x84\x00\x00\x00\x85\x00\x1d\x01\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x01\x68\x00\x6a\x00\x00\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x69\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x05\x00\x78\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x79\x00\x9b\x00\x09\x00\x0a\x00\x00\x00\x2d\x01\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x8d\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x00\x00\x82\x00\x83\x00\x84\x00\x00\x00\x85\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x01\x68\x00\x5b\x01\x88\x00\x35\x00\x00\x00\x36\x00\x37\x00\x69\x00\x00\x00\x74\x00\x75\x00\x76\x00\x77\x00\x05\x00\x78\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x79\x00\x00\x00\x5d\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x00\x00\x35\x01\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x00\x00\x82\x00\x83\x00\x84\x00\x00\x00\x85\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x68\x00\x00\x00\x00\x00\x06\x00\x43\x00\x07\x00\xe1\x00\x69\x00\x44\x00\x0a\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x2d\x00\x09\x00\x0a\x00\x00\x00\x45\x00\x48\x00\x47\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x18\x01\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x19\x01\x00\x00\x82\x00\x83\x00\x84\x00\x00\x00\x85\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\xc3\xff\xc3\xff\x00\x00\x05\x00\x68\x00\x00\x00\x00\x00\x06\x00\x43\x00\x07\x00\x2c\x00\x69\x00\x44\x00\x0a\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x2d\x00\x09\x00\x0a\x00\x00\x00\x45\x00\x49\x00\x47\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x64\x00\x65\x00\x66\x00\x67\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x68\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x00\x00\x9b\x00\x09\x00\x0a\x00\x05\x00\x9c\x00\x9d\x00\x9e\x00\x06\x00\x00\x00\x07\x00\x00\x00\x43\x01\x09\x00\x0a\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x4d\x01\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x00\x00\x82\x00\x83\x00\x84\x00\x00\x00\x85\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x68\x00\x00\x00\x00\x00\x06\x00\x43\x00\x07\x00\x05\x00\x69\x00\x44\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\xe8\x00\x09\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\xef\x00\x09\x00\x0a\x00\x45\x00\x4a\x00\x47\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x54\x01\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x00\x00\x82\x00\x83\x00\x84\x00\x00\x00\x85\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x00\x00\x3c\x01\x88\x00\x35\x00\x68\x00\x36\x00\x37\x00\x00\x00\x31\x01\x00\x00\x00\x00\x69\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x5d\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\xf6\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x64\x00\x65\x00\x66\x00\x67\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x68\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x01\x01\x09\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x0e\x01\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x64\x00\x65\x00\x66\x00\x67\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x68\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x1b\x01\x82\x00\x83\x00\x84\x00\x05\x00\x85\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x1d\x01\x00\x00\x1e\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x00\x00\x82\x00\x83\x00\x84\x00\x1c\x01\x85\x00\x64\x00\x65\x00\x66\x00\x67\x00\x8e\x00\x00\x00\x00\x00\x00\x00\x05\x00\x68\x00\x00\x00\x00\x00\x06\x00\x43\x00\x07\x00\x05\x00\x69\x00\x44\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x08\x00\x09\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x5a\x01\x0a\x00\x45\x00\x4b\x00\x47\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x00\x00\x82\x00\x83\x00\x84\x00\x00\x00\x85\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x00\x00\xf5\x00\x88\x00\x35\x00\x68\x00\x36\x00\x37\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x69\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x5d\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x4b\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x00\x00\x82\x00\x83\x00\x84\x00\x00\x00\x85\x00\x64\x00\x65\x00\x66\x00\x67\x00\xc0\x00\x00\x00\x87\x00\x88\x00\x35\x00\x68\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x5d\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x4e\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x64\x00\x65\x00\x66\x00\x67\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x68\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\x51\x01\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x52\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x64\x00\x65\x00\x66\x00\x67\x00\x79\x00\x00\x00\x7a\x00\xc4\x00\x7b\x00\x68\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\x25\x01\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x26\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x64\x00\x65\x00\x66\x00\x67\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x68\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x00\x00\x82\x00\x83\x00\x84\x00\xd1\x00\x85\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x27\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x64\x00\x65\x00\x66\x00\x67\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\x29\x01\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x2a\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x64\x00\x65\x00\x66\x00\x67\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x68\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\x2e\x01\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x2f\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\x38\x01\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\xeb\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\xf9\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\xfa\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x64\x00\x00\x00\x00\x00\x00\x00\x79\x00\x12\x01\x00\x00\x00\x00\x00\x00\x68\x00\x7c\x00\x7d\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\xfb\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\xfe\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x64\x00\x00\x00\x00\x00\x00\x00\x79\x00\x13\x01\x7a\x00\x00\x00\x7b\x00\x68\x00\x7c\x00\x7d\x00\x7e\x00\x00\x00\x80\x00\x81\x00\x69\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\xff\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x00\x00\x80\x00\x81\x00\x00\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\x07\x01\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x08\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x64\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x68\x00\x7c\x00\x7d\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x69\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\x0d\x01\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x0f\x01\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x7c\x00\x7d\x00\x7e\x00\x00\x00\x80\x00\x81\x00\x00\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\x10\x01\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x8e\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x05\x00\x82\x00\x83\x00\x84\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\x8f\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x90\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x79\x00\x00\x00\x34\x00\x35\x00\x00\x00\x36\x00\x37\x00\x00\x00\x00\x00\x7f\x00\x80\x00\x81\x00\x00\x00\x05\x00\x82\x00\x83\x00\x00\x00\x06\x00\x85\x00\x07\x00\x05\x00\x00\x00\x38\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x91\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x79\x00\x34\x00\x35\x00\x00\x00\x36\x00\x37\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x81\x00\x05\x00\x00\x00\x82\x00\x83\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x5d\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x92\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x64\x00\x65\x00\x66\x00\x67\x00\x00\x00\x79\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x7f\x00\x00\x00\x93\x00\x0a\x00\x05\x00\x82\x00\x83\x00\x00\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x94\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x95\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x79\x00\x07\x00\x00\x00\x00\x00\x96\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x83\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x00\x00\x78\x00\x00\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x79\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x05\x00\x78\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x79\x00\x05\x00\x97\x00\x0a\x00\x00\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x9a\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xa0\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xa1\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xa2\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xa3\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xa4\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xa5\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xa6\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xa7\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xa8\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xa9\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xaa\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xab\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xac\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xad\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xae\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xaf\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xb0\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xb1\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xb2\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\xb3\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x42\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x50\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x58\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x59\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x05\x00\x00\x00\x5a\x00\x0a\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\x5e\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (3, 162) [
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
	(162 , happyReduce_162)
	]

happy_n_terms = 93 :: Int
happy_n_nonterms = 44 :: Int

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
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
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
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (annotate2 happy_var_1 happy_var_3 (RunAssertion (annotate2 happy_var_1 happy_var_3 (ASNot happy_var_3)))
	)}}

happyReduce_6 = happySpecReduce_2  1# happyReduction_6
happyReduction_6 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (annotate2 happy_var_1 happy_var_2 (RunAssertion happy_var_2)
	)}}

happyReduce_7 = happyMonadReduce 2# 1# happyReduction_7
happyReduction_7 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
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
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	( do
                                                    d <- annotateWithSymbolTable $
                                                        annotate2List happy_var_1 happy_var_2 (Transparent (map unLoc happy_var_2))
                                                    return $ annotate2List happy_var_1 happy_var_2 (Bind [d]))}}
	) (\r -> happyReturn (happyIn7 r))

happyReduce_9 = happySpecReduce_1  1# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
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
		 (combineDecls (reverse happy_var_1)
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
	 = happyThen (case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	( convDecl happy_var_1 happy_var_3)}}
	) (\r -> happyReturn (happyIn15 r))

happyReduce_23 = happySpecReduce_2  9# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
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
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
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
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut20 happy_x_4 of { happy_var_4 -> 
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
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut20 happy_x_4 of { happy_var_4 -> 
	happyIn15
		 (annotate2List happy_var_1 happy_var_4 (SubType (unLoc happy_var_2) happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_27 = happySpecReduce_2  9# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (annotate2List happy_var_1 happy_var_2 (External (map unLoc happy_var_2))
	)}}

happyReduce_28 = happySpecReduce_2  9# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (annotate2List happy_var_1 happy_var_2 (Transparent (map unLoc happy_var_2))
	)}}

happyReduce_29 = happySpecReduce_3  9# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (annotate2 happy_var_1 happy_var_3 (Assert (annotate2 happy_var_1 happy_var_3 (ASNot happy_var_3)))
	)}}

happyReduce_30 = happySpecReduce_2  9# happyReduction_30
happyReduction_30 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
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
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
	happyIn15
		 (annotate2 happy_var_1 happy_var_4 (NameType (unLoc happy_var_2) happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_32 = happyReduce 8# 9# happyReduction_32
happyReduction_32 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut10 happy_x_4 of { happy_var_4 -> 
	case happyOut10 happy_x_7 of { happy_var_7 -> 
	case happyOutTok happy_x_8 of { happy_var_8 -> 
	happyIn15
		 (annotate2 happy_var_1 happy_var_8 (Module (unLoc happy_var_2) [] 
                                                    (map checkModuleDecl happy_var_4)
                                                    (map checkModuleDecl happy_var_7))
	) `HappyStk` happyRest}}}}}

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
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	case happyOut10 happy_x_6 of { happy_var_6 -> 
	case happyOutTok happy_x_7 of { happy_var_7 -> 
	happyIn15
		 (annotate2 happy_var_1 happy_var_7 (TimedSection Nothing
                                                    (Just happy_var_3)
                                                    (map checkTimedDecl happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_35 = happySpecReduce_3  10# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (annotate2 happy_var_1 happy_var_3 (Refinement happy_var_1 (getRefinesModel happy_var_2) happy_var_3 [])
	)}}}

happyReduce_36 = happyReduce 4# 10# happyReduction_36
happyReduction_36 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	case happyOut18 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (annotate2 happy_var_1 happy_var_4 (Refinement happy_var_1 (getRefinesModel happy_var_2) happy_var_3 [unLoc happy_var_4])
	) `HappyStk` happyRest}}}}

happyReduce_37 = happySpecReduce_2  10# happyReduction_37
happyReduction_37 happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (annotate2 happy_var_1 happy_var_2 (PropertyCheck happy_var_1 (unLoc happy_var_2) Nothing)
	)}}

happyReduce_38 = happySpecReduce_3  10# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (annotate2 happy_var_1 happy_var_3 (PropertyCheck happy_var_1 (unLoc happy_var_2) (Just (getPropModel happy_var_3)))
	)}}}

happyReduce_39 = happySpecReduce_1  11# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (liftLoc happy_var_1 DeadlockFreedom
	)}

happyReduce_40 = happySpecReduce_1  11# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (liftLoc happy_var_1 LivelockFreedom
	)}

happyReduce_41 = happySpecReduce_1  11# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (liftLoc happy_var_1 LivelockFreedom
	)}

happyReduce_42 = happySpecReduce_1  11# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (liftLoc happy_var_1 Deterministic
	)}

happyReduce_43 = happySpecReduce_2  12# happyReduction_43
happyReduction_43 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (annotate2 happy_var_1 happy_var_2 (TauPriority happy_var_2)
	)}}

happyReduce_44 = happySpecReduce_3  13# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (annotate2 happy_var_1 happy_var_3 (DataTypeClause (unLoc happy_var_1) (Just happy_var_3))
	)}}

happyReduce_45 = happySpecReduce_1  13# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (annotate happy_var_1 (DataTypeClause (unLoc happy_var_1) Nothing)
	)}

happyReduce_46 = happySpecReduce_1  14# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (reverse happy_var_1
	)}

happyReduce_47 = happySpecReduce_1  15# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 ([happy_var_1]
	)}

happyReduce_48 = happySpecReduce_3  15# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (happy_var_3:happy_var_1
	)}}

happyReduce_49 = happySpecReduce_1  16# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (liftLoc happy_var_1 (UnQual (OccName (getName happy_var_1)))
	)}

happyReduce_50 = happySpecReduce_3  16# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (annotate happy_var_1 (Qual (OccName (getName happy_var_1)) (unLoc happy_var_3))
	)}}

happyReduce_51 = happySpecReduce_1  17# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (liftLoc happy_var_1 (UnQual (OccName (getName happy_var_1)))
	)}

happyReduce_52 = happySpecReduce_1  18# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (reverse happy_var_1
	)}

happyReduce_53 = happySpecReduce_1  19# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 ([happy_var_1]
	)}

happyReduce_54 = happySpecReduce_3  19# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (happy_var_3:happy_var_1
	)}}

happyReduce_55 = happySpecReduce_1  20# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (liftLoc happy_var_1 (Int (getInt happy_var_1))
	)}

happyReduce_56 = happySpecReduce_1  20# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (liftLoc happy_var_1 (Char (getChar happy_var_1))
	)}

happyReduce_57 = happySpecReduce_1  20# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (liftLoc happy_var_1 (String (getString happy_var_1))
	)}

happyReduce_58 = happySpecReduce_1  20# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (liftLoc happy_var_1 (Bool True)
	)}

happyReduce_59 = happySpecReduce_1  20# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (liftLoc happy_var_1 (Bool False)
	)}

happyReduce_60 = happySpecReduce_1  21# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (convPat happy_var_1
	)}

happyReduce_61 = happyMonadReduce 1# 22# happyReduction_61
happyReduction_61 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	( do
                                    modifyTopFileParserState (
                                        \ st @ (FileParserState { sequenceStack = (c:cs) }) -> 
                                            st { sequenceStack = (c+1):cs })
                                    return happy_var_1)}
	) (\r -> happyReturn (happyIn28 r))

happyReduce_62 = happySpecReduce_1  23# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (happy_var_1
	)}

happyReduce_63 = happySpecReduce_1  24# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (checkExp happy_var_1
	)}

happyReduce_64 = happyMonadReduce 1# 25# happyReduction_64
happyReduction_64 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut32 happy_x_1 of { happy_var_1 -> 
	( do
                                        t <- freshPType
                                        let An l _ e = happy_var_1
                                        return $ An l (Nothing, t) e)}
	) (\r -> happyReturn (happyIn31 r))

happyReduce_65 = happySpecReduce_1  26# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (liftLoc happy_var_1 (Lit (unLoc happy_var_1))
	)}

happyReduce_66 = happySpecReduce_1  26# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (liftLoc happy_var_1 (Var (unLoc happy_var_1))
	)}

happyReduce_67 = happySpecReduce_3  26# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (Tuple happy_var_2)
	)}}}

happyReduce_68 = happySpecReduce_3  26# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (DotApp happy_var_1 happy_var_3)
	)}}

happyReduce_69 = happyReduce 4# 26# happyReduction_69
happyReduction_69 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_4 (Let (checkLetDecls happy_var_2) happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_70 = happyReduce 6# 26# happyReduction_70
happyReduction_70 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	case happyOut31 happy_x_6 of { happy_var_6 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_6 (If happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_71 = happySpecReduce_3  26# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (Paren happy_var_2)
	)}}}

happyReduce_72 = happyReduce 4# 26# happyReduction_72
happyReduction_72 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_4 (App happy_var_1 happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_73 = happyReduce 4# 26# happyReduction_73
happyReduction_73 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_4 (Lambda happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_74 = happySpecReduce_3  26# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp Equals happy_var_1 happy_var_3)
	)}}

happyReduce_75 = happySpecReduce_3  26# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp NotEquals happy_var_1 happy_var_3)
	)}}

happyReduce_76 = happySpecReduce_3  26# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp LessThan happy_var_1 happy_var_3)
	)}}

happyReduce_77 = happySpecReduce_3  26# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp GreaterThan happy_var_1 happy_var_3)
	)}}

happyReduce_78 = happySpecReduce_3  26# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp LessThanEq happy_var_1 happy_var_3)
	)}}

happyReduce_79 = happySpecReduce_3  26# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp GreaterThanEq happy_var_1 happy_var_3)
	)}}

happyReduce_80 = happySpecReduce_2  26# happyReduction_80
happyReduction_80 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_2 (BooleanUnaryOp Not happy_var_2)
	)}}

happyReduce_81 = happySpecReduce_3  26# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp Or happy_var_1 happy_var_3)
	)}}

happyReduce_82 = happySpecReduce_3  26# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (BooleanBinaryOp And happy_var_1 happy_var_3)
	)}}

happyReduce_83 = happySpecReduce_2  26# happyReduction_83
happyReduction_83 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_2 (MathsUnaryOp Negate happy_var_2)
	)}}

happyReduce_84 = happySpecReduce_3  26# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (MathsBinaryOp Plus happy_var_1 happy_var_3)
	)}}

happyReduce_85 = happySpecReduce_3  26# happyReduction_85
happyReduction_85 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (MathsBinaryOp Minus happy_var_1 happy_var_3)
	)}}

happyReduce_86 = happySpecReduce_3  26# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (MathsBinaryOp Mod happy_var_1 happy_var_3)
	)}}

happyReduce_87 = happySpecReduce_3  26# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (MathsBinaryOp Divide happy_var_1 happy_var_3)
	)}}

happyReduce_88 = happySpecReduce_3  26# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (MathsBinaryOp Times happy_var_1 happy_var_3)
	)}}

happyReduce_89 = happySpecReduce_1  26# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (annotate happy_var_1 (List [])
	)}

happyReduce_90 = happySpecReduce_3  26# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	case happyOut29 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (List happy_var_2)
	)}}}

happyReduce_91 = happyReduce 5# 26# happyReduction_91
happyReduction_91 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_4 of { happy_var_4 -> 
	case happyOut29 happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_5 (ListComp happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_92 = happyReduce 4# 26# happyReduction_92
happyReduction_92 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut29 happy_x_4 of { happy_var_4 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_4 (ListEnumFrom happy_var_2)
	) `HappyStk` happyRest}}}

happyReduce_93 = happyReduce 5# 26# happyReduction_93
happyReduction_93 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	case happyOut29 happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_5 (ListEnumFromTo happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_94 = happyReduce 6# 26# happyReduction_94
happyReduction_94 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_5 of { happy_var_5 -> 
	case happyOut29 happy_x_6 of { happy_var_6 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_6 (ListEnumFromComp happy_var_2 happy_var_5)
	) `HappyStk` happyRest}}}}

happyReduce_95 = happyReduce 7# 26# happyReduction_95
happyReduction_95 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	case happyOut48 happy_x_6 of { happy_var_6 -> 
	case happyOut29 happy_x_7 of { happy_var_7 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_7 (ListEnumFromToComp happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}}

happyReduce_96 = happySpecReduce_3  26# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (Concat happy_var_1 happy_var_3)
	)}}

happyReduce_97 = happySpecReduce_2  26# happyReduction_97
happyReduction_97 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_2 (ListLength happy_var_2)
	)}}

happyReduce_98 = happySpecReduce_3  26# happyReduction_98
happyReduction_98 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (Set happy_var_2)
	)}}}

happyReduce_99 = happyReduce 5# 26# happyReduction_99
happyReduction_99 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_5 (SetComp happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_100 = happyReduce 4# 26# happyReduction_100
happyReduction_100 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_4 (SetEnumFrom happy_var_2)
	) `HappyStk` happyRest}}}

happyReduce_101 = happyReduce 5# 26# happyReduction_101
happyReduction_101 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_5 (SetEnumFromTo happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_102 = happyReduce 6# 26# happyReduction_102
happyReduction_102 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_5 of { happy_var_5 -> 
	case happyOutTok happy_x_6 of { happy_var_6 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_6 (SetEnumFromComp happy_var_2 happy_var_5)
	) `HappyStk` happyRest}}}}

happyReduce_103 = happyReduce 7# 26# happyReduction_103
happyReduction_103 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	case happyOut48 happy_x_6 of { happy_var_6 -> 
	case happyOutTok happy_x_7 of { happy_var_7 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_7 (SetEnumFromToComp happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}}

happyReduce_104 = happySpecReduce_3  26# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (SetEnum happy_var_2)
	)}}}

happyReduce_105 = happyReduce 5# 26# happyReduction_105
happyReduction_105 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_5 (SetEnumComp happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_106 = happySpecReduce_1  26# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (liftLoc happy_var_1 (ExpPatWildCard)
	)}

happyReduce_107 = happySpecReduce_3  26# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (liftLoc happy_var_1 (ExpPatDoublePattern happy_var_1 happy_var_3)
	)}}

happyReduce_108 = happyReduce 4# 26# happyReduction_108
happyReduction_108 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_4 (Prefix happy_var_1 happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_109 = happySpecReduce_3  26# happyReduction_109
happyReduction_109 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (Prefix happy_var_1 [] happy_var_3)
	)}}

happyReduce_110 = happySpecReduce_3  26# happyReduction_110
happyReduction_110 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (ExternalChoice happy_var_1 happy_var_3)
	)}}

happyReduce_111 = happySpecReduce_3  26# happyReduction_111
happyReduction_111 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (Hiding happy_var_1 happy_var_3)
	)}}

happyReduce_112 = happySpecReduce_3  26# happyReduction_112
happyReduction_112 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (InternalChoice happy_var_1 happy_var_3)
	)}}

happyReduce_113 = happySpecReduce_3  26# happyReduction_113
happyReduction_113 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (Interleave happy_var_1 happy_var_3)
	)}}

happyReduce_114 = happyReduce 5# 26# happyReduction_114
happyReduction_114 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_5 (GenParallel happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_115 = happyReduce 7# 26# happyReduction_115
happyReduction_115 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	case happyOut31 happy_x_7 of { happy_var_7 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_7 (AlphaParallel happy_var_1 happy_var_3 happy_var_5 happy_var_7)
	) `HappyStk` happyRest}}}}

happyReduce_116 = happySpecReduce_3  26# happyReduction_116
happyReduction_116 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (Interrupt happy_var_1 happy_var_3)
	)}}

happyReduce_117 = happyReduce 5# 26# happyReduction_117
happyReduction_117 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_5 (Exception happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_118 = happySpecReduce_3  26# happyReduction_118
happyReduction_118 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (SlidingChoice happy_var_1 happy_var_3)
	)}}

happyReduce_119 = happySpecReduce_3  26# happyReduction_119
happyReduction_119 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (SequentialComp happy_var_1 happy_var_3)
	)}}

happyReduce_120 = happySpecReduce_3  26# happyReduction_120
happyReduction_120 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_3 (GuardedExp happy_var_1 happy_var_3)
	)}}

happyReduce_121 = happyReduce 5# 26# happyReduction_121
happyReduction_121 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_5 (SynchronisingExternalChoice happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_122 = happyReduce 5# 26# happyReduction_122
happyReduction_122 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_5 (SynchronisingInterrupt happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_123 = happyReduce 5# 26# happyReduction_123
happyReduction_123 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	case happyOut46 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_5 (Rename happy_var_1 happy_var_3 happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_124 = happyReduce 6# 26# happyReduction_124
happyReduction_124 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	case happyOut46 happy_x_4 of { happy_var_4 -> 
	case happyOut31 happy_x_6 of { happy_var_6 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_6 (LinkParallel happy_var_1 happy_var_3 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_125 = happyReduce 4# 26# happyReduction_125
happyReduction_125 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_4 (ReplicatedInterleave happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_126 = happyReduce 4# 26# happyReduction_126
happyReduction_126 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_4 (ReplicatedExternalChoice happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_127 = happyReduce 4# 26# happyReduction_127
happyReduction_127 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_4 (ReplicatedInternalChoice happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_128 = happyReduce 7# 26# happyReduction_128
happyReduction_128 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	case happyOut31 happy_x_7 of { happy_var_7 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_7 (ReplicatedAlphaParallel happy_var_2 happy_var_5 happy_var_7)
	) `HappyStk` happyRest}}}}

happyReduce_129 = happyReduce 6# 26# happyReduction_129
happyReduction_129 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_4 of { happy_var_4 -> 
	case happyOut31 happy_x_6 of { happy_var_6 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_6 (ReplicatedParallel happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_130 = happyReduce 7# 26# happyReduction_130
happyReduction_130 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_2 of { happy_var_2 -> 
	case happyOut46 happy_x_3 of { happy_var_3 -> 
	case happyOut48 happy_x_5 of { happy_var_5 -> 
	case happyOut31 happy_x_7 of { happy_var_7 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_7 (ReplicatedLinkParallel happy_var_2 happy_var_3 happy_var_5 happy_var_7)
	) `HappyStk` happyRest}}}}}

happyReduce_131 = happyReduce 4# 26# happyReduction_131
happyReduction_131 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_4 (ReplicatedSequentialComp happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_132 = happyReduce 6# 26# happyReduction_132
happyReduction_132 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_4 of { happy_var_4 -> 
	case happyOut31 happy_x_6 of { happy_var_6 -> 
	happyIn32
		 (annotate2 happy_var_1 happy_var_6 (ReplicatedSynchronisingExternalChoice
                                        happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_133 = happySpecReduce_2  27# happyReduction_133
happyReduction_133 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (annotate2 happy_var_1 happy_var_2 (Input (convPat happy_var_2) Nothing)
	)}}

happyReduce_134 = happyReduce 4# 27# happyReduction_134
happyReduction_134 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (annotate2 happy_var_1 happy_var_4 (Input (convPat happy_var_2) (Just (checkExp happy_var_4)))
	) `HappyStk` happyRest}}}

happyReduce_135 = happySpecReduce_2  27# happyReduction_135
happyReduction_135 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (annotate2 happy_var_1 happy_var_2 (NonDetInput (convPat happy_var_2) Nothing)
	)}}

happyReduce_136 = happyReduce 4# 27# happyReduction_136
happyReduction_136 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (annotate2 happy_var_1 happy_var_4 (NonDetInput (convPat happy_var_2) (Just (checkExp happy_var_4)))
	) `HappyStk` happyRest}}}

happyReduce_137 = happySpecReduce_2  27# happyReduction_137
happyReduction_137 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (annotate2 happy_var_1 happy_var_2 (Output (checkExp happy_var_2))
	)}}

happyReduce_138 = happySpecReduce_3  28# happyReduction_138
happyReduction_138 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 ((happy_var_1, happy_var_3)
	)}}

happyReduce_139 = happySpecReduce_1  29# happyReduction_139
happyReduction_139 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (reverse happy_var_1
	)}

happyReduce_140 = happySpecReduce_1  30# happyReduction_140
happyReduction_140 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ([happy_var_1]
	)}

happyReduce_141 = happySpecReduce_3  30# happyReduction_141
happyReduction_141 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (happy_var_3:happy_var_1
	)}}

happyReduce_142 = happySpecReduce_3  31# happyReduction_142
happyReduction_142 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 ((happy_var_1, happy_var_3)
	)}}

happyReduce_143 = happySpecReduce_1  32# happyReduction_143
happyReduction_143 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (reverse happy_var_1
	)}

happyReduce_144 = happySpecReduce_1  33# happyReduction_144
happyReduction_144 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 ([happy_var_1]
	)}

happyReduce_145 = happySpecReduce_3  33# happyReduction_145
happyReduction_145 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (happy_var_3:happy_var_1
	)}}

happyReduce_146 = happySpecReduce_1  34# happyReduction_146
happyReduction_146 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (reverse happy_var_1
	)}

happyReduce_147 = happySpecReduce_1  35# happyReduction_147
happyReduction_147 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 ([happy_var_1]
	)}

happyReduce_148 = happySpecReduce_2  35# happyReduction_148
happyReduction_148 happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn41
		 (happy_var_2:happy_var_1
	)}}

happyReduce_149 = happySpecReduce_3  36# happyReduction_149
happyReduction_149 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (happy_var_1:(reverse happy_var_3)
	)}}

happyReduce_150 = happySpecReduce_1  37# happyReduction_150
happyReduction_150 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (reverse happy_var_1
	)}

happyReduce_151 = happySpecReduce_0  38# happyReduction_151
happyReduction_151  =  happyIn44
		 ([]
	)

happyReduce_152 = happySpecReduce_1  38# happyReduction_152
happyReduction_152 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (reverse happy_var_1
	)}

happyReduce_153 = happySpecReduce_3  39# happyReduction_153
happyReduction_153 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (happy_var_3:happy_var_1
	)}}

happyReduce_154 = happySpecReduce_1  39# happyReduction_154
happyReduction_154 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 ([happy_var_1]
	)}

happyReduce_155 = happySpecReduce_0  40# happyReduction_155
happyReduction_155  =  happyIn46
		 ([]
	)

happyReduce_156 = happySpecReduce_2  40# happyReduction_156
happyReduction_156 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (happy_var_2
	)}

happyReduce_157 = happySpecReduce_3  41# happyReduction_157
happyReduction_157 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (annotate2 happy_var_1 happy_var_3 (Generator happy_var_1 happy_var_3)
	)}}

happyReduce_158 = happySpecReduce_3  41# happyReduction_158
happyReduction_158 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (annotate2 happy_var_1 happy_var_3 (Generator happy_var_1 happy_var_3)
	)}}

happyReduce_159 = happySpecReduce_1  41# happyReduction_159
happyReduction_159 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (annotate happy_var_1 (Qualifier happy_var_1)
	)}

happyReduce_160 = happySpecReduce_1  42# happyReduction_160
happyReduction_160 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (reverse happy_var_1
	)}

happyReduce_161 = happySpecReduce_3  43# happyReduction_161
happyReduction_161 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (happy_var_3:happy_var_1
	)}}

happyReduce_162 = happySpecReduce_1  43# happyReduction_162
happyReduction_162 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 ([happy_var_1]
	)}

happyNewToken action sts stk
	= getNextTokenWrapper(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	L _ TEOF -> happyDoAction 92# tk action sts stk;
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
	L _ TScope -> cont 37#;
	L _ TTimed -> cont 38#;
	L _ TAssert -> cont 39#;
	L _ TDeadlockFree -> cont 40#;
	L _ TLivelockFree -> cont 41#;
	L _ TDivergenceFree -> cont 42#;
	L _ TDeterministic -> cont 43#;
	L _ TTauPriority -> cont 44#;
	L _ (TRefines _) -> cont 45#;
	L _ (TModel _) -> cont 46#;
	L _ TNot -> cont 47#;
	L _ TAssertNot -> cont 48#;
	L _ TAnd -> cont 49#;
	L _ TOr -> cont 50#;
	L _ TEq -> cont 51#;
	L _ TNotEq -> cont 52#;
	L _ TLtEq -> cont 53#;
	L _ TGtEq -> cont 54#;
	L _ TEmptySeq -> cont 55#;
	L _ TLt -> cont 56#;
	L _ TGt -> cont 57#;
	L _ TCloseSeq -> cont 58#;
	L _ TPlus -> cont 59#;
	L _ TMinus -> cont 60#;
	L _ TTimes -> cont 61#;
	L _ TDivide -> cont 62#;
	L _ TMod -> cont 63#;
	L _ TConcat -> cont 64#;
	L _ THash -> cont 65#;
	L _ TLParen -> cont 66#;
	L _ TRParen -> cont 67#;
	L _ TLBrace -> cont 68#;
	L _ TRBrace -> cont 69#;
	L _ TLPipeBrace -> cont 70#;
	L _ TRPipeBrace -> cont 71#;
	L _ TLDoubleSqBracket -> cont 72#;
	L _ TRDoubleSqBracket -> cont 73#;
	L _ TLPipeSqBracket -> cont 74#;
	L _ TRPipeSqBracket -> cont 75#;
	L _ TLSqBracket -> cont 76#;
	L _ TRSqBracket -> cont 77#;
	L _ TExtChoice -> cont 78#;
	L _ TIntChoice -> cont 79#;
	L _ TInterleave -> cont 80#;
	L _ TPrefix -> cont 81#;
	L _ TInterrupt -> cont 82#;
	L _ TSlidingChoice -> cont 83#;
	L _ TRException -> cont 84#;
	L _ TParallel -> cont 85#;
	L _ TSemiColon -> cont 86#;
	L _ TGuard -> cont 87#;
	L _ TLSyncExtChoice -> cont 88#;
	L _ TRSyncExtChoice -> cont 89#;
	L _ TLSyncInterrupt -> cont 90#;
	L _ TRSyncInterrupt -> cont 91#;
	_ -> happyError' tk
	})

happyError_ 92# tk = happyError' tk
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
  happySomeParser = happyThen (happyParse 2#) (\x -> happyReturn (happyOut30 x))

happySeq = happyDontSeq


combineDecls :: [PDecl] -> [PDecl]
combineDecls [] = []
combineDecls ((An loc1 b (FunBind n ms)):(An loc2 c (FunBind n' ms')):ds) | n == n' = 
    combineDecls $ (An (combineSpans loc1 loc2) b (FunBind n (ms++ms'))):ds
combineDecls (d:ds) = d:combineDecls ds

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
            where
                (ps, n) = getPats exp

        convPatBind exp = PatBind (convPat exp) rhs
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
                check (FunBind a b) = True
                check (PatBind a b) = True
                check (External a) = True
                check (Transparent a) = True
                check (TimedSection _ _ ds) = and (map checkDecl ds)
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
