{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Util.Annotated where

import Data.Hashable
import Prelude
import Util.Exception
import Util.Prelude
import Util.PrettyPrint

data SrcLoc = 
    SrcLoc {
        srcLocFile :: String,
        srcLocLine :: !Int,
        srcLocCol :: !Int
    }
    | NoLoc
    deriving Eq
    
instance Ord SrcLoc where
    (SrcLoc f1 l1 c1) `compare` (SrcLoc f2 l2 c2) =
        (f1 `compare` f2) `thenCmp` 
        (l1 `compare` l2) `thenCmp` 
        (c1 `compare` c2)
    NoLoc `compare` NoLoc = EQ
    NoLoc `compare` _ = LT
    _ `compare` NoLoc = GT
    
-- From GHC
data SrcSpan = 
    SrcSpanOneLine { 
        srcSpanFile :: String,
        srcSpanLine :: !Int,
        srcSpanSCol :: !Int,
        srcSpanECol :: !Int
    }
    | SrcSpanMultiLine { 
        srcSpanFile :: String,
        srcSpanSLine :: !Int,
        srcSpanSCol :: !Int,
        srcSpanELine :: !Int,
        srcSpanECol :: !Int
    }
    | SrcSpanPoint { 
        srcSpanFile :: String,
        srcSpanLine :: !Int,
        srcSpanCol :: !Int
    }
    | Unknown
    -- | A builtin thing
    | BuiltIn
    deriving Eq
    
srcSpanStart :: SrcSpan -> SrcLoc
srcSpanStart (SrcSpanOneLine f l sc _) = SrcLoc f l sc
srcSpanStart (SrcSpanMultiLine f sl sc _ _) = SrcLoc f sl sc
srcSpanStart (SrcSpanPoint f l c) = SrcLoc f l c
srcSpanStart BuiltIn = NoLoc
srcSpanStart Unknown = NoLoc

srcSpanEnd :: SrcSpan -> SrcLoc
srcSpanEnd (SrcSpanOneLine f l _ ec) = SrcLoc f l ec
srcSpanEnd (SrcSpanMultiLine f _ _ el ec) = SrcLoc f el ec
srcSpanEnd (SrcSpanPoint f l c) = SrcLoc f l c
srcSpanEnd BuiltIn = NoLoc
srcSpanEnd Unknown = NoLoc

-- We want to order SrcSpans first by the start point, then by the end point.
instance Ord SrcSpan where
    a `compare` b = 
        (srcSpanStart a `compare` srcSpanStart b) `thenCmp` 
        (srcSpanEnd   a `compare` srcSpanEnd   b)

instance Show SrcSpan where
    show s = show (prettyPrint s)

instance PrettyPrintable SrcSpan where
    prettyPrint (SrcSpanOneLine f sline scol1 ecol1) = 
        text f <> colon <> int sline <> colon <> int scol1 <> char '-' <> int ecol1
    prettyPrint (SrcSpanMultiLine f sline scol eline ecol) = 
        text f <> colon <> int sline <> colon <> int scol
                        <> char '-' 
                        <> int eline <> colon <> int ecol
    prettyPrint (SrcSpanPoint f sline scol) = 
        text f <> colon <> int sline <> colon <> int scol
    prettyPrint Unknown = text "<unknown location>"
    prettyPrint BuiltIn = text "<built-in>"
    
combineSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSpans s1 s2 | srcSpanFile s1 /= srcSpanFile s2 = panic $ show $
    text "Cannot combine spans as they span files"
    $$ tabIndent (prettyPrint s1 $$ prettyPrint s2)
combineSpans (SrcSpanOneLine f1 line1 scol1 _) 
        (SrcSpanOneLine _ line2 _ ecol2) = 
    if line1 == line2 then SrcSpanOneLine f1 line1 scol1 ecol2
    else SrcSpanMultiLine f1 line1 scol1 line2 ecol2
combineSpans (SrcSpanOneLine f1 sline1 scol1 _) 
        (SrcSpanMultiLine _ _ _ eline2 ecol2) = 
    SrcSpanMultiLine f1 sline1 scol1 eline2 ecol2
combineSpans (SrcSpanMultiLine f1 sline1 scol1 _ _)
        (SrcSpanOneLine _ eline2 _ ecol2) =
    SrcSpanMultiLine f1 sline1 scol1 eline2 ecol2
combineSpans (SrcSpanMultiLine f1 sline1 scol1 _ _) 
        (SrcSpanMultiLine _ _ _ eline2 ecol2) =
    SrcSpanMultiLine f1 sline1 scol1 eline2 ecol2
combineSpans s1 s2 = panic $ show $
    text "combineSpans: invalid spans combined"
    $$ tabIndent (prettyPrint s1 $$ prettyPrint s2)

data Located a = 
    L {
        locatedLoc :: SrcSpan,
        locatedInner :: a 
    }
    
data Annotated a b = 
    An {
        loc :: SrcSpan,
        annotation :: a,
        inner :: b
    }

dummyAnnotation :: a
dummyAnnotation = panic "Dummy annotation evaluated"

unAnnotate :: Annotated a b -> b
unAnnotate (An _ _ b) = b
    
instance Show b => Show (Annotated a b) where
    show (An _ _ b) = show b
instance Show a => Show (Located a) where
    show (L _ a) = show a

instance (PrettyPrintable [b]) => PrettyPrintable [Annotated a b] where
    prettyPrint ans = prettyPrint (map unAnnotate ans)
instance (PrettyPrintable b) => PrettyPrintable (Annotated a b) where
    prettyPrint (An _ _ inner) = prettyPrint inner
instance (PrettyPrintable a) => PrettyPrintable (Located a) where
    prettyPrint (L _ inner) = prettyPrint inner

instance Eq b => Eq (Annotated a b) where
    (An _ _ b1) == (An _ _ b2) = b1 == b2
instance Eq a => Eq (Located a) where
    (L _ b1) == (L _ b2) = b1 == b2

instance Ord b => Ord (Annotated a b) where
    compare a b = compare (unAnnotate a) (unAnnotate b)
instance Ord b => Ord (Located b) where
    compare a b = compare (locatedInner a) (locatedInner b)

instance Hashable b => Hashable (Annotated a b) where
    hash a = hash (unAnnotate a)
