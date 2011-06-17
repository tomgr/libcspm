module Util.Annotated where

-- From GHC TODO: use FastString for file names
{- |
A SrcSpan delimits a portion of a text file.  It could be represented
by a pair of (line,column) coordinates, but in fact we optimise
slightly by using more compact representations for single-line and
zero-length spans, both of which are quite common.

The end position is defined to be the column /after/ the end of the
span.  That is, a span of (1,1)-(1,2) is one character long, and a
span of (1,1)-(1,1) is zero characters long.
-}
data SrcSpan
	= SrcSpanOneLine 		-- a common case: a single line
	{ srcSpanFile     :: String,
		srcSpanLine     :: {-# UNPACK #-} !Int,
		srcSpanSCol     :: {-# UNPACK #-} !Int,
		srcSpanECol     :: {-# UNPACK #-} !Int
	}
	| SrcSpanMultiLine
	{ srcSpanFile	  :: String,
		srcSpanSLine    :: {-# UNPACK #-} !Int,
		srcSpanSCol	  :: {-# UNPACK #-} !Int,
		srcSpanELine    :: {-# UNPACK #-} !Int,
		srcSpanECol     :: {-# UNPACK #-} !Int
	}
	| SrcSpanPoint
	{ srcSpanFile	  :: String,
		srcSpanLine	  :: {-# UNPACK #-} !Int,
		srcSpanCol      :: {-# UNPACK #-} !Int
	}
	| Unknown
	deriving (Eq)
	
instance Show SrcSpan where
	show (SrcSpanOneLine f sline scol1 ecol1) = f++":"++(show sline)++"::"++(show scol1)++"-"++(show ecol1)
	show (SrcSpanMultiLine f sline scol1 eline ecol1) = f++":"++(show sline)++"::"++(show scol1)++"-"++(show eline)++"::"++(show ecol1)
	
combineSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSpans s1 s2 | srcSpanFile s1 /= srcSpanFile s2 = error "Spans files"
combineSpans (SrcSpanOneLine f1 line1 scol1 ecol1) 
		(SrcSpanOneLine f2 line2 scol2 ecol2) = 
	if line1 == line2 then SrcSpanOneLine f1 line1 scol1 ecol2
	else SrcSpanMultiLine f1 line1 scol1 line2 ecol2
combineSpans (SrcSpanOneLine f1 sline1 scol1 ecol1) 
		(SrcSpanMultiLine f2 sline2 scol2 eline2 ecol2) = 
	SrcSpanMultiLine f1 sline1 scol1 eline2 ecol2
combineSpans (SrcSpanMultiLine f1 sline1 scol1 eline1 ecol1)
		(SrcSpanOneLine f2 eline2 scol2 ecol2) =
	SrcSpanMultiLine f1 sline1 scol1 eline2 ecol2
combineSpans (SrcSpanMultiLine f1 sline1 scol1 eline1 ecol1) 
		(SrcSpanMultiLine f2 sline2 scol2 eline2 ecol2) =
	SrcSpanMultiLine f1 sline1 scol1 eline2 ecol2
combineSpans f1 f2 = error (show f1 ++ show f2)

data Located a = 
	L SrcSpan a 
	deriving (Eq, Show)
	
data Annotated a b = 
	An SrcSpan a b

unAnnotate :: Annotated a b -> b
unAnnotate (An _ _ b) = b
	
instance Show b => Show (Annotated a b) where
	show (An _ _ b) = show b

instance Eq b => Eq (Annotated a b) where
	(An _ _ b1) == (An _ _ b2) = b1 == b2
