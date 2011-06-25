module Util.Annotated where

import Util.PrettyPrint

data SrcLoc
instance Ord SrcLoc
data SrcSpan
instance Eq SrcSpan
instance Ord SrcSpan
instance Show SrcSpan
instance PrettyPrintable SrcSpan
combineSpans :: SrcSpan -> SrcSpan -> SrcSpan

data Located a 