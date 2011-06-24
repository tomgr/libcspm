module Util.PrettyPrint (
	module Text.PrettyPrint.HughesPJ,
	PrettyPrintable (prettyPrint),
	tabWidth,
	tabIndent,
	angles, bars, list, dotSep,
	speakNth
)
where 

import Text.PrettyPrint.HughesPJ

class PrettyPrintable a where
	prettyPrint :: a -> Doc
	
tabWidth = 4
tabIndent = nest tabWidth

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

bars :: Doc -> Doc
bars d = char '|' <> d <> char '|'

dotSep :: [Doc] -> Doc
dotSep docs = fcat (punctuate (text ".") docs)

list :: [Doc] -> Doc
list docs =	
	fsep (punctuate (text ",") docs)

speakNth :: Int -> Doc
speakNth 1 = text "first"
speakNth 2 = text "second"
speakNth 3 = text "third"
speakNth 4 = text "fourth"
speakNth 5 = text "fifth"
speakNth 6 = text "sixth"
speakNth n = hcat [ int n, text suffix ]
	where
	suffix 
		| n <= 20       = "th"	-- 11,12,13 are non-std
		| last_dig == 1 = "st"
		| last_dig == 2 = "nd"
		| last_dig == 3 = "rd"
		| otherwise     = "th"
	last_dig = n `rem` 10
