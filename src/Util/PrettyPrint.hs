module Util.PrettyPrint (
    module Text.PrettyPrint.HughesPJ,
    PrettyPrintable (prettyPrint),
    tabWidth,
    tabIndent,
    shortDouble,
    commaSeparatedInt,
    angles, bars, list, dotSep,
    speakNth
)
where 

import Numeric
import Text.Printf
import Text.PrettyPrint.HughesPJ

class PrettyPrintable a where
    prettyPrint :: a -> Doc

-- | The width, in spaces, of a tab character.
tabWidth :: Int
tabWidth = 4

-- | Pretty prints an integer and separates it into groups of 3, separated by
-- commas.
commaSeparatedInt :: Int -> Doc
commaSeparatedInt =
    let
        breakIntoGroupsOf3 (c1:c2:c3:c4:cs) = 
            [c3,c2,c1] : breakIntoGroupsOf3 (c4:cs)
        breakIntoGroupsOf3 cs = [reverse cs]
    in 
    fcat . punctuate comma . reverse . map text 
        . breakIntoGroupsOf3 . reverse . show

-- | Show a double `d` printing only `places` places after the decimal place.
shortDouble :: Int -> Double -> Doc
shortDouble places d = text (showGFloat (Just places) d "")

-- | Indent a document by `tabWidth` characters, on each line
-- (uses `nest`).
tabIndent :: Doc -> Doc
tabIndent = nest tabWidth

-- | Surrounds a `Doc` with '<' and '>'.
angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

-- | Surrounds a `Doc` with '|'.
bars :: Doc -> Doc
bars d = char '|' <> d <> char '|'

-- | Separates a list of `Doc`s by '.'.
dotSep :: [Doc] -> Doc
dotSep docs = fcat (punctuate (text ".") docs)

-- | Separates a list of `Doc`s by ','.
list :: [Doc] -> Doc
list docs = fsep (punctuate (text ",") docs)

-- | Converts a number into 'first', 'second' etc.
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
        | n <= 20       = "th"  -- 11,12,13 are non-std
        | last_dig == 1 = "st"
        | last_dig == 2 = "nd"
        | last_dig == 3 = "rd"
        | otherwise     = "th"
    last_dig = n `rem` 10
