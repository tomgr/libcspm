{-# LANGUAGE MultiParamTypeClasses #-}
module Util.MonadicPrettyPrint(
    module Util.MonadicPrettyPrintInternal,
    MonadicPrettyPrintable(..),
    tabWidth,
    tabIndent,
    shortDouble,
    commaSeparatedInt,
    angles, bars, list, dotSep,
    speakNth,
    punctuateFront,
) where

import Control.Applicative hiding (empty)
import Numeric
import Util.MonadicPrettyPrintInternal

class MonadicPrettyPrintable m a where
    prettyPrint :: a -> m Doc

-- | The width, in spaces, of a tab character.
tabWidth :: Int
tabWidth = 4

-- | Pretty prints an integer and separates it into groups of 3, separated by
-- commas.
commaSeparatedInt :: (Monad m, Applicative m) => Int -> m Doc
commaSeparatedInt =
    let 
        breakIntoGroupsOf3 :: String -> [String]
        breakIntoGroupsOf3 (c1:c2:c3:c4:cs) = 
            [c3,c2,c1] : breakIntoGroupsOf3 (c4:cs)
        breakIntoGroupsOf3 cs = [reverse cs]
    in fcat . punctuate comma . sequence . reverse . map text
        . breakIntoGroupsOf3 . reverse . show

-- | Show a double `d` printing only `places` places after the decimal place.
shortDouble :: (Monad m, Applicative m) => Int -> Double -> m Doc
shortDouble places d = text (showGFloat (Just places) d "")

-- | Indent a document by `tabWidth` characters, on each line
-- (uses `nest`).
tabIndent :: (Monad m, Applicative m) => m Doc -> m Doc
tabIndent = nest tabWidth

-- | Surrounds a `Doc` with '<' and '>'.
angles :: (Monad m, Applicative m) => m Doc -> m Doc
angles d = char '<' <> d <> char '>'

-- | Surrounds a `Doc` with '|'.
bars :: (Monad m, Applicative m) => m Doc -> m Doc
bars d = char '|' <> d <> char '|'

-- | Separates a list of `Doc`s by '.'.
dotSep :: (Monad m, Applicative m) => m [Doc] -> m Doc
dotSep docs = fcat (punctuate (text ".") docs)

-- | Separates a list of `Doc`s by ','.
list :: (Monad m, Applicative m) => m [Doc] -> m Doc
list docs = fsep (punctuate (text ",") docs)

-- | Converts a number into 'first', 'second' etc.
speakNth :: (Monad m, Applicative m) => Int -> m Doc
speakNth 1 = text "first"
speakNth 2 = text "second"
speakNth 3 = text "third"
speakNth 4 = text "fourth"
speakNth 5 = text "fifth"
speakNth 6 = text "sixth"
speakNth n = hcat $ sequence [ int n, text suffix ]
    where
    suffix 
        | n <= 20       = "th"  -- 11,12,13 are non-std
        | last_dig == 1 = "st"
        | last_dig == 2 = "nd"
        | last_dig == 3 = "rd"
        | otherwise     = "th"
    last_dig = n `rem` 10

-- | Equivalent to [d1, sep <> d2, sep <> d3, ...].
punctuateFront :: (Monad m, Applicative m) => m Doc -> m [Doc] -> m [Doc]
punctuateFront sep dsm = dsm >>= \ds ->
    case ds of
        [] -> return []
        (x:xs) -> sequence [sep <> return x | x <- xs] >>= return . (x:)
