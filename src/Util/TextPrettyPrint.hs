module Util.TextPrettyPrint where

import Data.Monoid
import qualified Data.Text as ST
import qualified Data.Text.Lazy as T hiding (singleton)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as T

text :: String -> T.Builder
text = T.fromString

stext :: ST.Text -> T.Builder
stext = T.fromText

ltext :: T.Text -> T.Builder
ltext = T.fromLazyText

integral :: Integral a => a -> T.Builder
integral a = T.decimal a

comma, dot :: T.Builder
comma = T.singleton ','
dot = T.singleton '.'

wrap :: T.Builder -> T.Builder -> T.Builder -> T.Builder
wrap l t r = l `mappend` t `mappend` r

angles, braces, parens :: T.Builder -> T.Builder
parens b = wrap (T.singleton '(') b (T.singleton ')')
angles b = wrap (T.singleton '<') b (T.singleton '>')
braces b = wrap (T.singleton '{') b (T.singleton '}')

punctuate :: T.Builder -> [T.Builder] -> T.Builder
punctuate p [] = mempty
punctuate p [x] = x
punctuate p (x:xs) = x `mappend` p `mappend` punctuate p xs

list :: [T.Builder] -> T.Builder
list = punctuate comma

class FastPrettyPrintable a where
    toText :: a -> ST.Text
    toText a = T.toStrict (toLazyText a)

    toLazyText :: a -> T.Text
    toLazyText a = T.toLazyText (toBuilder a)

    toBuilder :: a -> T.Builder
