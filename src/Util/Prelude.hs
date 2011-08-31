module Util.Prelude where

import Data.Char
import Prelude
import System.Directory
import System.FilePath

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ x = x
thenCmp x _ = x

expandPathIO :: String -> IO String
expandPathIO ('~':'/':d) = do
    tilde <- getHomeDirectory -- will fail if HOME not defined
    return $ joinPath [tilde, d]
expandPathIO other = return other

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

cartProduct :: [[a]] -> [[a]]
cartProduct = sequence
