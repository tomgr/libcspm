module Util.Prelude where

import Data.Char
import Prelude
import System.Directory
import System.FilePath

-- | Given two orderings, returns the second if the first is 
-- `EQ` and returns the first otherwise.
thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ x = x
thenCmp x _ = x

-- | Given a file path, if the first character is a ~ then
-- expands the ~ to the users' home directory.
expandPathIO :: String -> IO String
expandPathIO ('~':'/':d) = do
    tilde <- getHomeDirectory -- will fail if HOME not defined
    return $ joinPath [tilde, d]
expandPathIO other = return other

-- | Remove whitespace from the beginning and end of a string.
trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- | Compute the cartesian product of a list of lists.
cartProduct :: [[a]] -> [[a]]
cartProduct = sequence
