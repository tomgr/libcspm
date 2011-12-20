-- | Misc utility functions that are defined on monads.
module Util.Monad (
    concatMapM,
    andM,
    orM,
    ($$)
) where

import Control.Monad

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

andM :: (Monad m) => [m Bool] -> m Bool
andM [m] = m
andM (m:ms) = do
    b <- m
    if b then andM ms else return False

orM :: (Monad m) => [m Bool] -> m Bool
orM [m] = m
orM (m:ms) = do
    b <- m
    if not b then orM ms else return True

($$) :: Monad m => m (a -> b) -> m a -> m b
($$) fm argm = do
    f <- fm
    arg <- argm
    return $ f arg
