-- | Misc utility functions that are defined on monads.
module Util.Monad (
    concatMapM,
    andM,
    orM,
    ($$)
) where

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = 
    let
        cm [] accum = return accum
        cm (x:xs) accum = do
            ys <- f x
            cm xs (accum++ys)
    in cm xs []
{-# INLINE concatMapM #-}

andM :: (Monad m) => [m Bool] -> m Bool
andM [m] = m
andM (m:ms) = do
    b <- m
    if b then andM ms else return False
andM _ = error "andM of empty list"

orM :: (Monad m) => [m Bool] -> m Bool
orM [m] = m
orM (m:ms) = do
    b <- m
    if not b then orM ms else return True
orM _ = error "orM of empty list"

($$) :: Monad m => m (a -> b) -> m a -> m b
($$) fm argm = do
    f <- fm
    arg <- argm
    return $ f arg
