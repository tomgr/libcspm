{-# LANGUAGE PatternGuards #-}
-- | Provides fuzzy string matching.
--
-- Taken from GHC (which is BSD licensed and thus compatible). Copyright
-- reproduced below:
--
-- (c) The University of Glasgow 2006
module Util.FuzzyLookup (
    fuzzyMatch, fuzzyLookup,
) where

import Data.Bits
import Data.Char
import qualified Data.IntMap as IM
import Data.List
import Data.Ord (comparing)
import Data.Word

fuzzyMatch :: String -> [String] -> [String]
fuzzyMatch key vals = fuzzyLookup key [(v,v) | v <- vals]

-- | Search for possible matches to the users input in the given list,
-- returning a small number of ranked results
fuzzyLookup :: String -> [(String,a)] -> [a]
fuzzyLookup user_entered possibilites
  = map fst $ take mAX_RESULTS $ sortBy (comparing snd)
    [ (poss_val, distance) | (poss_str, poss_val) <- possibilites
                       , let distance = restrictedDamerauLevenshteinDistance
                                            poss_str user_entered
                       , distance <= fuzzy_threshold ]
  where
    -- Work out an approriate match threshold:
    -- We report a candidate if its edit distance is <= the threshold,
    -- The threshhold is set to about a quarter of the # of characters the user entered
    --   Length    Threshold
    --     1         0          -- Don't suggest *any* candidates
    --     2         1          -- for single-char identifiers
    --     3         1
    --     4         1
    --     5         1
    --     6         2
    --
    fuzzy_threshold = truncate $ fromIntegral (length user_entered + 2) / (4 :: Rational)
    mAX_RESULTS = 3

restrictedDamerauLevenshteinDistance :: String -> String -> Int
restrictedDamerauLevenshteinDistance str1 str2
  = restrictedDamerauLevenshteinDistanceWithLengths m n str1 str2
  where
    m = length str1
    n = length str2

restrictedDamerauLevenshteinDistanceWithLengths
  :: Int -> Int -> String -> String -> Int
restrictedDamerauLevenshteinDistanceWithLengths m n str1 str2
  | m <= n
  = if n <= 32 -- n must be larger so this check is sufficient
    then restrictedDamerauLevenshteinDistance' (undefined :: Word32) m n str1 str2
    else restrictedDamerauLevenshteinDistance' (undefined :: Integer) m n str1 str2

  | otherwise
  = if m <= 32 -- m must be larger so this check is sufficient
    then restrictedDamerauLevenshteinDistance' (undefined :: Word32) n m str2 str1
    else restrictedDamerauLevenshteinDistance' (undefined :: Integer) n m str2 str1

restrictedDamerauLevenshteinDistance'
  :: (Bits bv, Num bv) => bv -> Int -> Int -> String -> String -> Int
restrictedDamerauLevenshteinDistance' _bv_dummy m n str1 str2
  | [] <- str1 = n
  | otherwise  = extractAnswer $
                 foldl' (restrictedDamerauLevenshteinDistanceWorker
                             (matchVectors str1) top_bit_mask vector_mask)
                        (0, 0, m_ones, 0, m) str2
  where
    m_ones@vector_mask = (2 ^ m) - 1
    top_bit_mask = (1 `shiftL` (m - 1)) `asTypeOf` _bv_dummy
    extractAnswer (_, _, _, _, distance) = distance

restrictedDamerauLevenshteinDistanceWorker
      :: (Bits bv, Num bv) => IM.IntMap bv -> bv -> bv
      -> (bv, bv, bv, bv, Int) -> Char -> (bv, bv, bv, bv, Int)
restrictedDamerauLevenshteinDistanceWorker str1_mvs top_bit_mask vector_mask
                                           (pm, d0, vp, vn, distance) char2
  = seq str1_mvs $ seq top_bit_mask $ seq vector_mask $
    seq pm' $ seq d0' $ seq vp' $ seq vn' $
    seq distance'' $ seq char2 $
    (pm', d0', vp', vn', distance'')
  where
    pm' = IM.findWithDefault 0 (ord char2) str1_mvs

    d0' = ((((sizedComplement vector_mask d0) .&. pm') `shiftL` 1) .&. pm)
      .|. ((((pm' .&. vp) + vp) .&. vector_mask) `xor` vp) .|. pm' .|. vn
          -- No need to mask the shiftL because of the restricted range of pm

    hp' = vn .|. sizedComplement vector_mask (d0' .|. vp)
    hn' = d0' .&. vp

    hp'_shift = ((hp' `shiftL` 1) .|. 1) .&. vector_mask
    hn'_shift = (hn' `shiftL` 1) .&. vector_mask
    vp' = hn'_shift .|. sizedComplement vector_mask (d0' .|. hp'_shift)
    vn' = d0' .&. hp'_shift

    distance' = if hp' .&. top_bit_mask /= 0 then distance + 1 else distance
    distance'' = if hn' .&. top_bit_mask /= 0 then distance' - 1 else distance'

sizedComplement :: Bits bv => bv -> bv -> bv
sizedComplement vector_mask vect = vector_mask `xor` vect

matchVectors :: (Bits bv, Num bv) => String -> IM.IntMap bv
matchVectors = snd . foldl' go (0 :: Int, IM.empty)
  where
    go (ix, im) char = let ix' = ix + 1
                           im' = IM.insertWith (.|.) (ord char) (2 ^ ix) im
                       in seq ix' $ seq im' $ (ix', im')
