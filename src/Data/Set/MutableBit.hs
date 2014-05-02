{-# LANGUAGE BangPatterns #-}
-- | Bit sets represent sets of integers by setting bit i to 1 iff i is in the
-- set. This means they can effeciently support many operations, like union
-- (bitwise or), intersection (bitwise and) etc. However, they obviously
-- can only represent sets of relatively small integers, as they require
-- O(max(S)) bits.
module Data.Set.MutableBit (
    -- * Mutable Bit Sets
    Set, 
    -- ** Creation
    newSized,
    -- ** Standard Operations
    member,
    insert,
    remove,
)
where

import Control.Monad.ST
import Data.Array.ST
import Data.Bits
import Data.Word
import Prelude hiding (map, mapM)

type Bucket = Word64
type Set s = STUArray s Int Bucket

bucketBitCount :: Int
bucketBitCount = finiteBitSize (undefined :: Bucket)

newSized :: Int -> ST s (Set s)
newSized maxVal = newArray (0, bucketCount-1) 0
    where bucketCount = 1+(maxVal `div` bucketBitCount)

member :: Set s -> Int -> ST s Bool
member set entry = do
    let (bucketIx, ixInBucket) = entry `divMod` bucketBitCount
    bucket <- readArray set bucketIx
    return $! testBit bucket ixInBucket

insert :: Set s -> Int -> ST s ()
insert set entry = do
    let (bucketIx, ixInBucket) = entry `divMod` bucketBitCount
    bucket <- readArray set bucketIx
    writeArray set bucketIx $! setBit bucket ixInBucket

remove :: Set s -> Int -> ST s ()
remove set entry = do
    let (bucketIx, ixInBucket) = entry `divMod` bucketBitCount
    bucket <- readArray set bucketIx
    writeArray set bucketIx $! clearBit bucket ixInBucket
