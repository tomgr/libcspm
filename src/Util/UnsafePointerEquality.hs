{-# LANGUAGE MagicHash #-}
module Util.UnsafePointerEquality (
    unsafePointerEquality,
)
where

import GHC.Prim

unsafePointerEquality :: a -> a -> Bool
unsafePointerEquality a b = 
    case reallyUnsafePtrEquality# a b of
        0# -> False
        1# -> True
{-# INLINE unsafePointerEquality #-}
