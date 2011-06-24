module Util.Prelude where

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ x = x
thenCmp x _ = x
