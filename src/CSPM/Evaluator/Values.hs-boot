module CSPM.Evaluator.Values where

import Data.Hashable

data Value
instance Eq Value
instance Hashable Value
instance Ord Value
