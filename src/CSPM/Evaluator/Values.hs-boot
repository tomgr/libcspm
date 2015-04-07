module CSPM.Evaluator.Values where

import Data.Hashable

data Value
instance Eq Value
instance Hashable Value
instance Ord Value

data InstantiatedFrame
instance Eq InstantiatedFrame
instance Hashable InstantiatedFrame
instance Ord InstantiatedFrame

trimValueForProcessName :: Value -> Value
