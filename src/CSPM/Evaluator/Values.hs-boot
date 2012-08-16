module CSPM.Evaluator.Values where

import Data.Hashable

data Value
instance Eq Value
instance Hashable Value
instance Ord Value

data ScopeIdentifier
instance Eq ScopeIdentifier
instance Hashable ScopeIdentifier
instance Ord ScopeIdentifier

trimValueForProcessName :: Value -> Value
