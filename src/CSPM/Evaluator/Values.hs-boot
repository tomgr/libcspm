module CSPM.Evaluator.Values where

import Data.Hashable
import Util.PrettyPrint

data Value
instance Eq Value
instance Hashable Value
instance Ord Value
instance PrettyPrintable Value
instance Show Value
