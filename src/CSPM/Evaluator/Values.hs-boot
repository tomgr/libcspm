module CSPM.Evaluator.Values where

import Util.PrettyPrint

data Value
instance Eq Value
instance Ord Value
instance PrettyPrintable Value
instance Show Value
