module CSPM.Evaluator.Values where

import Data.Hashable
import Util.PrettyPrint
import qualified Util.TextPrettyPrint as T

data Value
instance Eq Value
instance Hashable Value
instance Ord Value
instance PrettyPrintable Value
instance T.FastPrettyPrintable Value
instance Show Value
