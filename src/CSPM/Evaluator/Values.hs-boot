{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module CSPM.Evaluator.Values where

import {-# SOURCE #-} CSPM.Compiler.Processes
import Data.Hashable
import Util.PrettyPrint
import qualified Util.TextPrettyPrint as T

data Value ops
instance Eq (Value ops)
instance Hashable (Value ops)
instance Ord (Value ops)
instance PrettyPrintable (UProc ops) => PrettyPrintable (Value ops)
instance PrettyPrintable (UProc ops) => T.FastPrettyPrintable (Value ops)
instance PrettyPrintable (UProc ops) => Show (Value ops)
