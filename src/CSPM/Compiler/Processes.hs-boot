{-# LANGUAGE KindSignatures #-}
module CSPM.Compiler.Processes where

import qualified Data.Sequence as S

data ProcName ops
data Proc (seq :: * -> *) op pn
type UProc ops = Proc S.Seq ops (ProcName ops)
