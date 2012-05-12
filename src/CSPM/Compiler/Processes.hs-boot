module CSPM.Compiler.Processes where

data ProcName ops
data Proc op pn
type UProc ops = Proc ops (ProcName ops)
