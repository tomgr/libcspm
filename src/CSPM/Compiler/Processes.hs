{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, 
    OverloadedStrings, UndecidableInstances #-}
-- | This module provides the input data structure to the compiler.
module CSPM.Compiler.Processes (
    Proc(..), UProc,
    ProcOperator(..), 
    ProcName(..),
    operator, components,
    prettyPrintAllRequiredProcesses,
) where

import CSPM.DataStructures.Names
import {-# SOURCE #-} CSPM.Evaluator.Values
import qualified Data.Foldable as F
import qualified Data.Functor as F
import qualified Data.Sequence as S
import Data.Hashable
import Util.PrettyPrint
import qualified Util.TextPrettyPrint as T

type UProc ops = Proc ops (ProcName ops)

-- | ProcNames uniquely identify processes.
data ProcName ops =
    ProcName {
        -- | The name of this process (recal Name s are unique).
        name :: Name,
        -- | The arguments applied to this process, in case it was a function
        -- call.
        arguments :: [[Value ops]],
        -- | The parent of this proc name. This is used in let expressions.
        parent :: Maybe (ProcName ops)
    }
    -- | A proccess that has no name, but needs disambgiuation. These are
    -- used in prefixing to avoid problems with things like:
    -- P = c?x -> (let Q = ... within Q) as the ... can depend on x.
    | AnnonymousProcName {
        arguments :: [[Value ops]],
        parent :: Maybe (ProcName ops)
    }
    deriving Eq

instance Hashable (ProcName ops) where
    hash (ProcName n vss p) = combine 1 (combine (hash n) (combine (hash vss) (hash p)))
    hash (AnnonymousProcName as ps) = combine 2 (combine (hash as) (hash ps))
instance PrettyPrintable (UProc ops) => PrettyPrintable (ProcName ops) where
    prettyPrint (ProcName n args Nothing) =
        prettyPrint n
        <> hcat (map (\as -> parens (list (map prettyPrint as))) args)
    prettyPrint (ProcName n args (Just pn)) =
        prettyPrint pn <> colon<>colon <> prettyPrint (ProcName n args Nothing)
    prettyPrint (AnnonymousProcName args Nothing) =
        text "ANNON"
        <> hcat (map (\as -> parens (list (map prettyPrint as))) args)
    prettyPrint (AnnonymousProcName args (Just pn)) =
        prettyPrint pn <> colon<>colon <> prettyPrint (AnnonymousProcName args Nothing)
instance PrettyPrintable (UProc ops) => Show (ProcName ops) where
    show pn = show (prettyPrint pn)

instance PrettyPrintable (UProc ops) => T.FastPrettyPrintable (ProcName ops) where
    toBuilder (ProcName n args Nothing) =
        T.toBuilder n
        T.<> T.hcat (map (\as -> T.parens (T.list (map T.toBuilder as))) args)
    toBuilder (ProcName n args (Just pn)) =
        T.toBuilder n T.<> T.stext "::" T.<> T.toBuilder (ProcName n args Nothing)
    toBuilder (AnnonymousProcName args Nothing) =
        T.stext "ANNON"
        T.<> T.hcat (map (\as -> T.parens (T.list (map T.toBuilder as))) args)
    toBuilder (AnnonymousProcName args (Just pn)) =
        T.toBuilder pn T.<> T.stext "::" T.<> T.toBuilder (AnnonymousProcName args Nothing)

-- | An operator that can be applied to processes.
data ProcOperator =
    Chase 
    | Diamond 
    | Explicate 
    | Normalize 
    | ModelCompress
    | StrongBisim 
    | TauLoopFactor 
    | WeakBisim
    deriving (Eq)

instance PrettyPrintable ProcOperator where
    prettyPrint Chase = text "chase"
    prettyPrint Diamond = text "diamond"
    prettyPrint Explicate = text "explicate"
    prettyPrint Normalize = text "normal"
    prettyPrint ModelCompress = text "model_compress"
    prettyPrint StrongBisim = text "sbisim"
    prettyPrint TauLoopFactor = text "tau_loop_factor"
    prettyPrint WeakBisim = text "wbisim"

instance Show ProcOperator where
    show p = show (prettyPrint p)

-- | A compiled process. Note this is an infinite data structure (due to
-- PProcCall) as this makes compilation easy (we can easily chase
-- dependencies).
data Proc op pn = 
    PUnaryOp op (Proc op pn)
    | PBinaryOp op (Proc op pn) (Proc op pn)
    | POp op (S.Seq (Proc op pn))
    -- | Labels the process this contains. This allows infinite loops to be
    -- spotted.
    | PProcCall pn (Proc op pn)

-- | Gives the operator of a process. If the process is a ProcCall an error is
-- thrown.
operator :: Proc op pn -> op
operator (PUnaryOp op _) = op
operator (PBinaryOp op _ _)=  op
operator (POp op _) = op

-- | Returns the components of a given process.
components :: Proc op pn -> S.Seq (Proc op pn)
components (PBinaryOp _ p1 p2) = p1 S.<| p2 S.<| S.empty
components (POp _ ps) = ps
components (PUnaryOp _ p1) = S.singleton p1

-- | Given a process, returns the initial process and all processes that it
-- calls.
splitProcIntoComponents :: 
    Proc op (ProcName op) -> (Proc op (ProcName op), [(ProcName op, Proc op (ProcName op))])
splitProcIntoComponents p =
    let
        explored pns n = n `elem` (map fst pns)

        exploreAll pns [] = pns
        exploreAll pns (p:ps) = exploreAll (explore pns p) ps

        explore pns (PUnaryOp _ p) = explore pns p
        explore pns (PBinaryOp _ p1 p2) = exploreAll pns [p1,p2]
        explore pns (POp _ ps) = exploreAll pns (F.toList ps)
        explore pns (PProcCall n p) =
            if explored pns n then pns
            else explore ((n, p):pns) p
    in (p, explore [] p)

-- | Pretty prints the given process and all processes that it depends upon.
prettyPrintAllRequiredProcesses :: PrettyPrintable (UProc op) => Proc op (ProcName op) -> Doc
prettyPrintAllRequiredProcesses p =
    let
        (pInit, namedPs) = splitProcIntoComponents p
        ppNamedProc (n,p) =
            hang (prettyPrint n <+> char '=') tabWidth (prettyPrint p)
    in 
        vcat (punctuate (char '\n') ((map ppNamedProc namedPs)++[prettyPrint pInit]))
