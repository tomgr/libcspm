{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    TypeSynonymInstances #-}
module CSPM.Evaluator.ProcessValues (
    -- * Events
    Event(..),
    EventSet, eventSetFromList,
    -- * Processes
    Proc(..), UnCompiledProc,
    CSPOperator(..),
    ProcOperator(..), 
    ProcName(..),
    operator, components,
    prettyPrintAllRequiredProcesses,
) where

import CSPM.DataStructures.Names
import {-# SOURCE #-} CSPM.Evaluator.Values
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.Hashable
import Util.PrettyPrint

-- | Events, as represented in the LTS.
data Event = 
    -- | The internal special event tau.
    Tau 
    -- | The internal event tick, representing termination.
    | Tick 
    -- | Any event defined in a channel definition.
    | UserEvent Value
    deriving (Eq, Ord)

type EventSet = S.Seq Event

eventSetFromList :: [Event] -> EventSet
eventSetFromList = S.fromList

instance Hashable Event where
    hash Tau = 1
    hash Tick = 2
    hash (UserEvent vs) = combine 3 (hash vs)

-- | ProcNames uniquely identify processes.
data ProcName =
    ProcName {
        -- | The name of this process (recal Name s are unique).
        name :: Name,
        -- | The arguments applied to this process, in case it was a function
        -- call.
        arguments :: [[Value]],
        -- | The parent of this proc name. This is used in let expressions.
        parent :: Maybe ProcName
    }
    -- | A proccess that has no name, but needs disambgiuation. These are
    -- used in prefixing to avoid problems with things like:
    -- P = c?x -> (let Q = ... within Q) as the ... can depend on x.
    | AnnonymousProcName {
        arguments :: [[Value]],
        parent :: Maybe ProcName
    }
    deriving Eq

instance Hashable ProcName where
    hash (ProcName n vss p) = combine 1 (combine (hash n) (combine (hash vss) (hash p)))
    hash (AnnonymousProcName as ps) = combine 2 (combine (hash as) (hash ps))

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

data CSPOperator seq ev evs evm =
    PAlphaParallel (seq evs)
    | PException evs
    | PExternalChoice
    | PGenParallel evs
    | PHide evs
    | PInternalChoice
    | PInterrupt
    | PInterleave
    -- Map from event of left process, to event of right that it synchronises
    -- with. (Left being p1, Right being p2 ps ps).
    | PLinkParallel evm
    | POperator ProcOperator
    | PPrefix ev
    -- Map from Old -> New event
    | PRename evm
    | PSequentialComp
    | PSlidingChoice

-- | A compiled process. Note this is an infinite data structure (due to
-- PProcCall) as this makes compilation easy (we can easily chase
-- dependencies).
data Proc seq op pn ev evs evm = 
    PUnaryOp (op seq ev evs evm) (Proc seq op pn ev evs evm)
    | PBinaryOp (op seq ev evs evm) (Proc seq op pn ev evs evm) (Proc seq op pn ev evs evm)
    | POp (op seq ev evs evm) (seq (Proc seq op pn ev evs evm))
    -- | Labels the process this contains. This allows infinite loops to be
    -- spotted.
    | PProcCall pn (Proc seq op pn ev evs evm)

type UnCompiledProc = 
    Proc S.Seq CSPOperator ProcName Event (S.Seq Event) (S.Seq (Event, Event))

-- | Gives the operator of a process. If the process is a ProcCall an error is
-- thrown.
operator :: Proc seq op pn ev evs evm -> op seq ev evs evm
operator (PUnaryOp op _) = op
operator (PBinaryOp op _ _)=  op
operator (POp op _) = op

-- | Returns the components of a given process.
components :: Proc S.Seq op pn ev evs evm -> S.Seq (Proc S.Seq op pn ev evs evm)
components (PBinaryOp _ p1 p2) = p1 S.<| p2 S.<| S.empty
components (POp _ ps) = ps
components (PUnaryOp _ p1) = S.singleton p1

-- | Given a process, returns the initial process and all processes that it
-- calls.
splitProcIntoComponents :: (Eq pn, F.Foldable seq) => Proc seq op pn ev evs evm -> 
    (Proc seq op pn ev evs evm, [(pn, Proc seq op pn ev evs evm)])
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
prettyPrintAllRequiredProcesses ::
    (Eq pn, F.Foldable seq, PrettyPrintable pn,
        PrettyPrintable (Proc seq op pn ev evs evm)) => 
    Proc seq op pn ev evs evm -> Doc
prettyPrintAllRequiredProcesses p =
    let
        (pInit, namedPs) = splitProcIntoComponents p
        ppNamedProc (n,p) =
            hang (prettyPrint n <+> char '=') tabWidth (prettyPrint p)
    in 
        vcat (punctuate (char '\n') ((map ppNamedProc namedPs)++[prettyPrint pInit]))
