{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    TypeSynonymInstances, UndecidableInstances #-}
module CSPM.Evaluator.ProcessValues (
    -- * Events
    Event(..),
    EventSet, eventSetFromList, EventMap,
    -- * Processes
    Proc(..), UnCompiledProc, UnCompiledProcOperator, UnCompiledOperator,
    CSPOperator(..),
    ProcOperator(..), 
    ProcName(..),
    operator, components,
    splitProcIntoComponents,
    trimProcess,
) where

import {-# SOURCE #-} CSPM.Evaluator.Values
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.Hashable
import Util.Exception
import Util.Prelude

-- | Events, as represented in the LTS.
data Event = 
    -- | The internal special event tau.
    Tau 
    -- | The internal event tick, representing termination.
    | Tick 
    -- | Any event defined in a channel definition.
    | UserEvent Value
    deriving (Eq, Ord)

type EventMap = S.Seq (Event, Event)
type EventSet = S.Seq Event

eventSetFromList :: [Event] -> EventSet
eventSetFromList = S.fromList

instance Hashable Event where
    hash Tau = 1
    hash Tick = 2
    hash (UserEvent vs) = combine 3 (hash vs)

-- | ProcNames uniquely identify processes.
newtype ProcName = ProcName (ScopeIdentifier) deriving (Eq, Hashable, Ord)

-- | An operator that can be applied to processes.
data ProcOperator seq ev evs =
    Chase Bool
    | DelayBisim
    | Determinise
    | Diamond
    | Explicate Bool
    | Normalize Bool
    | ModelCompress
    | Prioritise Bool (seq evs)
    | StrongBisim
    | TauLoopFactor
    | TraceWatchdog evs ev
    | WeakBisim
    deriving (Eq, Ord)

instance (Hashable ev, Hashable evs, Hashable (seq evs)) =>
        Hashable (ProcOperator seq ev evs) where
    hash (Chase True) = 1
    hash (Chase False) = 2
    hash Diamond = 3
    hash (Explicate True) = 4
    hash (Explicate False) = 5
    hash (Normalize True)= 6
    hash (Normalize False)= 7
    hash (Prioritise False evs) = combine 8 (hash evs)
    hash (Prioritise True evs) = combine 9 (hash evs)
    hash ModelCompress = 10
    hash StrongBisim = 11
    hash TauLoopFactor = 12
    hash WeakBisim = 13
    hash Determinise = 14
    hash DelayBisim = 15
    hash (TraceWatchdog evs ev) = combine 16 (combine (hash evs) (hash ev))

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
    | POperator (ProcOperator seq ev evs)
    | PPrefix ev
    -- Map from Old -> New event
    | PRename evm
    | PSequentialComp
    | PSlidingChoice
    | PSynchronisingExternalChoice evs
    | PSynchronisingInterrupt evs
    deriving (Eq, Ord)

instance (Hashable ev, Hashable evm, Hashable evs, Hashable (seq evs)) =>
        Hashable (CSPOperator seq ev evs evm) where
    hash (PAlphaParallel s) = combine 1 (hash s)
    hash (PException s) = combine 2 (hash s)
    hash PExternalChoice = 3
    hash (PGenParallel evs) = combine 4 (hash evs)
    hash (PHide evs) = combine 5 (hash evs)
    hash PInternalChoice = 6
    hash PInterrupt = 7
    hash PInterleave = 8
    hash (PLinkParallel s) = combine 9 (hash s)
    hash (POperator op) = combine 11 (hash op)
    hash (PPrefix ev) = combine 12 (hash ev)
    hash (PRename evm) = combine 13 (hash evm)
    hash PSequentialComp = 14
    hash PSlidingChoice = 15
    hash (PSynchronisingExternalChoice evs) = combine 16 (hash evs)
    hash (PSynchronisingInterrupt evs) = combine 17 (hash evs)

errorThunk = panic "Trimmed process evaluated"

trimProcess :: UnCompiledProc -> UnCompiledProc
trimProcess (PUnaryOp op p1) =
    PUnaryOp (trimOperator op) (trimProcess p1)
trimProcess (PBinaryOp op p1 p2) =
    PBinaryOp (trimOperator op) (trimProcess p1) (trimProcess p2)
trimProcess (POp op ps) =
    POp (trimOperator op) (fmap trimProcess ps)
trimProcess (PProcCall pn _) = PProcCall pn errorThunk

trimEvent :: Event -> Event
trimEvent Tau = Tau
trimEvent Tick = Tick
trimEvent (UserEvent v) = UserEvent (trimValueForProcessName v)

trimOperator :: UnCompiledOperator -> UnCompiledOperator
trimOperator (PAlphaParallel s) = PAlphaParallel (fmap (fmap trimEvent) s)
trimOperator (PException s) = PException (fmap trimEvent s)
trimOperator PExternalChoice = PExternalChoice
trimOperator (PGenParallel evs) = PGenParallel (fmap trimEvent evs)
trimOperator (PHide evs) = PHide (fmap trimEvent evs)
trimOperator PInternalChoice = PInternalChoice
trimOperator PInterrupt = PInterrupt
trimOperator PInterleave = PInterleave
trimOperator (PLinkParallel evm) =
    PLinkParallel (fmap (\(ev,ev') -> (trimEvent ev, trimEvent ev')) evm)
trimOperator (POperator op) = POperator op
trimOperator (PPrefix ev) = PPrefix (trimEvent ev)
trimOperator (PRename evm) = 
    PRename (fmap (\(ev,ev') -> (trimEvent ev, trimEvent ev')) evm)
trimOperator PSequentialComp = PSequentialComp
trimOperator PSlidingChoice = PSlidingChoice
trimOperator (PSynchronisingExternalChoice evs) =
    PSynchronisingExternalChoice (fmap trimEvent evs)
trimOperator (PSynchronisingInterrupt evs) =
    PSynchronisingInterrupt (fmap trimEvent evs)

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

instance (Eq pn, Eq (seq (Proc seq op pn ev evs evm)), Eq (op seq ev evs evm)) =>
        Eq (Proc seq op pn ev evs evm) where
    (PProcCall pn1 _) == (PProcCall pn2 _) = pn1 == pn2
    (PUnaryOp op1 p1) == (PUnaryOp op2 p2) = op1 == op2 && p1 == p2
    (PBinaryOp op1 p1 p2) == (PBinaryOp op2 r1 r2) =
        op1 == op2 && p1 == r1 && p2 == r2
    (POp op1 ps1) == (POp op2 ps2) = op1 == op2 && ps1 == ps2
    _ == _ = False

instance (Hashable pn, Hashable (seq (Proc seq op pn ev evs evm)), Hashable (op seq ev evs evm)) =>
        Hashable (Proc seq op pn ev evs evm) where
    hash (PProcCall pn1 _) = combine 1 (hash pn1)
    hash (PUnaryOp op1 p1) = combine 2 (combine (hash op1) (hash p1))
    hash (PBinaryOp op1 p1 p2) = combine 3 (combine (hash op1) (combine (hash p1) (hash p2)))
    hash (POp op ps) = combine 4 (combine (hash op) (hash ps))

instance (Ord pn, Ord (seq (Proc seq op pn ev evs evm)), Ord (op seq ev evs evm)) =>
        Ord (Proc seq op pn ev evs evm) where
    compare (PProcCall pn1 _) (PProcCall pn2 _) = compare pn1 pn2
    compare (PProcCall _ _) _ = LT
    compare _ (PProcCall _ _) = GT
    compare (PUnaryOp op1 p1) (PUnaryOp op2 p2) =
        compare op1 op2 `thenCmp` compare p1 p2
    compare (PUnaryOp _ _) _ = LT
    compare _ (PUnaryOp _ _) = GT
    compare (PBinaryOp op1 p1 p2) (PBinaryOp op2 r1 r2) =
        compare op1 op2 `thenCmp` compare p1 r1 `thenCmp` compare p2 r2
    compare (PBinaryOp _ _ _) _ = LT
    compare _ (PBinaryOp _ _ _) = GT
    compare (POp op1 ps1) (POp op2 ps2) =
        compare op1 op2 `thenCmp` compare ps1 ps2

type UnCompiledProc = 
    Proc S.Seq CSPOperator ProcName Event (S.Seq Event) (S.Seq (Event, Event))
type UnCompiledOperator = 
    CSPOperator S.Seq Event (S.Seq Event) (S.Seq (Event, Event))
type UnCompiledProcOperator =
    ProcOperator S.Seq Event (S.Seq Event)

instance Hashable a => Hashable (S.Seq a) where
    hash a = foldr combine 0 (F.toList (fmap hash a))

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
