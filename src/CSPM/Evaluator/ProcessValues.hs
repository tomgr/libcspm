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
import qualified Data.Set as St
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
    hashWithSalt s Tau = s `hashWithSalt` (1 :: Int)
    hashWithSalt s Tick = s `hashWithSalt` (2 :: Int)
    hashWithSalt s (UserEvent vs) = s `hashWithSalt` (3 :: Int) `hashWithSalt` vs

-- | ProcNames uniquely identify processes.
newtype ProcName = ProcName (ScopeIdentifier) deriving (Eq, Hashable, Ord)

-- | An operator that can be applied to processes.
data ProcOperator seq ev evs =
    Chase Bool
    | DelayBisim
    | Determinise
    | Diamond
    | Explicate Bool
    | FailureWatchdog evs ev
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
    hashWithSalt s (Chase True) = s `hashWithSalt` (1 :: Int)
    hashWithSalt s (Chase False) = s `hashWithSalt` (2 :: Int)
    hashWithSalt s Diamond = s `hashWithSalt` (3 :: Int)
    hashWithSalt s (Explicate True) = s `hashWithSalt` (4 :: Int)
    hashWithSalt s (Explicate False) = s `hashWithSalt` (5 :: Int)
    hashWithSalt s (Normalize True)= s `hashWithSalt` (6 :: Int)
    hashWithSalt s (Normalize False)= s `hashWithSalt` (7 :: Int)
    hashWithSalt s (Prioritise False evs) =
        s `hashWithSalt` (8 :: Int) `hashWithSalt` evs
    hashWithSalt s (Prioritise True evs) = s `hashWithSalt` (9 :: Int) `hashWithSalt` evs
    hashWithSalt s ModelCompress = 10
    hashWithSalt s StrongBisim = 11
    hashWithSalt s TauLoopFactor = 12
    hashWithSalt s WeakBisim = 13
    hashWithSalt s Determinise = 14
    hashWithSalt s DelayBisim = 15
    hashWithSalt s (TraceWatchdog evs ev) =
        s `hashWithSalt` (16 :: Int) `hashWithSalt` evs `hashWithSalt` ev
    hashWithSalt s (FailureWatchdog evs ev) =
        s `hashWithSalt` (18 :: Int) `hashWithSalt` evs `hashWithSalt` ev

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
    hashWithSalt s (PAlphaParallel a) = s `hashWithSalt` (1 :: Int) `hashWithSalt` a
    hashWithSalt s (PException a) = s `hashWithSalt` (2 :: Int) `hashWithSalt` a
    hashWithSalt s PExternalChoice = 3
    hashWithSalt s (PGenParallel evs) = s `hashWithSalt` (4 :: Int) `hashWithSalt` evs
    hashWithSalt s (PHide evs) = s `hashWithSalt` (5 :: Int) `hashWithSalt` evs
    hashWithSalt s PInternalChoice = 6
    hashWithSalt s PInterrupt = 7
    hashWithSalt s PInterleave = 8
    hashWithSalt s (PLinkParallel evs) = s `hashWithSalt` (9 :: Int) `hashWithSalt` evs
    hashWithSalt s (POperator op) = s `hashWithSalt` (11 :: Int) `hashWithSalt` op
    hashWithSalt s (PPrefix ev) = s `hashWithSalt` (12 :: Int) `hashWithSalt` ev
    hashWithSalt s (PRename evm) = s `hashWithSalt` (13 :: Int) `hashWithSalt` evm
    hashWithSalt s PSequentialComp = 14
    hashWithSalt s PSlidingChoice = 15
    hashWithSalt s (PSynchronisingExternalChoice evs) =
        s `hashWithSalt` (16 :: Int) `hashWithSalt` evs
    hashWithSalt s (PSynchronisingInterrupt evs) =
        s `hashWithSalt` (17 :: Int) `hashWithSalt` evs

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
    hashWithSalt s (PProcCall pn1 _) = s `hashWithSalt` (1 :: Int) `hashWithSalt` pn1
    hashWithSalt s (PUnaryOp op1 p1) = s `hashWithSalt` (2 :: Int) `hashWithSalt` op1 `hashWithSalt` p1
    hashWithSalt s (PBinaryOp op1 p1 p2) =
        s `hashWithSalt` (3 :: Int) `hashWithSalt` op1 `hashWithSalt` p1 `hashWithSalt` p2
    hashWithSalt s (POp op ps) =
        s `hashWithSalt` (4 :: Int) `hashWithSalt` op `hashWithSalt` ps

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
    hashWithSalt s a = foldr hashWithSalt s (F.toList (fmap hash a))

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
splitProcIntoComponents :: (Eq pn, Ord pn, F.Foldable seq) =>
    Proc seq op pn ev evs evm -> 
    (Proc seq op pn ev evs evm, [(pn, Proc seq op pn ev evs evm)])
splitProcIntoComponents p =
    let
        explored s n = St.member n s

        exploreAll s pns [] = (s, pns)
        exploreAll s pns (p:ps) = exploreAll s' pns' ps
            where (s', pns') = explore s pns p

        explore s pns (PUnaryOp _ p) = explore s pns p
        explore s pns (PBinaryOp _ p1 p2) = exploreAll s pns [p1,p2]
        explore s pns (POp _ ps) = exploreAll s pns (F.toList ps)
        explore s pns (PProcCall n p) =
            if explored s n then (s, pns)
            else explore (St.insert n s) ((n, p):pns) p
    in (p, snd $ explore St.empty [] p)
