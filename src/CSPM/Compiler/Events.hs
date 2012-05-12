{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, UndecidableInstances #-}
module CSPM.Compiler.Events (
    Event(..),
    EventSet, fromList,
) where

import Data.Foldable as F
import Data.Hashable
import qualified Data.Sequence as Sq

import CSPM.Compiler.Processes
import {-# SOURCE #-} CSPM.Evaluator.Values
import Util.PrettyPrint
import qualified Util.TextPrettyPrint as T

-- | Events, as represented in the LTS.
data Event ops = 
    -- | The internal special event tau.
    Tau 
    -- | The internal event tick, representing termination.
    | Tick 
    -- | Any event defined in a channel definition.
    | UserEvent (Value ops)
    deriving (Eq, Ord)

type EventSet ops = Sq.Seq (Event ops)

fromList :: [Event ops] -> EventSet ops
fromList = Sq.fromList

instance PrettyPrintable (UProc ops) => T.FastPrettyPrintable (Event ops) where
    toBuilder Tau = T.char 'τ'
    toBuilder Tick = T.char '✓'
    toBuilder (UserEvent v) = T.toBuilder v
instance Hashable (Event ops) where
    hash Tau = 1
    hash Tick = 2
    hash (UserEvent vs) = combine 3 (hash vs)
instance PrettyPrintable (UProc ops) => PrettyPrintable (Event ops) where
    prettyPrint Tau = char 'τ'
    prettyPrint Tick = char '✓'
    prettyPrint (UserEvent v) = prettyPrint v
instance PrettyPrintable (UProc ops) => Show (Event ops) where
    show ev = show (prettyPrint ev)

instance PrettyPrintable (UProc ops) => PrettyPrintable (Sq.Seq (Event ops)) where
    prettyPrint s = braces (list (map prettyPrint (F.toList s)))
