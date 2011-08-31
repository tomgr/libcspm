{-# LANGUAGE TypeSynonymInstances #-}
module CSPM.Compiler.Events (
    Event(..),
    EventSet
) where

import qualified CSPM.Compiler.Map as M
import qualified CSPM.Compiler.Set as S
import Util.PrettyPrint

-- | Events, as represented in the LTS.
data Event = 
    -- | The internal special event tau.
    Tau 
    -- | The internal event tick, representing termination.
    | Tick 
    -- | Any event defined in a channel definition.
    | UserEvent String
    deriving (Eq, Ord)

instance PrettyPrintable Event where
    prettyPrint Tau = char 'τ'
    prettyPrint Tick = char '✓'
    prettyPrint (UserEvent s) = text s
instance Show Event where
    show ev = show (prettyPrint ev)

-- | An alias for ease
type EventSet = S.Set Event
type EventMap = M.Map Event Event

instance PrettyPrintable EventSet where
    prettyPrint s = braces (list (map prettyPrint (S.toList s)))
