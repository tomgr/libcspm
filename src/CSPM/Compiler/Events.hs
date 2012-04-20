{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module CSPM.Compiler.Events (
    Event(..), newUserEvent,
    EventSet
) where

import Data.Hashable
import qualified Data.Text as T
import Util.PrettyPrint

-- | Events, as represented in the LTS.
data Event = 
    -- | The internal special event tau.
    Tau 
    -- | The internal event tick, representing termination.
    | Tick 
    -- | Any event defined in a channel definition.
    | UserEvent T.Text
    deriving (Eq, Ord)

newUserEvent :: String -> Event
newUserEvent = UserEvent . T.pack

instance Hashable Event where
    hash Tau = 1
    hash Tick = 2
    hash (UserEvent vs) = combine 3 (hash vs)
instance PrettyPrintable Event where
    prettyPrint Tau = char 'τ'
    prettyPrint Tick = char '✓'
    prettyPrint (UserEvent s) = text (T.unpack s)
instance Show Event where
    show ev = show (prettyPrint ev)

-- | An alias for ease
type EventSet = S.Set Event
type EventMap = M.Map Event Event

instance PrettyPrintable EventSet where
    prettyPrint s = braces (list (map prettyPrint (S.toList s)))
