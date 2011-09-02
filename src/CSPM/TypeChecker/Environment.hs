module CSPM.TypeChecker.Environment (
    module Util.HierarchicalMap,
    Environment, 
    mkSymbolInformation, SymbolInformation(..)
)
where

import CSPM.DataStructures.Names
import CSPM.DataStructures.Types
import Util.HierarchicalMap

-- | Make symbol information for the type assuming that the symbol
-- is not deprecated and its type is not unsafe.
mkSymbolInformation :: TypeScheme -> SymbolInformation
mkSymbolInformation t = SymbolInformation { 
        typeScheme = t, 
        isDeprecated = False, 
        isTypeUnsafe = False 
    }

-- | Used to represent information about a symbol
data SymbolInformation = SymbolInformation {
        -- | The type of the symbol
        typeScheme :: TypeScheme,
        -- | Is this symbol deprecated
        isDeprecated :: Bool,
        -- | Is this symbols' type too general (if so
        -- use of it will emit a soundness warning)
        isTypeUnsafe :: Bool
    }
    deriving (Eq, Show)

type Environment = HierarchicalMap Name SymbolInformation
