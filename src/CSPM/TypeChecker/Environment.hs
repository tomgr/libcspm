module CSPM.TypeChecker.Environment (
    Environment,
    new, bind, maybeLookup, update, delete, toList,
    mkSymbolInformation, SymbolInformation(..)
)
where

import qualified Data.Map as M

import CSPM.Syntax.Names
import CSPM.Syntax.Types

-- | Make symbol information for the type assuming that the symbol
-- is not deprecated and its type is not unsafe.
mkSymbolInformation :: TypeScheme -> SymbolInformation
mkSymbolInformation t = SymbolInformation { 
        typeScheme = t, 
        isDeprecated = False,
        deprecationReplacement = Nothing,
        isTypeUnsafe = False 
    }

-- | Used to represent information about a symbol
data SymbolInformation = SymbolInformation {
        -- | The type of the symbol
        typeScheme :: TypeScheme,
        -- | Is this symbol deprecated
        isDeprecated :: Bool,
        deprecationReplacement :: Maybe Name,
        -- | Is this symbols' type too general (if so
        -- use of it will emit a soundness warning)
        isTypeUnsafe :: Bool
    }
    deriving (Eq, Show)

type Environment = M.Map Name SymbolInformation

new :: Environment
new = M.empty

bind :: Environment -> [(Name, SymbolInformation)] -> Environment
bind env bs = foldr (\ (n,t) env -> M.insert n t env) env bs

maybeLookup :: Environment -> Name -> Maybe SymbolInformation
maybeLookup env n = M.lookup n env

update :: Environment -> Name -> SymbolInformation -> Environment
update env n s = M.insert n s env

delete :: Environment -> Name -> Environment
delete env n = M.delete n env

toList :: Environment -> [(Name, SymbolInformation)]
toList = M.toList
