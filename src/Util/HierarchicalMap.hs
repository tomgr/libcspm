{-# LANGUAGE DeriveDataTypeable #-}
module Util.HierarchicalMap where

import qualified Data.Map as M
import Data.Typeable
import Prelude hiding (lookup)
import Util.Exception

data HierarchicalMap a b = 
    HierarchicalMap [M.Map a b]
    deriving Show

data HierarchicalMapException a = 
    ValueNotFoundException a
    deriving (Show, Typeable)

instance (Ord a, Show a, Typeable a) => Exception (HierarchicalMapException a)

-- | Creates/updates a key in the top level map.
update :: Ord a => HierarchicalMap a b -> a -> b -> HierarchicalMap a b
update (HierarchicalMap (m:ms)) k v = 
    HierarchicalMap ((M.insert k v m):ms)

updateMulti :: Ord a => HierarchicalMap a b -> [(a, b)] -> HierarchicalMap a b
updateMulti m bs = 
    foldl (\ m (a,b) -> update m a b) m bs

-- | Looks up a key in any map, starting from the top
lookup :: (Show k, Typeable k, Ord k) => HierarchicalMap k a -> k -> a
lookup hm k = case maybeLookup hm k of
    Just v -> v
    Nothing -> throwException (ValueNotFoundException k)

maybeLookup :: (Show k, Typeable k, Ord k) => HierarchicalMap k a -> k -> Maybe a
maybeLookup (HierarchicalMap []) k = Nothing
maybeLookup (HierarchicalMap (m:ms)) k = 
    case M.lookup k m of
        Just v -> Just v
        Nothing -> maybeLookup (HierarchicalMap ms) k

maybeLookupInTopLayer :: (Show k, Typeable k, Ord k) => HierarchicalMap k a -> k -> Maybe a
maybeLookupInTopLayer (HierarchicalMap []) k = Nothing
maybeLookupInTopLayer (HierarchicalMap (m:ms)) k = M.lookup k m

popLayer :: Ord a => HierarchicalMap a b -> HierarchicalMap a b
popLayer (HierarchicalMap (m:ms)) = HierarchicalMap ms

flatten :: Ord a => HierarchicalMap a b -> [(a, b)]
flatten (HierarchicalMap ms) = h ms []
    where
        h [] _ = []
        h (m:ms) ks = l++(h ms ks')
            where
                l = [(k, v) | (k, v) <- M.toList m, not (k `elem` ks)]
                ks' = map fst l++ks

newLayer :: Ord a => HierarchicalMap a b -> HierarchicalMap a b
newLayer (HierarchicalMap ms) = HierarchicalMap (M.empty : ms)

newLayerAndBind :: Ord a => HierarchicalMap a b -> [(a, b)] -> HierarchicalMap a b
newLayerAndBind (HierarchicalMap ms) bs = 
    HierarchicalMap (M.fromList bs : ms)

newRecursiveLayerAndBind :: Ord a => 
    HierarchicalMap a b -> [HierarchicalMap a b -> (a, b)] -> HierarchicalMap a b
newRecursiveLayerAndBind map bs = newMap
    where
        bs' = [f newMap | f <- bs]
        newMap = newLayerAndBind map bs'

-- | Creates a new map
new :: Ord a => HierarchicalMap a b
new = HierarchicalMap [M.empty]
