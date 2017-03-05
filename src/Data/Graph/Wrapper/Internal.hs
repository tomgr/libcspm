-- | Exposes things that are considered to be too unstable for inclusion in the exports of "Data.Graph.Wrapper".
--
-- Use of this module should be avoided as it will change frequently and changes to this module alone will not necessarily
-- follow the Package Versioning Policy.
{-# OPTIONS_HADDOCK not-home #-}
module Data.Graph.Wrapper.Internal where

import Data.Array
import Data.Maybe (fromMaybe)
import qualified Data.Graph as G

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable


-- This module currently contains just enough definitions that lets us put the definition of Graph
-- here and not have any orphan instances


-- | An edge from the first vertex to the second
type Edge i = (i, i)


-- | A directed graph
data Graph i v = G {
    graph :: G.Graph,
    indexGVertexArray :: Array G.Vertex i,
    gVertexVertexArray :: Array G.Vertex v
  }

instance (Ord i, Show i, Show v) => Show (Graph i v) where
    show g = "fromVerticesEdges " ++ show ([(i, vertex g i) | i <- vertices g]) ++ " " ++ show (edges g)

instance Functor (Graph i) where
    fmap f g = g { gVertexVertexArray = fmap f (gVertexVertexArray g) }

instance Foldable.Foldable (Graph i) where
    foldMap f g = Foldable.foldMap f (gVertexVertexArray g)

instance Traversable.Traversable (Graph i) where
    traverse f g = fmap (\gvva -> g { gVertexVertexArray = gvva }) (Traversable.traverse f (gVertexVertexArray g))


traverseWithKey :: Applicative t => (i -> a -> t b) -> Graph i a -> t (Graph i b)
traverseWithKey f g = fmap (\gvva -> g { gVertexVertexArray = gvva }) (traverseWithIndex (\gv -> f (gVertexIndex g gv)) (gVertexVertexArray g))
  where
    traverseWithIndex :: Applicative t => (G.Vertex -> a -> t b) -> Array G.Vertex a -> t (Array G.Vertex b)
    traverseWithIndex f a = fmap (array (bounds a)) $ flip Traversable.traverse (assocs a) $ \(k, v) -> fmap ((,) k) $ f k v


{-# RULES "indexGVertex/gVertexIndex" forall g i. gVertexIndex g (indexGVertex g i) = i #-}
{-# RULES "gVertexIndex/indexGVertex" forall g v. indexGVertex g (gVertexIndex g v) = v #-}

{-# NOINLINE [0] indexGVertex #-}
indexGVertex :: Ord i => Graph i v -> i -> G.Vertex
indexGVertex g i = indexGVertex' (indexGVertexArray g) i

{-# NOINLINE [0] gVertexIndex #-}
gVertexIndex :: Graph i v -> G.Vertex -> i
gVertexIndex g gv = indexGVertexArray g ! gv

gVertexVertex :: Graph i v -> G.Vertex -> v
gVertexVertex g gv = gVertexVertexArray g ! gv

-- | Retrieve data associated with the vertex
vertex :: Ord i => Graph i v -> i -> v
vertex g = gVertexVertex g . indexGVertex g


indexGVertex' :: Ord i => Array G.Vertex i -> i -> G.Vertex
indexGVertex' key_map k = fromMaybe (error "Data.Graph.Wrapper.fromList: one of the edges of a vertex pointed to a vertex that was not supplied in the input") (indexGVertex'_maybe key_map k)

indexGVertex'_maybe :: Ord i => Array G.Vertex i -> i -> Maybe G.Vertex
indexGVertex'_maybe key_map k = go 0 (snd (bounds key_map))
  where
    go a b | a > b = Nothing
           | otherwise = case compare k (key_map ! mid) of
                           LT -> go a (mid - 1)
                           EQ -> Just mid
                           GT -> go (mid + 1) b
     where mid = (a + b) `div` 2


-- | Exhaustive list of vertices in the graph
vertices :: Graph i v -> [i]
vertices g = map (gVertexIndex g) $ G.vertices (graph g)

-- | Exhaustive list of edges in the graph
edges :: Graph i v -> [Edge i]
edges g = map (\(x, y) -> (gVertexIndex g x, gVertexIndex g y)) $ G.edges (graph g)
