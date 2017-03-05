{-# LANGUAGE FlexibleContexts #-}
-- | A wrapper around the types and functions from "Data.Graph" to make programming with them less painful. Also
-- implements some extra useful goodies such as 'successors' and 'sccGraph', and improves the documentation of
-- the behaviour of some functions.
--
-- As it wraps "Data.Graph", this module only supports directed graphs with unlabelled edges.
--
-- Incorporates code from the 'containers' package which is (c) The University of Glasgow 2002 and based
-- on code described in:
--
--   /Lazy Depth-First Search and Linear Graph Algorithms in Haskell/,
--   by David King and John Launchbury
module Data.Graph.Wrapper (
    Edge, Graph,
    
    vertex,
    
    fromListSimple, fromList, fromListLenient, fromListBy, fromVerticesEdges,
    toList,
    
    vertices, edges, successors,
    
    outdegree, indegree,
    
    transpose,
    
    reachableVertices, hasPath,
    
    topologicalSort, depthNumbering,
    
    SCC(..), stronglyConnectedComponents, sccGraph,
    
    traverseWithKey
  ) where

import Data.Graph.Wrapper.Internal

import Control.Arrow (second)
import Control.Monad
import Control.Monad.ST

import Data.Array
import Data.Array.ST
import qualified Data.Graph as G
import qualified Data.IntSet as IS
import Data.List (sortBy, mapAccumL)
import Data.Maybe (fromMaybe, fromJust, mapMaybe)
import qualified Data.Map as M
import Data.Ord
import qualified Data.Set as S

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c


-- amapWithKey :: Ix i => (i -> v -> v') -> Array i v -> Array i v'
-- -- More efficient, but not portable (uses GHC.Arr exports):
-- --amapWithKey f arr = unsafeArray' (bounds arr) (numElements arr) [(i, f i (unsafeAt arr i)) | i <- [0 .. n - 1]]
-- amapWithKey f arr = array (bounds arr) [(i, f i v) | (i, v) <- assocs arr]

amapWithKeyM :: (Monad m, Ix i) => (i -> v -> m v') -> Array i v -> m (Array i v')
amapWithKeyM f arr = liftM (array (bounds arr)) $ mapM (\(i, v) -> liftM (\v' -> (i, v')) $ f i v) (assocs arr)


-- | Construct a 'Graph' where the vertex data double up as the indices.
--
-- Unlike 'Data.Graph.graphFromEdges', vertex data that is listed as edges that are not actually themselves
-- present in the input list are reported as an error.
fromListSimple :: Ord v => [(v, [v])] -> Graph v v
fromListSimple = fromListBy id

-- | Construct a 'Graph' that contains the given vertex data, linked up according to the supplied key extraction
-- function and edge list.
--
-- Unlike 'Data.Graph.graphFromEdges', indexes in the edge list that do not correspond to the index of some item in the
-- input list are reported as an error.
fromListBy :: Ord i => (v -> i) -> [(v, [i])] -> Graph i v
fromListBy f vertices = fromList [(f v, v, is) | (v, is) <- vertices]

-- | Construct a 'Graph' directly from a list of vertices (and vertex data).
--
-- If either end of an 'Edge' does not correspond to a supplied vertex, an error will be raised.
fromVerticesEdges :: Ord i => [(i, v)] -> [Edge i] -> Graph i v
fromVerticesEdges vertices edges | M.null final_edges_map = fromList done_vertices
                                 | otherwise              = error "fromVerticesEdges: some edges originated from non-existant vertices"
  where
    (final_edges_map, done_vertices) = mapAccumL accum (M.fromListWith (++) (map (second return) edges)) vertices
    accum edges_map (i, v) = case M.updateLookupWithKey (\_ _ -> Nothing) i edges_map of (mb_is, edges_map) -> (edges_map, (i, v, fromMaybe [] mb_is))

-- | Construct a 'Graph' that contains the given vertex data, linked up according to the supplied index and edge list.
--
-- Unlike 'Data.Graph.graphFromEdges', indexes in the edge list that do not correspond to the index of some item in the
-- input list are reported as an error.
fromList :: Ord i => [(i, v, [i])] -> Graph i v
fromList = fromList' False

-- | Construct a 'Graph' that contains the given vertex data, linked up according to the supplied index and edge list.
--
-- Like 'Data.Graph.graphFromEdges', indexes in the edge list that do not correspond to the index of some item in the
-- input list are silently ignored.
fromListLenient :: Ord i => [(i, v, [i])] -> Graph i v
fromListLenient = fromList' True

{-# INLINE fromList' #-}
fromList' :: Ord i => Bool -> [(i, v, [i])] -> Graph i v
fromList' lenient vertices = G graph key_map vertex_map
  where
    max_v           = length vertices - 1
    bounds0         = (0, max_v) :: (G.Vertex, G.Vertex)
    sorted_vertices = sortBy (comparing fst3) vertices
    
    index_vertex = if lenient then mapMaybe (indexGVertex'_maybe key_map) else map (indexGVertex' key_map)
    
    graph       = array bounds0 $ [0..] `zip` map (index_vertex . thd3) sorted_vertices
    key_map     = array bounds0 $ [0..] `zip` map fst3                  sorted_vertices
    vertex_map  = array bounds0 $ [0..] `zip` map snd3                  sorted_vertices


-- | Morally, the inverse of 'fromList'. The order of the elements in the output list is unspecified, as is the order of the edges
-- in each node's adjacency list. For this reason, @toList . fromList@ is not necessarily the identity function.
toList :: Ord i => Graph i v -> [(i, v, [i])]
toList g = [(indexGVertexArray g ! m, gVertexVertexArray g ! m, map (indexGVertexArray g !) ns) | (m, ns) <- assocs (graph g)]

-- | Find the vertices we can reach from a vertex with the given indentity
successors :: Ord i => Graph i v -> i -> [i]
successors g i = map (gVertexIndex g) (graph g ! indexGVertex g i)

-- | Number of edges going out of the vertex.
--
-- It is worth sharing a partial application of 'outdegree' to the 'Graph' argument if you intend to query
-- for the outdegrees of a number of vertices.
outdegree :: Ord i => Graph i v -> i -> Int
outdegree g = \i -> outdegrees ! indexGVertex g i
  where outdegrees = G.outdegree (graph g)

-- | Number of edges going in to the vertex.
--
-- It is worth sharing a partial application of 'indegree' to the 'Graph' argument if you intend to query
-- for the indegrees of a number of vertices.
indegree :: Ord i => Graph i v -> i -> Int
indegree g = \i -> indegrees ! indexGVertex g i
  where indegrees = G.indegree (graph g)

-- | The graph formed by flipping all the edges, so edges from i to j now go from j to i
transpose :: Graph i v -> Graph i v
transpose g = g { graph = G.transposeG (graph g) }

-- | Topological sort of of the graph (<http://en.wikipedia.org/wiki/Topological_sort>). If the graph is acyclic,
-- vertices will only appear in the list once all of those vertices with arrows to them have already appeared.
--
-- Vertex /i/ precedes /j/ in the output whenever /j/ is reachable from /i/ but not vice versa.
topologicalSort :: Graph i v -> [i]
topologicalSort g = map (gVertexIndex g) $ G.topSort (graph g)

-- | List all of the vertices reachable from the given starting point
reachableVertices :: Ord i => Graph i v -> i -> [i]
reachableVertices g = map (gVertexIndex g) . G.reachable (graph g) . indexGVertex g

-- | Is the second vertex reachable by following edges from the first vertex?
--
-- It is worth sharing a partial application of 'hasPath' to the first vertex if you are testing for several
-- vertices being reachable from it.
hasPath :: Ord i => Graph i v -> i -> i -> Bool
hasPath g i1 = (`elem` reachableVertices g i1)

-- | Number the vertices in the graph by how far away they are from the given roots. The roots themselves have depth 0,
-- and every subsequent link we traverse adds 1 to the depth. If a vertex is not reachable it will have a depth of 'Nothing'.
depthNumbering :: Ord i => Graph i v -> [i] -> Graph i (v, Maybe Int)
depthNumbering g is = runST $ do
    -- This array records the minimum known depth for the node at the moment
    depth_array <- newArray (bounds (graph g)) Nothing :: ST s (STArray s G.Vertex (Maybe Int))
    let -- Lets us adjust the known depth given a new observation
        atDepth gv depth = do
            mb_old_depth <- readArray depth_array gv
            let depth' = maybe depth (`min` depth) mb_old_depth
            depth' `seq` writeArray depth_array gv (Just depth')

    -- Do an depth-first search on the graph (checking for cycles to prevent non-termination),
    -- recording the depth at which any node was seen in that array.
    let gos seen depth gvs = mapM_ (go seen depth) gvs

        go seen depth gv 
          | depth `seq` False = error "depthNumbering: unreachable"
          | gv `IS.member` seen = return ()
          | otherwise = do
            gv `atDepth` depth
            gos (IS.insert gv seen) (depth + 1) (graph g ! gv)
    gos IS.empty 0 (map (indexGVertex g) is)
    
    -- let go _    _     []  = return ()
    --     go seen depth gvs = do
    --         let go_one (seen, next_gvs) gv
    --               | gv `IS.member` seen = return (seen, next_gvs)
    --               | otherwise = do gv `atDepth` depth
    --                                return (IS.insert gv seen, next_gvs ++ (graph g ! gv))
    --         (seen, next_gvs) <- foldM go_one (seen, []) gvs
    --         go seen (depth + 1) next_gvs
    -- 
    -- go IS.empty 0 (map (indexGVertex g) is)
    
    gvva <- amapWithKeyM (\gv v -> liftM (\mb_depth -> (v, mb_depth)) $ readArray depth_array gv) (gVertexVertexArray g)
    return $ g { gVertexVertexArray = gvva }


data SCC i = AcyclicSCC i
           | CyclicSCC [i]
           deriving (Show)

instance Functor SCC where
    fmap f (AcyclicSCC v) = AcyclicSCC (f v)
    fmap f (CyclicSCC vs) = CyclicSCC (map f vs)

instance Foldable.Foldable SCC where
    foldMap f (AcyclicSCC v) = f v
    foldMap f (CyclicSCC vs) = Foldable.foldMap f vs

instance Traversable.Traversable SCC where
    traverse f (AcyclicSCC v) = fmap AcyclicSCC (f v)
    traverse f (CyclicSCC vs) = fmap CyclicSCC (Traversable.traverse f vs)

-- | Strongly connected components (<http://en.wikipedia.org/wiki/Strongly_connected_component>).
--
-- The SCCs are listed in a *reverse topological order*. That is to say, any edges *to* a node in the SCC
-- originate either *from*:
--
--   1) Within the SCC itself (in the case of a 'CyclicSCC' only)
--   2) Or from a node in a SCC later on in the list
--
-- Vertex /i/ strictly precedes /j/ in the output whenever /i/ is reachable from /j/ but not vice versa.
-- Vertex /i/ occurs in the same SCC as /j/ whenever both /i/ is reachable from /j/ and /j/ is reachable from /i/.
stronglyConnectedComponents :: Graph i v -> [SCC i]
stronglyConnectedComponents g = map decode forest
  where
    forest = G.scc (graph g)
    decode (G.Node v []) | mentions_itself v = CyclicSCC [gVertexIndex g v]
                         | otherwise         = AcyclicSCC (gVertexIndex g v)
    decode other = CyclicSCC (dec other [])
      where dec (G.Node v ts) vs = gVertexIndex g v : foldr dec vs ts
    
    mentions_itself v = v `elem` (graph g ! v)

-- | The graph formed by the strongly connected components of the input graph. Each node in the resulting
-- graph is indexed by the set of vertex indices from the input graph that it contains.
sccGraph :: Ord i => Graph i v -> Graph (S.Set i) (M.Map i v)
sccGraph g = fromList nodes'
  where
    -- As we consume the SCCs, we accumulate a Map i (S.Set i) that tells us which SCC any given index belongs to.
    -- When we do a lookup, it is sufficient to look in the map accumulated so far because nodes that are successors
    -- of a SCC must occur to the *left* of it in the list.
    (_final_i2scc_i, nodes') = mapAccumL go M.empty (stronglyConnectedComponents g)
    
    --go :: M.Map i (S.Set i) -> SCC i -> (M.Map i (S.Set i), (S.Set i, M.Map i v, [S.Set i]))
    go i2scc_i scc = (i2scc_i', (scc_i,
                                 Foldable.foldMap (\i -> M.singleton i (vertex g i)) scc,
                                 Foldable.foldMap (\i -> map (fromJust . (`M.lookup` i2scc_i')) (successors g i)) scc))
      where
        -- The mechanism by which we index the new graph -- the set of indexes of its components
        scc_i = Foldable.foldMap S.singleton scc
        i2scc_i' = i2scc_i `M.union` Foldable.foldMap (\i -> M.singleton i scc_i) scc