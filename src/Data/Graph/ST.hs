{-# LANGUAGE BangPatterns #-}
-- | Graph algorithms in the ST monad.
module Data.Graph.ST (
    Graph, newGraph, newGraphNoDupeNodes,
    successorNodes,
    -- * SCC Computation
    SCC(..), sccs,
    -- * Relation Tools
    nonReflexiveRepresentativesForNodes,
) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Hashable
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as B
import Data.List (sortBy)
import Data.STRef
import qualified Data.Set.MutableBit as BS

type HashTable s k v = B.HashTable s k v

data SCC a = AcyclicSCC a | CyclicSCC [a] deriving (Eq, Show)

nodesOfScc :: SCC a -> [a]
nodesOfScc (AcyclicSCC a) = [a]
nodesOfScc (CyclicSCC as) = as

-- | A graph of 'a's in the state thread s.
--
-- We store the successors in an unboxed array and store indexes into the
-- array for the index at which at a node's successors start. This is very
-- memory efficient and cache friendly.
data Graph s a = Graph {
        nodeMap :: HashTable s a Int,
        invNodeMap :: STArray s Int a,
        successorsArray :: UArray Int Int,
        successorStarts :: UArray Int Int,
        nodeCount :: Int
    }

-- a version of mapM that accumulates on the heap, not the stack.
mapM' f = go [] 
  where 
    go acc [] = return (reverse acc) 
    go acc (a:as) = do {x <- f a; go (x:acc) as} 

successorsForNode :: Graph s a -> Int -> [Int]
successorsForNode gr nid =
    map (\ix -> successorsArray gr!ix) [successorStarts gr!nid..(successorStarts gr!(nid+1) -1)]

successorNodes :: (Eq a, Hashable a) => Graph s a -> a -> ST s [a]
successorNodes graph node = do
    Just nid <- H.lookup (nodeMap graph) node
    mapM (readArray (invNodeMap graph)) (successorsForNode graph nid)

newGraph :: (Eq a, Hashable a) => [a] -> [(a, a)] -> ST s (Graph s a)
newGraph nodes edges = do
    nodeNumberTable <- H.new :: ST s (HashTable s a Int)

    nextNode <- newSTRef 0
    -- We firstly map each of the nodes to an int in [0, length nodes)
    mapM_ (\n -> do
        mnid <- H.lookup nodeNumberTable n
        case mnid of
            Just nid -> return ()
            Nothing -> do
                nid <- readSTRef nextNode
                H.insert nodeNumberTable n nid
                writeSTRef nextNode $! nid+1
        ) nodes
    nodeCount <- readSTRef nextNode

    -- The following requires mapM otherwise we might pop the stack
    intEdges <- mapM' (\(x,y) -> do
            Just ix <- H.lookup nodeNumberTable x
            Just iy <- H.lookup nodeNumberTable y
            return (ix,iy)
        ) edges

    invNodeNumberTable <- newArray_ (0, nodeCount-1) :: ST s (STArray s Int a)
    H.mapM_ (\ (a,aid) -> writeArray invNodeNumberTable aid a) nodeNumberTable

    let edgeCount = length edges
        !sortedEdges = sortBy (\ x y -> compare (fst x) (fst y)) intEdges
        successors :: UArray Int Int
        !successors = listArray (0, edgeCount-1) (map snd sortedEdges)
        -- nodeStarts[nodeCount] = edgeCount, to make everything easier.
        nodeStarts :: UArray Int Int
        !nodeStarts = listArray (0, nodeCount) (computeStarts (-1) 0 sortedEdges)
        computeStarts :: Int -> Int -> [(Int, Int)] -> [Int]
        computeStarts currentNode _ [] | currentNode == nodeCount = []
        computeStarts currentNode eix [] = eix : computeStarts (currentNode+1) eix []
        computeStarts currentNode eix ((n,_):es) | currentNode == n =
            computeStarts currentNode (eix+1) es
        computeStarts currentNode eix ((n,s):es) =
            eix : computeStarts (currentNode+1) eix ((n,s):es)

    return $! Graph {
        nodeMap = nodeNumberTable,
        invNodeMap = invNodeNumberTable,
        successorsArray = successors,
        successorStarts = nodeStarts,
        nodeCount = nodeCount
    }

newGraphNoDupeNodes :: (Eq a, Hashable a) => [a] -> [(a, a)] -> ST s (Graph s a)
newGraphNoDupeNodes nodes edges = do
    let nodeCount = length nodes
        edgeCount = length edges
    
    nodeNumberTable <- H.newSized nodeCount :: ST s (HashTable s a Int)
    invNodeNumberTable <- newArray_ (0, nodeCount-1) :: ST s (STArray s Int a)
    -- We firstly map each of the nodes to an int in [0, length nodes)
    zipWithM (H.insert nodeNumberTable) nodes [0..]
    zipWithM (writeArray invNodeNumberTable) [0..] nodes
    -- The following requires mapM otherwise we might pop the stack
    intEdges <- mapM' (\(x,y) -> do
            Just ix <- H.lookup nodeNumberTable x
            Just iy <- H.lookup nodeNumberTable y
            return (ix,iy)
        ) edges

    let !sortedEdges = sortBy (\ x y -> compare (fst x) (fst y)) intEdges
        successors :: UArray Int Int
        !successors = listArray (0, edgeCount-1) (map snd sortedEdges)
        -- nodeStarts[nodeCount] = edgeCount, to make everything easier.
        nodeStarts :: UArray Int Int
        !nodeStarts = listArray (0, nodeCount) (computeStarts (-1) 0 sortedEdges)
        computeStarts :: Int -> Int -> [(Int, Int)] -> [Int]
        computeStarts currentNode _ [] | currentNode == nodeCount = []
        computeStarts currentNode eix [] = eix : computeStarts (currentNode+1) eix []
        computeStarts currentNode eix ((n,_):es) | currentNode == n =
            computeStarts currentNode (eix+1) es
        computeStarts currentNode eix ((n,s):es) =
            eix : computeStarts (currentNode+1) eix ((n,s):es)

    return $! Graph {
        nodeMap = nodeNumberTable,
        invNodeMap = invNodeNumberTable,
        successorsArray = successors,
        successorStarts = nodeStarts,
        nodeCount = nodeCount
    }

-- | An optimised implementation of Tarjan's SCC algorithm.
sccs :: (Eq a, Hashable a) => Graph s a -> ST s [SCC a]
sccs graph = do
    sccs <- intSccs graph
    mapM (\ scc -> case scc of
                        AcyclicSCC xid -> do
                            x <- readArray (invNodeMap graph) xid
                            return $! AcyclicSCC x
                        CyclicSCC xids -> do
                            xs <- mapM (readArray (invNodeMap graph)) xids
                            return $! CyclicSCC xs) sccs

-- | An optimised implementation of Tarjan's SCC algorithm.
--
-- Returns the SCCs according to a reverse topological sort of the DAG of the
-- SCCs (i.e. if an scc x has an edge to an scc y, then x preceeds y).
intSccs :: (Eq a, Hashable a) => Graph s a -> ST s [SCC Int]
intSccs graph = do
    let successors = successorsArray graph
        nodeStarts = successorStarts graph
    -- The DFS number of each node
    dfsNumber <- newArray (0, nodeCount graph-1) (-1) :: ST s (STArray s Int Int)
    -- The lowlink of each node.
    lowlink <- newArray (0, nodeCount graph-1) (-1) :: ST s (STArray s Int Int)
    -- The current strongly connected component
    pointStack <- newSTRef []
    -- A bitset represetning the set of nodes in the current SCC.
    stackSet <- BS.newSized (nodeCount graph-1)
    -- The next DFS number
    nextDFSNumber <- newSTRef 0
    -- The set of computed SCCs.
    computedSccs <- newSTRef []

    -- We use an explicit stack to avoid popping the Haskell stack. Invariant
    -- is that the given node/transition has not yet been visited.
    programStack <- newSTRef []

    let modifyStackTop nid tid = modifySTRef programStack (\ stk -> (nid, tid):tail stk)
        popStack = do
            _:stk <- readSTRef programStack
            modifySTRef programStack (\ _ -> stk )
            case stk of
                [] -> return ()
                (nid, tid):_ -> visitTransition' nid tid

        strongConnect nid = do
            ix <- readSTRef nextDFSNumber
            writeArray lowlink nid ix
            writeArray dfsNumber nid ix
            writeSTRef nextDFSNumber (ix+1)
            modifySTRef pointStack (\xs -> nid : xs)
            BS.insert stackSet nid

            modifySTRef programStack (\stk -> (nid, nodeStarts!nid) : stk)
            visitTransition nid (nodeStarts!nid)

        visitTransition nid tid | tid >= nodeStarts!(nid+1) = do
            -- Finish visitng this node
            ix <- readArray dfsNumber nid
            ourLowLink <- readArray lowlink nid
            when (ourLowLink == ix) $ do
                -- Found a new SCC - pop items off the stack
                let takeItems is [] = return (is, [])
                    takeItems is (nid:nids) = do
                        nodeNum <- readArray dfsNumber nid
                        if nodeNum >= ix then takeItems (nid:is) nids
                        else return (is, nid:nids)
                items <- readSTRef pointStack
                (scc, remaining) <- takeItems [] items
                mapM_ (BS.remove stackSet) scc
                writeSTRef pointStack remaining
                let !scc' = case scc of
                                [x] | x `elem` successorsForNode graph x -> CyclicSCC [x]
                                [x] -> AcyclicSCC x
                                _ -> CyclicSCC scc
                modifySTRef computedSccs (\sccs -> scc' : sccs)
            popStack
        visitTransition nid tid = do
            let sid = successors!tid
        
            dfsNum <- readArray dfsNumber sid
            if dfsNum == -1 then do
                -- Found a tree arc
                -- Record that we need to come back to here.
                modifyStackTop nid tid
                strongConnect sid
            else do
                b <- BS.member stackSet sid
                when b $ do
                    theirNumber <- readArray dfsNumber sid
                    ourLowLink <- readArray lowlink nid
                    writeArray lowlink nid (min ourLowLink theirNumber) 
                visitTransition nid (tid+1)

        visitTransition' nid tid = do
            let sid = successors!tid
            ourLowLink <- readArray lowlink nid
            theirLowLink <- readArray lowlink sid
            writeArray lowlink nid (min ourLowLink theirLowLink)
            visitTransition nid (tid+1)

    mapM_ (\nid -> do
        num <- readArray dfsNumber nid
        when (num == -1) $ strongConnect nid) [0..nodeCount graph-1]
    readSTRef computedSccs

-- | Given a graph, computes the transitive (but not reflexive) closure of the
-- graph and then returns the relation (a,b) such that b is the representative
-- member for a. Note, no pairs of the form a == b are returned, even if there
-- is an edge from a to b. This is to minimise the size of the transitive
-- closure.
nonReflexiveRepresentativesForNodes :: (Eq a, Hashable a) => Graph s a -> ST s [(a, a)]
nonReflexiveRepresentativesForNodes graph = do
    sccs <- intSccs graph
    let sccCount = length sccs
    -- A map from node id to the id of the scc id
    sccForNode <- newArray (0, nodeCount graph-1) (-1) :: ST s (STUArray s Int Int)
    zipWithM (\ sccId scc ->
            mapM_ (\ nid -> writeArray sccForNode nid sccId) (nodesOfScc scc)
        ) [0..] sccs

    -- Map from scc id to node representative (of the scc)
    sccRepresentatives <- newArray (0, sccCount-1) (-1) :: ST s (STUArray s Int Int)
    zipWithM (\ sccId scc ->
            writeArray sccRepresentatives sccId (head (nodesOfScc scc))
        ) [0..] sccs

    -- Now, create the representative pairs
    xss <- mapM' (\nid -> do
            sccId <- readArray sccForNode nid
            sccRepr <- readArray sccRepresentatives sccId
            if sccRepr == nid then 
                -- don't include
                return []
            else do
                n <- readArray (invNodeMap graph) nid
                repr <- readArray (invNodeMap graph) sccRepr
                return [(n, repr)]
        ) [0..nodeCount graph-1]
    return $! concat xss
