{-# LANGUAGE FlexibleInstances #-}
module CSPM.Symmetry.DependencyGraph (
    DependencyGraph, initialDependencyGraph,
    DependencyGraphNode,
    DependencyReason(..),
    addDeclarations,
    allDependenciesOfNode,
    nodeForName, nodeForName',
    nodeNames,
    declarationType,
) where

import Control.Monad.State
import qualified Data.Graph.Inductive as G
import qualified Data.Map as M
import Data.Maybe (fromJust)

import CSPM.Prelude
import CSPM.Symmetry.DeclarationFinder
import CSPM.Symmetry.Dependency
import CSPM.Syntax.AST
import CSPM.Syntax.FreeVars (boundNames)
import CSPM.Syntax.Names
import CSPM.Syntax.Types
import Util.Annotated
import Util.Exception

data DependencyReason =
    -- | A dependency because this name calls the other name.
    CallsName SrcSpan Name
    -- | An implicit dependency, such as inclusion within a module, or within
    -- a timed section etc.
    | ContainedWithinModule Name SrcSpan
    | ContainedWithinTimedSection SrcSpan
    deriving Show

type DependencyGraphNodeLabel = TCDecl

data DependencyGraph = DependencyGraph {
        graph :: G.Gr () DependencyReason,
        nextDeclId :: Int,
        namesForNode :: M.Map Int [Name],
        declarationForName :: M.Map Name Int,
        declarationTypes :: M.Map Name TypeScheme
    }

newtype DependencyGraphNode = DependencyGraphNode G.Node deriving (Eq, Ord, Show)

initialDependencyGraph :: DependencyGraph
initialDependencyGraph = 
    let allBuiltins = builtins True
        declarationForName = M.fromList $ zip (map name allBuiltins) [0..]
    in DependencyGraph {
            graph = G.mkGraph [(i, ()) | i <- [0..length allBuiltins-1]] [],
            nextDeclId = length allBuiltins,
            namesForNode = M.fromList $ zip [0..] (map (\ s -> [name s]) allBuiltins),
            declarationForName = declarationForName,
            declarationTypes = M.fromList [(name b, typeScheme b) | b <- allBuiltins]
        }

-- | Adds the given declaration to the dependency graph.
addDeclarations :: FindDeclarations a => a -> DependencyGraph ->
    (DependencyGraph, [(TCDecl, Maybe DependencyGraphNode)])
addDeclarations ds g =
    let (ns, g') = runState (addNodesToGraph (findDeclarations ds)) g
    in (g', ns)

nodeForName :: Name -> DependencyGraph -> Maybe DependencyGraphNode
nodeForName n g =
    case M.lookup n (declarationForName g) of
        Just startNode -> Just (DependencyGraphNode startNode)
        Nothing -> Nothing

nodeForName' :: Name -> DependencyGraph -> DependencyGraphNode
nodeForName' n g = 
    case M.lookup n (declarationForName g) of
        Just startNode -> DependencyGraphNode startNode
        Nothing -> panic $ "Could not locate node for "++show n

declarationType :: Name -> DependencyGraph -> Maybe TypeScheme
declarationType n g = M.lookup n (declarationTypes g)

-- | If the node corresponds to a builtin name, returns the name.
nodeNames :: DependencyGraphNode -> DependencyGraph -> [Name]
nodeNames (DependencyGraphNode n) graph =
    fromJust $ M.lookup n (namesForNode graph)

-- | Given a name, returns a list of names that this depends on, along with a
-- reason why it is a dependency (i.e. a path through  the depdendency path).
--
-- The paths are guaranteed to be minimal in length.
allDependenciesOfNode :: DependencyGraphNode -> DependencyGraph ->
    [(DependencyGraphNode, [DependencyReason])]
allDependenciesOfNode (DependencyGraphNode startNode) g =
    let bfsTree = G.bft startNode (graph g)
        -- each path in bfstree is reversed, and ends at a particular node.
        allPaths :: [[G.Node]]
        allPaths = map reverse bfsTree
        reasonForEdge :: G.Node -> G.Node -> DependencyReason
        reasonForEdge from to = r
            where (_, _, r) = head $ filter (\ (_, e, _) -> to == e) $ G.out (graph g) from
        reasonForPath [_] = []
        reasonForPath (x:y:xs) = reasonForEdge x y : reasonForPath (y:xs)
        constructReason path =
            (DependencyGraphNode end, reasonForPath path)
            where end = last path
    in map constructReason allPaths

-- ****************************************************************************
-- Graph Construction

type DependencyGraphMonad a = State DependencyGraph a

insertNode :: [Name] -> DependencyGraphMonad G.Node
insertNode boundNames = do
    declId <- gets nextDeclId
    modify (\ st -> st {
            graph = G.insNode (declId, ()) (graph st),
            nextDeclId = declId + 1,
            declarationForName =
                M.union (declarationForName st) (M.fromList [(n, declId) | n <- boundNames]),
            namesForNode = M.insert declId boundNames (namesForNode st)
        })
    return declId

insertNodeForDecl :: TCDecl -> [Name] -> DependencyGraphMonad G.Node
insertNodeForDecl (An _ (boundSymbols, _) _) boundNames = do
    modify (\ st -> st {
            declarationTypes = M.union (declarationTypes st) (M.fromList boundSymbols)
        })
    insertNode boundNames

addEdge :: DependencyReason -> G.Node -> G.Node -> DependencyGraphMonad ()
addEdge reason from to = 
    modify (\ st -> st {
        graph = G.insEdge (from, to, reason) (graph st)
    })

maybeAddEdge :: DependencyReason -> G.Node ->
    (TCDecl, Maybe DependencyGraphNode) ->  DependencyGraphMonad ()
maybeAddEdge reason to (_, Just (DependencyGraphNode dn)) = addEdge reason dn to
maybeAddEdge _ _ (_, Nothing) = return ()

addCallEdge :: G.Node -> (SrcSpan, Name) -> DependencyGraphMonad ()
addCallEdge from (callSite, dep) =
    modify (\ st ->
        case M.lookup dep (declarationForName st) of
            Just to -> st {
                    graph = G.insEdge (from, to, CallsName callSite dep) (graph st)
                }
            Nothing -> st)

addNodesToGraph :: [TCDecl] -> DependencyGraphMonad [(TCDecl, Maybe DependencyGraphNode)]
addNodesToGraph ds = do
    ns <- mapM addNodeToGraph ds
    sequence ns >>= return . concat

addNodeToGraph :: TCDecl -> DependencyGraphMonad (DependencyGraphMonad
    [(TCDecl, Maybe DependencyGraphNode)])

addNodeToGraph d@(An _ _ (External _)) = return $! return [(d, Nothing)]
addNodeToGraph d@(An _ _ (PrintStatement e)) = return $! return [(d, Nothing)]
addNodeToGraph d@(An _ _ (Transparent _)) = return $! return [(d, Nothing)]

addNodeToGraph d@(An _ _ (Assert e)) = do
    createDeclEdges <- mapM addNodeToGraph (findDeclarations e)
    return $! do
        declNodes <- sequence createDeclEdges >>= return . concat
        return $ (d, Nothing) : declNodes
addNodeToGraph d@(An _ _ (Channel ns es ta)) = do
    n <- insertNodeForDecl d (boundNames d)
    createDeclEdges <- mapM addNodeToGraph (findDeclarations es)
    return $! do
        mapM_ (addCallEdge n) (dependencies d)
        declNodes <- sequence createDeclEdges >>= return . concat
        return $ (d, Just (DependencyGraphNode n)) : declNodes
addNodeToGraph d@(An _ _ (DataType n cs)) = do
    n <- insertNodeForDecl d (boundNames d)
    createDeclEdges <- mapM addNodeToGraph (findDeclarations cs)
    return $! do
        mapM_ (addCallEdge n) (dependencies d)
        declNodes <- sequence createDeclEdges >>= return . concat
        return $ (d, Just (DependencyGraphNode n)) : declNodes
addNodeToGraph d@(An loc _ (FunBind fn ms ta)) = do
    n <- insertNodeForDecl d (boundNames d)
    createDeclEdges <- mapM addNodeToGraph (findDeclarations ms)
    return $! do
        mapM_ (addCallEdge n) (dependencies d)
        declNodes <- sequence createDeclEdges >>= return . concat
        return $ (d, Just (DependencyGraphNode n)) : declNodes
addNodeToGraph d@(An loc _ (Module mn _ priv pub)) = do
    moduleNode <- insertNodeForDecl d [mn]
    createDeclEdges <- mapM addNodeToGraph (priv++pub)
    return $! do
        declNodes <- sequence createDeclEdges >>= return . concat
        mapM_ (maybeAddEdge (ContainedWithinModule mn loc) moduleNode) declNodes
        return $ (d, Just (DependencyGraphNode moduleNode)) : declNodes
addNodeToGraph d@(An loc _ (ModuleInstance mn nt args nm _)) = do
    instanceNode <- insertNodeForDecl d [mn]
    createDeclEdges <- mapM (\ (oldName, newName) -> do
        newNameNode <- insertNode [newName]
        return $ do
            g <- gets id
            let DependencyGraphNode oldNameNode = nodeForName' oldName g
            addEdge (CallsName loc oldName) newNameNode oldNameNode
            addEdge (ContainedWithinModule mn loc) newNameNode instanceNode
            ) (M.toList nm)
    return $! do
        sequence createDeclEdges
        addCallEdge instanceNode (loc, nt)
        return [(d, Just (DependencyGraphNode instanceNode))]
addNodeToGraph d@(An _ _ (NameType n e ta)) = do
    n <- insertNodeForDecl d (boundNames d)
    createDeclEdges <- mapM addNodeToGraph (findDeclarations e)
    return $! do
        mapM_ (addCallEdge n) (dependencies d)
        declNodes <- sequence createDeclEdges >>= return . concat
        return $ (d, Just (DependencyGraphNode n)) : declNodes
addNodeToGraph d@(An _ _ (PatBind p e ta)) = do
    n <- insertNodeForDecl d (boundNames d)
    createDeclEdges <- mapM addNodeToGraph (findDeclarations e)
    return $! do
        mapM_ (addCallEdge n) (dependencies d)
        declNodes <- sequence createDeclEdges >>= return . concat
        return $ (d, Just (DependencyGraphNode n)) : declNodes
addNodeToGraph d@(An _ _ (SubType n cs)) = do
    n <- insertNodeForDecl d (boundNames d)
    createDeclEdges <- mapM addNodeToGraph (findDeclarations cs)
    return $! do
        mapM_ (addCallEdge n) (dependencies d)
        declNodes <- sequence createDeclEdges >>= return . concat
        return $ (d, Just (DependencyGraphNode n)) : declNodes
addNodeToGraph d@(An loc _ (TimedSection _ me ds)) = do
    timedNode <- insertNodeForDecl d []
    createDeclEdges <- mapM addNodeToGraph ds
    return $! do
        mapM_ (addCallEdge timedNode) (dependencies me)
        declNodes <- sequence createDeclEdges >>= return . concat
        mapM_ (maybeAddEdge (ContainedWithinTimedSection loc) timedNode) declNodes
        return $ (d, Just (DependencyGraphNode timedNode)) : declNodes
