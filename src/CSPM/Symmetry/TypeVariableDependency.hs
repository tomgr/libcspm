module CSPM.Symmetry.TypeVariableDependency (
    TypeVariableDependencyGraph,
    initialTypeVariableDependencyGraph,
    analyseTypeVariableDeclarations,
    analyseTypeVariableUsage,
    pathFromTypeVar,
    allDataTypes,
) where

import Control.Monad.State
import qualified Data.Graph.Inductive as G
import Data.List (nub, sort)
import qualified Data.Map as M
import qualified Data.Set as S

import CSPM.Prelude
import CSPM.Symmetry.BuiltInFunctions
import CSPM.Symmetry.DependencyGraph
import CSPM.Syntax.AST
import CSPM.Syntax.Names
import CSPM.Syntax.Types
import CSPM.Syntax.Visitor
import Util.Annotated

-- Consider the graph where the nodes are pairs (f, a) where f is a function
-- declaration, and a is a type variable, and there is an edge from (f, a) ->
-- (f’,a’) iff f can call f’ and instantiate the type variable a’ with a type
-- that contains a.

data TypeVariableDependencyGraph = TypeVariableDependencyGraph {
        graph :: G.Gr (DependencyGraphNode, TypeVar) DependencyReason,
        builtinNodes :: S.Set G.Node,
        nodeMap :: M.Map (DependencyGraphNode, TypeVar) G.Node,
        invNodeMap :: M.Map G.Node (DependencyGraphNode, TypeVar)
    }

initialTypeVariableDependencyGraph :: DependencyGraph ->
    TypeVariableDependencyGraph
initialTypeVariableDependencyGraph depGraph =
        TypeVariableDependencyGraph {
                graph = G.mkGraph nodes [],
                builtinNodes = S.fromList $ map fst nodes,
                nodeMap = M.fromList (map (\ (a, b) -> (b, a)) nodes),
                invNodeMap = M.fromList nodes
            }
    where
        nodes = zip [0..] nodeLabels
        nodeLabels = [(nodeForName' (name b) depGraph, tv) | b <- builtins True,
                        isPolymorphicBuiltinNonSymmetric (name b),
                        let ForAll cs _ = typeScheme b,
                        (tv, _) <- cs]

type EdgeFinderMonad = State (DependencyGraphNode, [G.LEdge DependencyReason])

analyseTypeVariableDeclarations ::
    DependencyGraph
    -> [(TCDecl, Maybe DependencyGraphNode)]
    -> TypeVariableDependencyGraph
    -> TypeVariableDependencyGraph
analyseTypeVariableDeclarations depGraph decls polyGraph =
    let
        -- Firstly create nodes for each polymorphic declaration
        createNodes (decl, Nothing) = []
        createNodes (An _ (boundTypes, _) (ModuleInstance _ _ _ nm _), _) =
            [(nodeForName' n depGraph, tv) |
                (n, ForAll cs _) <- boundTypes, (tv, _) <- cs]
        createNodes (decl, Just n) =
            [(n, tv) | (_, ForAll cs _) <- getSymbolTable decl, (tv, _) <- cs]
        newNodeLabels = concatMap createNodes decls
        newNodes = zip (G.newNodes (length newNodeLabels) (graph polyGraph)) newNodeLabels
        newNodeMap =
            M.union (nodeMap polyGraph)
                (M.fromList (map (\ (a, b) -> (b, a)) newNodes))
        nodeFor n = M.lookup n newNodeMap

        -- Deduce dependencies amongst the polymorphic declarations by visisting
        -- each declaration.
        visitor = defaultVisitor {
                visitExp = visitExp
            }
        edgesForDecl :: (TCDecl, Maybe DependencyGraphNode) -> [G.LEdge DependencyReason]
        edgesForDecl (decl, Nothing) = []
        edgesForDecl (An loc (boundTypes, _) (ModuleInstance _ _ _ nm _), Just node) =
            let invNameMap = M.fromList [(b, a) | (a, b) <- M.toList nm]
            in concatMap (\ (ourName, ForAll _ instantiatedTyp) -> 
                let Just oldName = M.lookup ourName invNameMap
                    ourNode = nodeForName' ourName depGraph
                    targetNode = nodeForName' oldName depGraph
                in case declarationType oldName depGraph of
                    Nothing -> []
                    Just (ForAll [] _) -> []
                    Just (ForAll _ typ) ->
                        let sub = findSubstitution typ instantiatedTyp
                            edgesForSub (calledVar, newType) = concatMap (\ tv -> 
                                case (nodeFor (ourNode, typeVar tv), nodeFor (targetNode, calledVar)) of
                                    (Just from, Just to) -> [(from, to, CallsName loc oldName)]
                                    _ -> []
                                ) (allTypeVars newType)
                        in concatMap edgesForSub sub
                ) boundTypes
        edgesForDecl (decl, Just node) =
            snd $ execState (visit visitor decl) (node, [])
        newEdges = concatMap edgesForDecl decls

        visitExp :: TCExp -> EdgeFinderMonad () -> EdgeFinderMonad ()
        visitExp (An loc (instantiatedTyp, _) (Var n)) _ = do
            ourNode <- gets fst
            case declarationType n depGraph of
                Nothing -> return ()
                Just (ForAll [] typ) -> return ()
                Just (ForAll _ typ) -> do
                    let targetNode = nodeForName' n depGraph
                        sub = findSubstitution typ instantiatedTyp
                        edgesForSub (calledVar, newType) = concatMap (\ tv -> 
                            case (nodeFor (ourNode, typeVar tv), nodeFor (targetNode, calledVar)) of
                                (Just from, Just to) -> [(from, to, CallsName loc n)]
                                _ -> []
                            ) (allTypeVars newType)
                        newEdges = concatMap edgesForSub sub
                    modify (\ (n, es) -> (n, newEdges ++ es))
        visitExp _ prog = prog

        newGraph = G.insEdges newEdges (G.insNodes newNodes (graph polyGraph))

        -- Now we should restrict newGraph to only include nodes that can
        -- ultiamtely depend on one of the builtin nonpolymorphic functions.
        -- Other functions are innocuous.
        reverseReachableNodes = S.toList (builtinNodes polyGraph)
            ++ G.rdfs (S.toList (builtinNodes polyGraph)) newGraph
        reverseReachableNodesSet = S.fromList reverseReachableNodes
        nodeIsRelevant (a, _) = S.member a reverseReachableNodesSet
        edgeIsRelevant (a, b, _) =
            S.member a reverseReachableNodesSet
            || S.member b reverseReachableNodesSet
        restrictedGraph = G.mkGraph
            (filter nodeIsRelevant (G.labNodes newGraph))
            (filter edgeIsRelevant (G.labEdges newGraph))
    in TypeVariableDependencyGraph {
            graph = restrictedGraph,
            builtinNodes = builtinNodes polyGraph,
            nodeMap = M.fromList [(a, b) | (a, b) <- M.toList newNodeMap,
                                            S.member b reverseReachableNodesSet],
            invNodeMap = M.fromList [(b, a) | (a, b) <- M.toList newNodeMap,
                                            S.member b reverseReachableNodesSet]
        }

-- | Analyses an application of the given function name, which has been
-- instantiated  with the given type. It returns a list of 'errors', where an
-- error is a triple (n, tv, t) indicating that the function n was called with
-- tv instantiated to the concrete type t.
analyseTypeVariableUsage :: Name -> Type -> DependencyGraph ->
    TypeVariableDependencyGraph -> [(Name, TypeVar, Type)]
analyseTypeVariableUsage n instantiatedType depGraph polyGraph =
    case declarationType n depGraph of
        Nothing -> []
        Just (ForAll [] typ) -> []
        Just (ForAll _ typ) ->
            let targetNode = nodeForName' n depGraph
                sub = findSubstitution typ instantiatedType
            in case sub of
                [] -> []
                (tv, typ):_ -> 
                    case M.lookup (targetNode, tv) (nodeMap polyGraph) of
                        Nothing -> []
                        Just node -> [(n, tv, typ)]

-- | Computes a path from the given name typevar pair to a builtin non-symmetric
-- member. This assumes that there is a node for (name, typevar).
pathFromTypeVar :: Name -> TypeVar -> DependencyGraph ->
    TypeVariableDependencyGraph -> (Name, [DependencyReason])
pathFromTypeVar n tv depGraph polyGraph =
    let targetNode = nodeForName' n depGraph
        Just startNode = M.lookup (targetNode, tv) (nodeMap polyGraph)
        isFinishedPath (end:_) = S.member end (builtinNodes polyGraph)
        bfsTree = filter isFinishedPath $ G.bft startNode (graph polyGraph)
        allPaths = map reverse bfsTree
        reasonForEdge :: G.Node -> G.Node -> DependencyReason
        reasonForEdge from to = r
            where (_, _, r) = head $ filter (\ (_, e, _) -> to == e) $ G.out (graph polyGraph) from
        reasonForPath [_] = []
        reasonForPath (x:y:xs) = reasonForEdge x y : reasonForPath (y:xs)
        constructReason path =
            let
                Just (end, _) = M.lookup (last path) (invNodeMap polyGraph)
                endName = head $ nodeNames end depGraph
            in (endName, reasonForPath path)
    in constructReason (head allPaths)

-- | Attempts to finds a substitution made to the first type to yield the
-- second type.
findSubstitution :: Type -> Type -> [(TypeVar, Type)]
findSubstitution (TVar v) t = [(typeVar v, t)]
findSubstitution (TFunction targs1 tr1) (TFunction targs2 tr2) =
    concat $ zipWith findSubstitution (tr1:targs1) (tr2:targs2)
findSubstitution (TSeq t1) (TSeq t2) = findSubstitution t1 t2
findSubstitution (TSet t1) (TSet t2) = findSubstitution t1 t2
findSubstitution (TTuple ts1) (TTuple ts2) =
    concat $ zipWith findSubstitution ts1 ts2
findSubstitution (TDot l1 l2) (TDot r1 r2) =
    findSubstitution l1 r1 ++ findSubstitution l2 r2
findSubstitution (TDotable l1 l2) (TDotable r1 r2) =
    findSubstitution l1 r1 ++ findSubstitution l2 r2
findSubstitution (TMap k1 v1) (TMap k2 v2) =
    findSubstitution k1 k2 ++ findSubstitution v1 v2
findSubstitution (TExtendable t1 _) (TExtendable t2 _) = findSubstitution t1 t2
findSubstitution TProc TProc = []
findSubstitution TInt TInt = []
findSubstitution TBool TBool = []
findSubstitution TChar TChar = []
findSubstitution TEvent TEvent = []
findSubstitution TExtendableEmptyDotList TExtendableEmptyDotList =[]
findSubstitution (TDatatype _) (TDatatype _) = []
findSubstitution t1 t2 = 
    -- In this case, we have failed to find a substitution. In this case,
    -- we just assume the pessimal case and assuming all type vars are related
    [(typeVar tv1, t2) | tv1 <- allTypeVars t1]

-- | Returns the set of all type variables contained in the given type.
allTypeVars :: Type -> [TypeVarRef]
allTypeVars t = nub $ sort $ search t
    where
        search (TVar v) = [v]
        search (TFunction targs tr) = concatMap search (tr:targs)
        search (TSeq t) = search t
        search (TSet t) = search t
        search (TTuple ts) = concatMap search ts
        search (TDot t1 t2) = search t1 ++ search t2
        search (TDotable t1 t2) = search t1 ++ search t2
        search (TMap k v) = search k ++ search v
        search (TExtendable t tvref) = tvref : search t
        search TProc = []
        search TInt = []
        search TBool = []
        search TChar = []
        search TEvent = []
        search TExtendableEmptyDotList = []
        search (TDatatype _) = []

allDataTypes :: Type -> [Type]
allDataTypes t = nub $ sort $ search t
    where
        search (TVar v) = []
        search (TFunction targs tr) = concatMap search (tr:targs)
        search (TSeq t) = search t
        search (TSet t) = search t
        search (TTuple ts) = concatMap search ts
        search (TDot t1 t2) = search t1 ++ search t2
        search (TDotable t1 t2) = search t1 ++ search t2
        search (TMap k v) = search k ++ search v
        search (TExtendable t _) = search t
        search TProc = []
        search TInt = []
        search TBool = []
        search TChar = []
        search TEvent = []
        search TExtendableEmptyDotList = []
        search t@(TDatatype n) = [t]
