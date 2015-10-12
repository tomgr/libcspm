module CSPM.Symmetry.Monad (
    SymmetryState, initialSymmetryDetectorState,
    NonSymmetryReason(..), SymmetryType(..),
    SymmetryMonad, runSymmetryDetector,
    recordNonSymmetryReason,
    recordDeclerationNonSymmetries,
    registerType, registerDeclarations,
    symmetryScope, allRegisteredTypes,
    scopeNonSymmetryReasons,
    transitiveNonSymmetriesOfNode,
    analyseTypeVariables,
) where

import Control.Monad.State
import qualified Data.Map as M

import CSPM.Syntax.AST
import CSPM.Syntax.Names
import CSPM.Syntax.Types
import CSPM.Symmetry.BuiltInFunctions
import CSPM.Symmetry.DeclarationFinder
import CSPM.Symmetry.DependencyGraph
import CSPM.Symmetry.TypeVariableDependency
import Util.Annotated
import Util.Monad

-- | An explanation for why something is not symmetric in a certain type.
data NonSymmetryReason =
    -- | Contains a constant of the symmetric type. The expression is the
    -- use of the constant, whilst the name specifies the constant
    -- (i.e. data-type constructor) used.
    ExpContainsConstant TCExp Name
    | PatContainsConstant TCPat Name
    -- | This function itself is not necessarily non-symmetric, but it calls a
    -- non-symmetric function, which is non symmetric for the reason given.
    | DependsOnNonSymmetricItem DependencyReason NonSymmetryReason
    -- | A builtin function that is non-symmetric.
    | BuiltinNonSymmetric Name
    -- | A call to a non-symmetric polymorphic function, named n, where the
    -- typevar of the called function is instantiated with type.
    | InstantiatesNonSymmetricPolymorphicItem DependencyReason TypeVar Type
        NonSymmetryReason

data SymmetryType =
    FullySymmetric {
        symmetryTypeType :: Type
    }
    -- | Fully symmetric over sym(T) - {| ns |}
    | PartiallySymmetric {
        symmetryTypeType :: Type,
        symmetryTypeNonSymmetricConstants :: [Name]
    }
    deriving Show

data SymmetryState = SymmetryState {
        -- | A list of all types that are being considered for symmetries.
        allTypes :: [(Type, [Name])],
        dependencyGraph :: DependencyGraph,
        typeVariableDependencyGraph :: TypeVariableDependencyGraph,
        currentNonSymmetryReasons :: [(Type, NonSymmetryReason)],
        declarationNonSymmetryReasons ::
            M.Map DependencyGraphNode [(Type, NonSymmetryReason)]
    }

type SymmetryMonad = State SymmetryState

allRegisteredTypes :: SymmetryMonad [(Type, [Name])]
allRegisteredTypes = gets allTypes

recordNonSymmetryReason :: Type -> NonSymmetryReason -> SymmetryMonad ()
recordNonSymmetryReason t reason =
    modify (\ st -> st {
            currentNonSymmetryReasons = (t, reason):currentNonSymmetryReasons st
        })

initialSymmetryDetectorState :: SymmetryState
initialSymmetryDetectorState = SymmetryState {
        allTypes = [],
        dependencyGraph = g,
        typeVariableDependencyGraph = initialTypeVariableDependencyGraph g,
        currentNonSymmetryReasons = [],
        declarationNonSymmetryReasons = M.empty
    }
    where g = initialDependencyGraph

runSymmetryDetector :: SymmetryMonad a -> SymmetryState -> (a, SymmetryState)
runSymmetryDetector = runState

registerType :: Type -> [Name] -> SymmetryMonad ()
registerType typ constructors = modify (\ st -> st {
        allTypes = (typ, constructors) : allTypes st
    })

registerDeclarations :: FindDeclarations a => a -> SymmetryMonad [(TCDecl, Maybe DependencyGraphNode)]
registerDeclarations ds = do
    g <- gets dependencyGraph
    let (g', node) = addDeclarations ds g
    tvg <- gets typeVariableDependencyGraph
    let tvg' = analyseTypeVariableDeclarations g' node tvg
    modify (\ st -> st {
            dependencyGraph = g',
            typeVariableDependencyGraph = tvg'
        })
    return node

symmetryScope :: SymmetryMonad a -> SymmetryMonad a
symmetryScope prog = do
    nonSymmetryReasons <- gets currentNonSymmetryReasons
    modify (\ st -> st { currentNonSymmetryReasons = [] })
    a <- prog
    modify (\ st -> st { currentNonSymmetryReasons = nonSymmetryReasons })
    return a

-- | Records the non-symmetry reasons to the declaration in question.
recordDeclerationNonSymmetries :: DependencyGraphNode -> SymmetryMonad ()
recordDeclerationNonSymmetries n = do
    modify (\ st -> st {
            declarationNonSymmetryReasons =
                M.insert n (currentNonSymmetryReasons st) (declarationNonSymmetryReasons st)
        })

declarationNodeForName :: Name -> SymmetryMonad (Maybe DependencyGraphNode)
declarationNodeForName n = do
    g <- gets dependencyGraph
    return $ nodeForName n g

analyseTypeVariables :: Name -> SrcSpan -> Type -> SymmetryMonad ()
analyseTypeVariables n loc instantiatedTyp = do
    depGraph <- gets dependencyGraph
    typGraph <- gets typeVariableDependencyGraph
    mapM_ (\ (n, tv, typ) -> do
        let (endName, path) = pathFromTypeVar n tv depGraph typGraph
            make [] = BuiltinNonSymmetric endName
            make (depReason:rs) =
                DependsOnNonSymmetricItem depReason (make rs)
            childReason = make path
        mapM_ (\ td ->
            recordNonSymmetryReason td $
                InstantiatesNonSymmetricPolymorphicItem (CallsName loc n) tv typ childReason
            ) (allDataTypes typ)
        ) (analyseTypeVariableUsage n instantiatedTyp depGraph typGraph)

transitiveNonSymmetriesOfNode :: (SrcSpan, Name) ->
    SymmetryMonad [(Type, NonSymmetryReason)]
transitiveNonSymmetriesOfNode (callSite, name) = do
    g <- gets dependencyGraph
    ts <- gets allTypes
    n <- declarationNodeForName name
    let
        constructTransitiveReason :: [DependencyReason] ->
            (Type, NonSymmetryReason) -> (Type, NonSymmetryReason)
        constructTransitiveReason path (typ, baseReason) =
                (typ, DependsOnNonSymmetricItem (CallsName callSite name) (transReason path))
            where
                transReason [] = baseReason
                transReason (r : path) =
                    DependsOnNonSymmetricItem r (transReason path)

        constructReason :: (DependencyGraphNode, [DependencyReason]) ->
            SymmetryMonad [(Type, NonSymmetryReason)]
        constructReason (n, path) =
            case nodeNames n g of
                [n] | nameType n == WiredInName -> return $ 
                    if isNonPolymorphicBuiltinNonSymmetric n then
                        [(typ, BuiltinNonSymmetric n) | (typ, _) <- ts]
                    else []
                ns -> do
                    reasons <- declarationNonSymmetries n
                    case reasons of
                        Nothing -> return []
                        Just rs -> return $ map (constructTransitiveReason path) rs

    case n of
        Just n -> concatMapM constructReason (allDependenciesOfNode n g)
        Nothing -> return []

-- | Returns the symmetry information deduced for the given name, if it has been
-- analysed.
declarationNonSymmetries :: DependencyGraphNode ->
    SymmetryMonad (Maybe [(Type, NonSymmetryReason)])
declarationNonSymmetries n = do
    m <- gets declarationNonSymmetryReasons
    return $ M.lookup n m

scopeNonSymmetryReasons :: SymmetryMonad [(Type, NonSymmetryReason)]
scopeNonSymmetryReasons = gets currentNonSymmetryReasons
