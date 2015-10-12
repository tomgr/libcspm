{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
module CSPM.Symmetry (
    SymmetriesDectectable(analyseSymmetries),
    SymmetryState, SymmetryMonad,
    SymmetryType(..),
    initialSymmetryDetectorState,
    runSymmetryDetector,
    checkIsSymmetric,
    checkSymmetryAssertion,
    listSymmetricTypes,
    allPossiblySymmetricTypes,
    createSymmetryTrackerFunction,
)
where

import Control.Monad.State
import Data.List (groupBy, nub, sort, sortBy, (\\))
import qualified Data.Map as M
import qualified Data.Set as S

import CSPM.Symmetry.Detector
import CSPM.Symmetry.DeclarationFinder
import CSPM.Symmetry.Dependency
import CSPM.Symmetry.Exceptions
import CSPM.Symmetry.Monad
import CSPM.Syntax.Names
import CSPM.Syntax.Types
import CSPM.Syntax.AST
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

extractConstants :: [NonSymmetryReason] -> [Name]
extractConstants rs =
    let 
        extract (ExpContainsConstant _ n) = [n]
        extract (PatContainsConstant _ n) = [n]
        extract (DependsOnNonSymmetricItem _ r) = extract r
        extract (BuiltinNonSymmetric {}) = []
        extract (InstantiatesNonSymmetricPolymorphicItem {}) = []
    in concatMap extract rs

-- | Given a set of all possible types, and a set of discovered non-symmetry
-- reasons, computes combined symmetry deductions for each type.
computeSymmetryTypes :: [(Type, [Name])] -> [(Type, NonSymmetryReason)] ->
    [(Either Type SymmetryType, [NonSymmetryReason])]
computeSymmetryTypes allTypes reasons =
    let
        groupedReasons :: [(Type, [NonSymmetryReason])]
        groupedReasons =
            map (\ ts -> (fst (head ts), map snd ts)) $
            groupBy (\ (x, _) (y, _) -> x == y) $
            sortBy  (\ (x, _) (y, _) -> compare x y) reasons

        fullySymmetricTypes = map fst allTypes \\ (nub (sort (map fst groupedReasons)))

        constantsForType typ = 
            case lookup typ allTypes of
                Just cs -> cs
                Nothing -> panic "constants not found"

        constructSymmetryType :: (Type, [NonSymmetryReason]) -> Either Type SymmetryType
        constructSymmetryType (t, rs) =
            let
                check [] =
                    if cs == nub (sort (constantsForType t)) then 
                        -- Not symmetric at all
                        Left t
                    else Right $ PartiallySymmetric t cs
                    where cs = nub $ sort $ extractConstants rs
                check (ExpContainsConstant _ n : rs) = check rs
                check (PatContainsConstant _ n : rs) = check rs
                check (DependsOnNonSymmetricItem _ r : rs) = check (r:rs)
                check (BuiltinNonSymmetric _ : _) = Left t
                check (InstantiatesNonSymmetricPolymorphicItem _ _ _ _ : _) = Left t
            in check rs
    in
        [(Right $ FullySymmetric t, []) | t <- fullySymmetricTypes]
        ++ (map (\ rs -> (constructSymmetryType rs, snd rs)) groupedReasons)

listSymmetricTypes' :: (Dependencies a, FindDeclarations a, SymmetriesDectectable a) => a ->
    SymmetryMonad [(Either Type SymmetryType, [NonSymmetryReason])]
listSymmetricTypes' val = symmetryScope $ do
    analyseSymmetries val
    localReasons <- scopeNonSymmetryReasons
    -- Find all transitive reasons
    transitiveReasons <- mapM transitiveNonSymmetriesOfNode (dependencies val)
    types <- allRegisteredTypes
    return $ computeSymmetryTypes types $ localReasons ++ concat transitiveReasons

-- | Given a required type, and an actual symmetry type, returns the list of
-- non symmetry reasons for that type, formatted as error messages.
constructError :: SymmetryType -> (Either Type SymmetryType, [NonSymmetryReason]) ->
                    [NonSymmetryReason]
constructError (FullySymmetric _) (Right (FullySymmetric _), _) = []
constructError (FullySymmetric _) (actual, nonSymReasons) = nonSymReasons
constructError (PartiallySymmetric _ ns1) (Right (FullySymmetric _), _) = []
constructError (PartiallySymmetric _ ns1) (Right (PartiallySymmetric _ ns2), _)
    | nub (sort ns1) == nub (sort ns2) = []
constructError (PartiallySymmetric _ nsRequired) (actual, nonSymReasons) =
    let relevantNs = case actual of
                    Right (PartiallySymmetric _ nsActual) -> nub (sort nsActual) \\ nub (sort nsRequired)
                    Left _ -> nub (sort nsRequired)
        isRelevant (ExpContainsConstant _ n) = n `elem` relevantNs
        isRelevant (PatContainsConstant _ n) = n `elem` relevantNs
        isRelevant (DependsOnNonSymmetricItem _ r) = isRelevant r
        isRelevant (BuiltinNonSymmetric _) = True
        isRelevant (InstantiatesNonSymmetricPolymorphicItem _ _ _ _) = True
    in filter isRelevant nonSymReasons

checkIsSymmetric :: (PrettyPrintable a, SymmetriesDectectable (Annotated b a)) =>
    Annotated b a -> [SymmetryType] -> SymmetryMonad ()
checkIsSymmetric val reqTypes = do
    symmetryTypes <- listSymmetricTypes' val
    let 
        typeFor (Left t) = t
        typeFor (Right t) = symmetryTypeType t

        symmetriesByType = M.fromList [(typeFor t, r) | r@(t, _) <- symmetryTypes]
        allErrors = concatMap (\ req ->
            case M.lookup (symmetryTypeType req) symmetriesByType of
                Just actual ->
                    map (explainNonSymmetryReason val (explanationType val) req)
                        (constructError req actual)
                Nothing -> panic "Could not find error"
            ) reqTypes

    case allErrors of
        [] -> return ()
        _ -> throwSourceError allErrors

-- | Lists any types in which the given value is at least partially symmetric.
listSymmetricTypes :: SymmetriesDectectable a =>
    a -> SymmetryMonad [SymmetryType]
listSymmetricTypes val = do
    reasons <- listSymmetricTypes' val
    return [t | (Right t, _) <- reasons]

class (Dependencies a, FindDeclarations a, PrettyPrintable a) =>
        SymmetriesDectectable a where
    analyseSymmetries :: a -> SymmetryMonad ()
    explanationType :: a -> String
instance SymmetriesDectectable TCAssertion where
    analyseSymmetries assert = do
        mds <- registerDeclarations (findDeclarations assert)
        mapM_ (\ (a, b) -> analyseDecl a b) mds
        analyseAssertion assert
    explanationType _ = "The assertion"
instance SymmetriesDectectable TCCSPMFile where
    analyseSymmetries file = analyseFile file
    explanationType _ = "The file"
instance SymmetriesDectectable TCExp where
    analyseSymmetries expr = do
        mds <- registerDeclarations (findDeclarations expr)
        mapM_ (\ (a, b) -> analyseDecl a b) mds
        analyseExp expr
    explanationType _ = "The expression"
instance PrettyPrintable [Exp Name] where
    prettyPrint es = list (map prettyPrint es)
instance SymmetriesDectectable [TCExp] where
    analyseSymmetries as = mapM_ analyseSymmetries as
    explanationType _ = "The expressions"
instance SymmetriesDectectable TCInteractiveStmt where
    analyseSymmetries (An _ _ (Evaluate e)) = analyseSymmetries e
    analyseSymmetries (An _ _ (Bind ds)) = do
        mds <- registerDeclarations (findDeclarations ds)
        mapM_ (\ (a, b) -> analyseDecl a b) mds
    analyseSymmetries (An _ _ (RunAssertion a)) = do
        mds <- registerDeclarations (findDeclarations a)
        mapM_ (\ (a, b) -> analyseDecl a b) mds

    explanationType (An _ _ (Evaluate e)) = explanationType e
    explanationType (An _ _ (Bind ds)) = "The declarations"
    explanationType (An _ _ (RunAssertion a)) = "The assertion"

-- | Checks a symmetry assertion, as specified in the AST. The assertion
-- consists of a list of pairs (t, <c_1, ..., c_n>) which means that the
-- expression must be symmetric over diff(t, {c_1, ..., c_n}).
checkSymmetryAssertion :: TCExp -> [TCSymmetrySpecification] -> SymmetryMonad ()
checkSymmetryAssertion val spec =
    checkIsSymmetric val (map (\ spec ->
        case unAnnotate spec of
            StandardSymmetryGroup n [] -> FullySymmetric (TDatatype n)
            StandardSymmetryGroup n ns -> PartiallySymmetric (TDatatype n) ns) spec)

createSymmetryTrackerFunction :: SymmetryMonad (Maybe Name -> [(Name, Type)] -> [Name])
createSymmetryTrackerFunction = do
    st <- gets id
    return $! \ _ nts -> map fst nts

allPossiblySymmetricTypes :: SymmetryMonad [(Type, [Name])]
allPossiblySymmetricTypes = allRegisteredTypes
