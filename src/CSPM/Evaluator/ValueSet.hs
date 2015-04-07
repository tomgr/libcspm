-- | Provides a set implementation for machine CSP sets. This relies heavily
-- on the type checking and assumes in many places that the sets being operated
-- on are suitable for the opertion in question.
--
-- We cannot just use the built in set implementation as FDR assumes in several
-- places that infinite sets are allowed.
module CSPM.Evaluator.ValueSet (
    -- * Construction
    ValueSet(..),
    emptySet, fromList, toList, toSeq, singleton,
    -- * Basic Functions
    compareValueSets,
    member, card, empty,
    union, unions, infiniteUnions,
    intersection, intersections,
    difference,
    -- * Derived functions
    CartProductType(..), cartesianProduct, powerset, allSequences, allMaps,
    fastUnDotCartProduct,
    -- * Specialised Functions
    singletonValue,
    valueSetToEventSet,
    unDotProduct,
)
where

import Control.Monad
import qualified Data.Foldable as F
import Data.Hashable
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq
import qualified Data.Traversable as T

import {-# SOURCE #-} CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Values
--import qualified CSPM.Evaluator.ProcessValues as CE
import Util.Exception
import qualified Util.List as UL
import Util.Prelude

data CartProductType = CartDot | CartTuple deriving (Eq, Ord)

data ValueSet = 
    -- | Set of all integers
    Integers
    -- | Set of all processes
    | Processes
    -- | An explicit set of values
    | ExplicitSet (S.Set Value)
    -- | The infinite set of integers starting at lb.
    | IntSetFrom Int
    -- | A union of several sets.
    | CompositeSet (Sq.Seq ValueSet)
    -- | A set containing all sequences over the given set.
    | AllSequences ValueSet
    -- | A cartesian product of several sets.
    | CartesianProduct [ValueSet] CartProductType
    -- | The powerset of the given set
    | Powerset ValueSet
    -- | The set of all maps from the given domain to the given image.
    | AllMaps ValueSet ValueSet

instance Hashable ValueSet where
    hashWithSalt s Integers = s `hashWithSalt` (1 :: Int)
    hashWithSalt s Processes = s `hashWithSalt` (2 :: Int)
    hashWithSalt s (IntSetFrom lb) = s `hashWithSalt` (3 :: Int) `hashWithSalt` lb
    hashWithSalt s (AllSequences vs) = s `hashWithSalt` (4 :: Int) `hashWithSalt` vs
    -- All the above are the ONLY possible representations of the sets (as the
    -- sets are infinite). However, other sets can be represented in multiple
    -- ways so we have to normalise to an explicit set, essentially. 
    -- This is already guaranteed to be sorted
    hashWithSalt s (ExplicitSet vs) = s `hashWithSalt` (5 :: Int) `hashWithSalt` (S.toList vs)
    hashWithSalt s set = s `hashWithSalt` (sort $ toList set)

instance Eq ValueSet where
    s1 == s2 = compare s1 s2 == EQ
instance Ord ValueSet where
    -- Basic (possibly)-infinite cases
    compare Integers Integers = EQ
    compare Integers _ = GT
    compare _ Integers = LT
    compare Processes Processes = EQ
    compare Processes _ = GT
    compare _ Processes = LT
    compare (IntSetFrom lb1) (IntSetFrom lb2) = compare lb1 lb2
    compare (CartesianProduct vs1 cp1) (CartesianProduct vs2 cp2) =
        compare cp1 cp2 `thenCmp` compare vs1 vs2
    -- Mixed infinite cases

    -- Explicitly finite cases
    compare (CompositeSet s1) (CompositeSet s2) = compare s1 s2
    compare (AllSequences s1) (AllSequences s2) = compare s1 s2
    compare (ExplicitSet s1) (ExplicitSet s2) = compare s1 s2
    compare (Powerset s1) (Powerset s2) = compare s1 s2
    compare (AllMaps k1 v1) (AllMaps k2 v2) =
        compare k1 k2 `thenCmp` compare v1 v2
    -- Fallback to comparing the lists
    compare s1 s2 = compare (sort $ toList s1) (sort $ toList s2)

flipOrder :: Maybe Ordering -> Maybe Ordering
flipOrder Nothing = Nothing
flipOrder (Just EQ) = Just EQ
flipOrder (Just LT) = Just GT
flipOrder (Just GT) = Just LT

-- | Compares two value sets using subseteq (as per the specification).
compareValueSets :: ValueSet -> ValueSet -> Maybe Ordering
-- Processes
compareValueSets Processes Processes = Just EQ
compareValueSets _ Processes = Just LT
compareValueSets Processes _ = Just GT
-- Integers
compareValueSets Integers Integers = Just EQ
compareValueSets _ Integers = Just LT
compareValueSets Integers _ = Just GT
-- IntSetFrom
compareValueSets (IntSetFrom lb1) (IntSetFrom lb2) = Just (compare lb1 lb2)
-- ExplicitSet
compareValueSets (ExplicitSet s1) (ExplicitSet s2) =
    if s1 == s2 then Just EQ
    else if S.isProperSubsetOf s1 s2 then Just LT
    else if S.isProperSubsetOf s2 s1 then Just GT
    else Nothing
-- IntSetFrom+ExplicitSet
compareValueSets (IntSetFrom lb1) (ExplicitSet s2) =
    let 
        VInt lb2 = S.findMin s2
        VInt ub2 = S.findMax s2
    in if lb1 <= lb2 then Just GT else Nothing
compareValueSets (ExplicitSet s1) (IntSetFrom lb1) =
    flipOrder (compareValueSets (IntSetFrom lb1) (ExplicitSet s1))
-- Composite Sets
compareValueSets (CompositeSet ss) s =
    compareValueSets (fromList (toList (CompositeSet ss))) s
compareValueSets s (CompositeSet ss) = 
    flipOrder (compareValueSets (CompositeSet ss) s)
compareValueSets s1 s2 | empty s1 && empty s2 = Just EQ
compareValueSets s1 s2 | empty s1 && not (empty s2) = Just LT
compareValueSets s1 s2 | not (empty s1) && empty s2 = Just GT
-- AllSequences+ExplicitSet sets
compareValueSets (AllSequences vs1) (AllSequences vs2) = compareValueSets vs1 vs2
compareValueSets (ExplicitSet vs) (AllSequences vss) =
    if and (map (flip member (AllSequences vss)) (S.toList vs)) then Just LT
    -- Otherwise, there is some item in vs that is not in vss. However, unless
    -- vss is empty there must be some value in the second set that is not in
    -- the first, since (AllSequences vss) is infinite and, if the above
    -- finishes, we know the first set is finite. Hence, they would be
    -- incomparable.
    else Nothing
compareValueSets (AllSequences vss) (ExplicitSet vs) =
    flipOrder (compareValueSets (ExplicitSet vs) (AllSequences vss))
-- CartesianProduct+ExplicitSet sets
compareValueSets (CartesianProduct vss1 vc1) (CartesianProduct vss2 vc2) | vc1 == vc2 =
    let os = zipWith compareValueSets vss1 vss2
        order v [] = v
        order (Just LT) (Just x : xs) | x /= GT = order (Just LT) xs
        order (Just EQ) (Just x : xs) = order (Just x) xs
        order (Just GT) (Just x : xs) | x /= LT = order (Just GT) xs
        order _ _ = Nothing
    in order (head os) os
compareValueSets (ExplicitSet vs) (CartesianProduct vsets vc) =
    compareValueSets (ExplicitSet vs) (fromList (toList (CartesianProduct vsets vc)))
compareValueSets (CartesianProduct vsets vc) (ExplicitSet vs) =
    flipOrder (compareValueSets (ExplicitSet vs) (CartesianProduct vsets vc))
compareValueSets (Powerset vs1) (Powerset vs2) = compareValueSets vs1 vs2
compareValueSets s1 (Powerset s2) =
    compareValueSets s1 (fromList (toList (Powerset s2)))
compareValueSets (Powerset s1) s2 = flipOrder (compareValueSets s2 (Powerset s1))
compareValueSets s1 (AllMaps ks vs) =
    compareValueSets s1 (fromList (toList (AllMaps ks vs)))
compareValueSets (AllMaps ks vs) s2 = 
    flipOrder (compareValueSets s2 (AllMaps ks vs))
-- Anything else is incomparable
compareValueSets _ _ = Nothing

-- | Produces a ValueSet of the carteisan product of several ValueSets, 
-- using 'vc' to convert each sequence of values into a single value.
cartesianProduct :: CartProductType -> [ValueSet] -> ValueSet
cartesianProduct vc vss = CartesianProduct vss vc

powerset :: ValueSet -> ValueSet
powerset = Powerset

-- | Returns the set of all sequences over the input set. This is infinite
-- so we use a CompositeSet.
allSequences :: ValueSet -> ValueSet
allSequences s = AllSequences s

allMaps :: ValueSet -> ValueSet -> ValueSet
allMaps ks vs = AllMaps ks vs

-- | The empty set
emptySet :: ValueSet
emptySet = ExplicitSet S.empty

-- | Converts a list to a set
fromList :: [Value] -> ValueSet
fromList = ExplicitSet . S.fromList

-- | Constructs a singleton set
singleton :: Value -> ValueSet
singleton = ExplicitSet . S.singleton

-- | Converts a set to list.
toList :: ValueSet -> [Value]
toList (ExplicitSet s) = S.toList s
toList (IntSetFrom lb) = map VInt [lb..]
toList (CompositeSet ss) = F.msum (fmap toList ss)
toList Integers =
    let merge (x:xs) ys = x:merge ys xs in map VInt $ merge [0..] [(-1),(-2)..]
toList Processes = throwSourceError [cannotConvertProcessesToListMessage]
toList (AllSequences vs) =
    if empty vs then [] else 
    let 
        itemsAsList :: [Value]
        itemsAsList = toList vs

        list :: Integer -> [Value]
        list 0 = [VList []]
        list n = concatMap (\x -> map (app x) (list (n-1)))  itemsAsList
            where
                app :: Value -> Value -> Value
                app x (VList xs) = VList $ x:xs
        
        allSequences :: [Value]
        allSequences = concatMap list [0..]
    in allSequences
toList (CartesianProduct vss ct) =
    let cp = UL.cartesianProduct (map toList vss)
    in case ct of
            CartTuple -> map tupleFromList cp
            CartDot -> map VDot cp
toList (Powerset vs) =
    (map (VSet . fromList) . filterM (\x -> [True, False]) . toList) vs
toList (AllMaps ks' vs') =
    let ks = toList (Powerset ks')
        vs = toList vs'
        -- | Creates all maps that have the given set as its domain
        makeMaps :: [Value] -> [Value]
        makeMaps [] = [VMap $ M.empty]
        makeMaps (k:ks) =
            map (\ [VMap m, v] -> VMap $ M.insert k v m)
                (UL.cartesianProduct [makeMaps ks, vs])
    in concatMap (\ (VSet s) -> makeMaps (toList s)) ks

toSeq :: ValueSet -> Sq.Seq Value
toSeq (ExplicitSet s) = F.foldMap Sq.singleton s
toSeq (IntSetFrom lb) = fmap VInt (Sq.fromList [lb..])
toSeq (CompositeSet ss) = F.msum (fmap toSeq ss)
toSeq (AllSequences vs) = Sq.fromList (toList (AllSequences vs))
toSeq (CartesianProduct vss ct) = Sq.fromList (toList (CartesianProduct vss ct))
toSeq (Powerset vs) = Sq.fromList (toList (Powerset vs))
toSeq (AllMaps ks vs) = Sq.fromList (toList (AllMaps ks vs))
toSeq Integers = throwSourceError [cannotConvertIntegersToListMessage]
toSeq Processes = throwSourceError [cannotConvertProcessesToListMessage]

-- | Returns the value iff the set contains one item only.
singletonValue :: ValueSet -> Maybe Value
singletonValue s = case toList s of
                            [x] -> Just x
                            _ -> Nothing

-- | Is the specified value a member of the set.
member :: Value -> ValueSet -> Bool
member v (ExplicitSet s) = S.member v s
member (VInt i) Integers = True
member (VInt i) (IntSetFrom lb) = i >= lb
member v (CompositeSet ss) = F.or (fmap (member v) ss)
-- FDR does actually try and given the correct value here.
member (VProc p) Processes = True
member (VList vs) (AllSequences s) = and (map (flip member s) vs)
member (VDot vs) (CartesianProduct vss CartDot) = and (zipWith member vs vss)
member (VTuple vs) (CartesianProduct vss CartTuple) =
    and (zipWith member (elems vs) vss)
member (VSet s) (Powerset vs) = and (map (\v -> member v vs) (toList s))
member (VMap m) (AllMaps ks vs) =
    and (map (flip member ks) (map fst (M.toList m)))
    && and (map (flip member vs) (map snd (M.toList m)))
member v s1 = throwSourceError [cannotCheckSetMembershipError v s1]

-- | The cardinality of the set. Throws an error if the set is infinite.
card :: ValueSet -> Integer
card (ExplicitSet s) = toInteger (S.size s)
card (CompositeSet ss) = F.sum (fmap card ss)
card (CartesianProduct vss _) = product (map card vss)
card (Powerset s) = 2^(card s)
card (AllMaps ks vs) =
    -- For each key, we can either not map it to anything, or map it to one
    -- of the values
    (card vs + 1) ^ (card ks)
card s = throwSourceError [cardOfInfiniteSetMessage s]

-- | Is the specified set empty?
empty :: ValueSet -> Bool
empty (ExplicitSet s) = S.null s
empty (CompositeSet ss) = F.and (fmap empty ss)
empty (IntSetFrom lb) = False
empty (AllSequences s) = empty s
empty (CartesianProduct vss _) = or (map empty vss)
empty Processes = False
empty Integers = False
empty (Powerset s) = False
empty (AllMaps ks vs) = False

-- | Replicated union.
unions :: [ValueSet] -> ValueSet
unions [] = emptySet
unions vs = foldr1 union vs

infiniteUnions :: [ValueSet] -> ValueSet
infiniteUnions [vs] = vs
infiniteUnions vs =
    let extract (CompositeSet s) = s
        extract s = Sq.singleton s
    in CompositeSet (F.msum (map extract vs))

-- | Replicated intersection.
intersections :: [ValueSet] -> ValueSet
intersections [] = emptySet
intersections (v:vs) = foldr1 intersection vs

-- | Union two sets throwing an error if it cannot be done in a way that will
-- terminate.
union :: ValueSet -> ValueSet -> ValueSet
-- Process unions
union _ Processes = Processes
union Processes _ = Processes
-- Integer unions
union (IntSetFrom lb1) (IntSetFrom lb2) = IntSetFrom (min lb1 lb2)
union _ Integers = Integers
union Integers _ = Integers
-- Explicit unions
union (ExplicitSet s1) (ExplicitSet s2) = ExplicitSet (S.union s1 s2)
union (CompositeSet s1) (CompositeSet s2) = CompositeSet (s1 Sq.>< s2)
union (CompositeSet s1) s2 = CompositeSet (s2 Sq.<| s1)
union s1 (CompositeSet s2) = CompositeSet (s1 Sq.<| s2)
union (IntSetFrom lb) s = CompositeSet (Sq.fromList [IntSetFrom lb, s])
union s (IntSetFrom lb) = CompositeSet (Sq.fromList [IntSetFrom lb, s])
union (AllSequences s1) s2 = CompositeSet (Sq.fromList [AllSequences s1, s2])
union s2 (AllSequences s1) = CompositeSet (Sq.fromList [AllSequences s1, s2])
union (AllMaps k v) s2 = CompositeSet (Sq.fromList [AllMaps k v, s2])
union s2 (AllMaps k v) = CompositeSet (Sq.fromList [AllMaps k v, s2])
-- Otherwise, we force to an explicit set (this has better performance than
-- always forming an explicit set)
union s1 s2 = ExplicitSet $ S.union (S.fromList (toList s1)) (S.fromList (toList s2))

-- | Intersects two sets throwing an error if it cannot be done in a way that 
-- will terminate.
intersection :: ValueSet -> ValueSet -> ValueSet
-- Explicit intersections
intersection (ExplicitSet s1) (ExplicitSet s2) =
    ExplicitSet (S.intersection s1 s2)
-- Integer intersections
intersection (IntSetFrom lb1) (IntSetFrom lb2) = IntSetFrom (max lb1 lb2)
intersection Integers Integers = Integers
intersection x Integers = x
intersection Integers x = x
-- Integer+ExplicitSet
intersection (ExplicitSet s1) (IntSetFrom lb1) =
    let
        VInt ubs = S.findMax s1
        VInt lbs = S.findMin s1
    in if lbs >= lb1 then ExplicitSet s1
    else ExplicitSet (S.intersection (S.fromList (map VInt [lbs..ubs])) s1)
intersection (IntSetFrom lb1) (ExplicitSet s2) =
    intersection (ExplicitSet s2) (IntSetFrom lb1)
-- Process intersection
intersection Processes Processes = Processes
intersection Processes x = x
intersection x Processes = x
-- Composite Sets
intersection (CompositeSet ss) s =
    CompositeSet (fmap (intersection s) ss)
intersection s (CompositeSet ss) =
    CompositeSet (fmap (intersection s) ss)
-- Cartesian product and all sequences - here we simpy convert to a list and
-- compare.
intersection (CartesianProduct vss vc) s =
    intersection (fromList (toList (CartesianProduct vss vc))) s
intersection s (CartesianProduct vss vc) =
    intersection (fromList (toList (CartesianProduct vss vc))) s
intersection (AllSequences vs) s =
    intersection (fromList (toList (AllSequences vs))) s
intersection s (AllSequences vs) =
    intersection (fromList (toList (AllSequences vs))) s
intersection (AllMaps ks vs) s =
    intersection (fromList (toList (AllMaps ks vs))) s
intersection s (AllMaps ks vs) =
    intersection (fromList (toList (AllMaps ks vs))) s
intersection (Powerset s1) (Powerset s2) = Powerset (intersection s1 s2)
intersection (Powerset s1) s2 = intersection (fromList (toList (Powerset s1))) s2
intersection s2 (Powerset s1) = intersection (fromList (toList (Powerset s1))) s2

difference :: ValueSet -> ValueSet -> ValueSet
difference (ExplicitSet s1) (ExplicitSet s2) = ExplicitSet (S.difference s1 s2)
difference (IntSetFrom lb1) (IntSetFrom lb2) = fromList (map VInt [lb1..(lb2-1)])
difference _ Integers = ExplicitSet S.empty
difference (IntSetFrom lb1) (ExplicitSet s1) =
    let
        VInt ubs = S.findMax s1
        VInt lbs = S.findMin s1
        card = S.size s1
        rangeSize = 1+(ubs-lbs)
        s1' = IntSetFrom lb1
        s2' = ExplicitSet s1
    in if fromIntegral rangeSize == card then
            -- is contiguous
            if lb1 == lbs then IntSetFrom (ubs+1)
            else throwSourceError [cannotDifferenceSetsMessage s1' s2']
        else
            -- is not contiguous
            throwSourceError [cannotDifferenceSetsMessage s1' s2']
difference (ExplicitSet s1) (IntSetFrom lb1) =
    ExplicitSet (S.fromList [VInt i | VInt i <- S.toList s1, i < lb1])
difference (CompositeSet ss) s =
    CompositeSet (fmap (\s1 -> difference s1 s) ss)
difference s (CompositeSet ss) = F.foldl difference s ss
difference s1 s2 = difference (fromList (toList s1)) (fromList (toList s2))

valueSetToEventSet :: ValueSet -> EventSet
valueSetToEventSet = eventSetFromList . map valueEventToEvent . toList

-- | Attempts to decompose the set into a cartesian product, returning Nothing
-- if it cannot.
unDotProduct :: ValueSet -> Maybe [ValueSet]
unDotProduct (CartesianProduct vs CartTuple) = return [CartesianProduct vs CartTuple]
unDotProduct (CartesianProduct (s1:ss) CartDot) =
    -- This is reducible only if this set doesn't represent a set of datatype/
    -- channel items.  If this is the case recursively express as a CartDot
    -- though
    case singletonValue s1 of
        Just (VDataType _) -> return [CartesianProduct (s1:ss) CartDot]
        Just (VChannel _) -> return [CartesianProduct (s1:ss) CartDot]
        _ -> return (s1:ss)
unDotProduct (AllSequences vs) = return [AllSequences vs]
unDotProduct (AllMaps ks vs) = return [AllMaps ks vs]
unDotProduct (IntSetFrom i1) = return [IntSetFrom i1]
unDotProduct (Powerset s) = return [Powerset s]
unDotProduct Integers = return [Integers]
unDotProduct Processes = return [Processes]
unDotProduct (CompositeSet ss) = do
    vs <- T.mapM unDotProduct ss
    let ok [_] = True
        ok _ = False
        ex [x] = x
    if F.and (fmap ok vs) then Just [CompositeSet (fmap ex vs)]
    else Nothing
unDotProduct (ExplicitSet s) | S.null s = return [ExplicitSet s]
unDotProduct (ExplicitSet s) =
    case head (S.toList s) of
        VDot _ -> Nothing
        _ -> Just [ExplicitSet s]

fastUnDotCartProduct :: ValueSet -> Value -> Maybe [ValueSet]
fastUnDotCartProduct (CartesianProduct s CartDot) _ = Just s
fastUnDotCartProduct (CompositeSet ss) (VDot (h:vs)) =
    let sq = Sq.filter isInteresting ss
        isInteresting (CartesianProduct (hset:_) CartDot) =
            case singletonValue hset of
                Just h' | h == h' -> True
                _ -> False
        isInteresting _ = False
    in if Sq.null sq then Nothing
        else case Sq.index sq 0 of
                CartesianProduct s CartDot -> Just s
                _ -> Nothing
fastUnDotCartProduct _ _ = Nothing
