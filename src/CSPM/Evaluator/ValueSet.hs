-- | Provides a set implementation for machine CSP sets. This relies heavily
-- on the type checking and assumes in many places that the sets being operated
-- on are suitable for the opertion in question.
--
-- We cannot just use the built in set implementation as FDR assumes in several
-- places that infinite sets are allowed.
module CSPM.Evaluator.ValueSet (
    -- * Construction
    ValueSet(Integers, Processes, IntSetFrom),
    emptySet, fromList, toList,
    -- * Basic Functions
    compareValueSets,
    member, card, empty,
    union, unions,
    intersection, intersections,
    difference,
    -- * Derived functions
    cartesianProduct, powerset, allSequences,
    -- * Specialised Functions
    singletonValue,
    valueSetToEventSet
)
where

import Control.Monad
import qualified Data.Set as S

import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Values
import qualified CSPM.Compiler.Events as CE
import qualified CSPM.Compiler.Set as CS
import Util.Exception
import Util.PrettyPrint hiding (empty)

data ValueSet = 
    -- | Set of all integers
    Integers
    -- | Set of all processes
    | Processes
    -- | An explicit set of values
    | ExplicitSet (S.Set Value)
    -- | The infinite set of integers starting at lb.
    | IntSetFrom Int -- {lb..}
    -- | A set of two value sets. Note that the tree of sets may be infinite.
    -- NB. Composite sets are always infinite.
    | CompositeSet ValueSet ValueSet
    -- Only used for the internal set representation
    deriving Ord

instance Eq ValueSet where
    s1 == s2 = compareValueSets s1 s2 == Just EQ

instance PrettyPrintable ValueSet where
    prettyPrint Integers = text "Integers"
    prettyPrint Processes = text "Proc"
    prettyPrint (IntSetFrom lb) = braces (int lb <> text "...")
    prettyPrint (ExplicitSet s) =
        braces (list (map prettyPrint $ S.toList s))
    prettyPrint (CompositeSet s1 s2) =
        text "union" <> parens (prettyPrint s1 <> comma <+> prettyPrint s2)
    
instance Show ValueSet where
    show = show . prettyPrint

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
compareValueSets (CompositeSet s1 s2) s =
    case (compareValueSets s1 s, compareValueSets s2 s) of
        (Nothing, _)        -> Nothing
        (_, Nothing)        -> Nothing
        (Just LT, Just x)   -> if x /= GT then Just LT else Nothing
        (Just EQ, Just x)   -> Just x
        (Just GT, Just x)   -> if x /= LT then Just GT else Nothing
compareValueSets s (CompositeSet s1 s2) = 
    flipOrder (compareValueSets (CompositeSet s1 s2) s)

-- Anything else is incomparable
--compareValueSets _ _ = Nothing

-- | Produces a ValueSet of the carteisan product of several ValueSets, 
-- using 'vc' to convert each sequence of values into a single value.
cartesianProduct :: ([Value] -> Value) -> [ValueSet] -> ValueSet
cartesianProduct vc = fromList . map vc . sequence . map toList

-- | Returns the powerset of a ValueSet. This requires
powerset :: ValueSet -> ValueSet
powerset = fromList . map (VSet . fromList) . 
            filterM (\x -> [True, False]) . toList

-- | Returns the set of all sequences over the input set. This is infinite
-- so we use a CompositeSet.
allSequences :: ValueSet -> ValueSet
allSequences s = if empty s then emptySet else 
    let 
        itemsAsList :: [Value]
        itemsAsList = toList s

        list :: Integer -> [Value]
        list 0 = [VList []]
        list n = concatMap (\x -> map (app x) (list (n-1)))  itemsAsList
            where
                app :: Value -> Value -> Value
                app x (VList xs) = VList $ x:xs
        
        yielder :: [Value] -> ValueSet
        yielder (x:xs) = CompositeSet (ExplicitSet (S.singleton x)) (yielder xs)

        allSequences :: [Value]
        allSequences = concatMap list [0..]
    in yielder allSequences

-- | The empty set
emptySet :: ValueSet
emptySet = ExplicitSet S.empty

-- | Converts a list to a set
fromList :: [Value] -> ValueSet
fromList = ExplicitSet . S.fromList

-- | Converts a set to list.
toList :: ValueSet -> [Value]
toList (ExplicitSet s) = S.toList s
toList (IntSetFrom lb) = map VInt [lb..]
toList (CompositeSet s1 s2) = toList s1 ++ toList s2
toList Integers = throwSourceError [cannotConvertIntegersToListMessage]
toList Processes = throwSourceError [cannotConvertProcessesToListMessage]

-- | Returns the value iff the set contains one item only.
singletonValue :: ValueSet -> Maybe Value
singletonValue s =
    let
        isSingleton :: ValueSet -> Bool
        isSingleton (ExplicitSet s) = S.size s == 1
        isSingleton _ = False
    in if isSingleton s then Just (head (toList s)) else Nothing

-- | Is the specified value a member of the set.
member :: Value -> ValueSet -> Bool
member v (ExplicitSet s) = S.member v s
member (VInt i) Integers = True
member (VInt i) (IntSetFrom lb) = i >= lb
member v (CompositeSet s1 s2) = member v s1 || member v s2
-- FDR does actually try and given the correct value here.
member (VProc p) Processes = True
member v s1 = throwSourceError [cannotCheckSetMembershipError v s1]

-- | The cardinality of the set. Throws an error if the set is infinite.
card :: ValueSet -> Integer
card (ExplicitSet s) = toInteger (S.size s)
card s = throwSourceError [cardOfInfiniteSetMessage s]

-- | Is the specified set empty?
empty :: ValueSet -> Bool
empty (ExplicitSet s) = S.null s
empty (CompositeSet s1 s2) = False
empty (IntSetFrom lb) = False
empty Processes = False
empty Integers = False

-- | Replicated union.
unions :: [ValueSet] -> ValueSet
unions vs = foldr union (ExplicitSet S.empty) vs

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
-- Composite unions
union s1 (s2 @ (CompositeSet _ _)) = CompositeSet s1 s2
union (s1 @ (CompositeSet _ _)) s2 = CompositeSet s1 s2
-- Explicit unions
union (ExplicitSet s1) (ExplicitSet s2) = ExplicitSet (S.union s1 s2)
union (ExplicitSet s1) (IntSetFrom lb) =
    CompositeSet (ExplicitSet s1) (IntSetFrom lb)
union (IntSetFrom lb) (ExplicitSet s1) =
    CompositeSet (ExplicitSet s1) (IntSetFrom lb)

-- The above are all the well typed possibilities
--union s1 s2 = throwSourceError [cannotUnionSetsMessage s1 s2]

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
-- Composite sets are always infinite and therefore cannot be intersected in any
-- finite way.
intersection s1 s2 = throwSourceError [cannotIntersectSetsMessage s1 s2]

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
difference s1 s2 = throwSourceError [cannotDifferenceSetsMessage s1 s2]

valueSetToEventSet :: ValueSet -> CS.Set CE.Event
valueSetToEventSet = CS.fromList . map valueEventToEvent . toList
