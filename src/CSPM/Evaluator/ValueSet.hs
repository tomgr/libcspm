module CSPM.Evaluator.ValueSet where

import Control.Monad
import qualified Data.Set as S

import CSPM.Evaluator.Values
import qualified CSPM.Compiler.Events as CE
import qualified CSPM.Compiler.Set as CS
import Util.Exception
import Util.PrettyPrint hiding (empty)

data ValueSet = 
    Integers -- Set of all integers
    | Processes -- Set of all processes
    | ExplicitSet (S.Set Value)
    | IntSetFrom Integer -- {lb..}
    | RangedSet Integer Integer -- {lb..ub}
    | LazySet [Value]

instance Eq ValueSet where
    Integers == Integers = True
    Processes == Processes = True
    IntSetFrom lb1 == IntSetFrom lb2 = lb1 == lb2
    RangedSet lb1 ub1 == RangedSet lb2 ub2 = lb1 == lb2 && ub1 == ub2
    ExplicitSet s1 == ExplicitSet s2 = s1 == s2

    ExplicitSet s == RangedSet lb ub = s == S.fromList (map VInt [lb..ub])
    RangedSet lb ub == ExplicitSet s = s == S.fromList (map VInt [lb..ub])
    -- TODO: complete
    
    _ == _ = panic "Cannot compare sets"
    
instance Ord ValueSet where
    compare (ExplicitSet s1) (ExplicitSet s2) = compare s1 s2
    -- TODO: complete

instance PrettyPrintable ValueSet where
    prettyPrint Integers = text "Integers"
    prettyPrint Processes = text "Proc"
    prettyPrint (IntSetFrom lb) = braces (integer lb <> text "...")
    prettyPrint (RangedSet lb ub) = 
        braces (integer lb <> text "..." <> integer ub)
    prettyPrint (ExplicitSet s) =
        braces (list (map prettyPrint $ S.toList s))
    prettyPrint (LazySet vs) =
        braces (list (map prettyPrint vs))
    -- TODO: complete
    
instance Show ValueSet where
    show = show . prettyPrint

-- | Produces a ValueSet of the carteisan product of several ValueSets, 
-- using 'vc' to convert each sequence of values into a single value.
cartesianProduct :: ([Value] -> Value) -> [ValueSet] -> ValueSet
cartesianProduct vc = fromList . map vc . sequence . map toList

-- | Returns the powerset of a ValueSet. This requires
powerset :: ValueSet -> ValueSet
powerset = fromList . map (VSet . fromList) . 
            filterM (\x -> [True, False]) . toList

-- | Returns the set of all sequences over the input set
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
    in LazySet (list 0)

-- | The empty set
emptySet :: ValueSet
emptySet = ExplicitSet S.empty

-- | Converts a list to a set
fromList :: [Value] -> ValueSet
fromList = ExplicitSet . S.fromList

-- | Converts a set to list.
toList :: ValueSet -> [Value]
toList (ExplicitSet s) = S.toList s
toList (RangedSet lb ub) = map VInt [lb..ub]
toList (IntSetFrom lb) = map VInt [lb..]
toList (LazySet xs) = xs
-- TODO: rest

-- | Returns the value iff the set contains one item only
singletonValue :: ValueSet -> Maybe Value
singletonValue s = if card s == 1 then Just (head (toList s)) else Nothing

member :: Value -> ValueSet -> Bool
member v (ExplicitSet s) = S.member v s
member (VInt i) Integers = True
member (VInt i) (IntSetFrom lb) = i >= lb
member (VInt i) (RangedSet lb ub) = i >= lb && i <= ub
member v (LazySet xs) = v `elem` xs

card :: ValueSet -> Integer
card (ExplicitSet s) = toInteger (S.size s)
card (RangedSet lb ub) = ub-lb+1

empty :: ValueSet -> Bool
empty Processes = panic "empty(Processes) Not implemented"
empty (ExplicitSet s) = S.null s
empty (IntSetFrom lb) = False
empty (Integers) = False
empty (RangedSet lb ub) = lb > ub
empty (LazySet xs) =
    case xs of
        x:_ -> False
        _   -> True

mapMonotonic :: (Value -> Value) -> ValueSet -> ValueSet
mapMonotonic f (ExplicitSet s) = ExplicitSet $ S.mapMonotonic f s

unions :: [ValueSet] -> ValueSet
unions vs = foldr union (ExplicitSet S.empty) vs

intersections :: [ValueSet] -> ValueSet
intersections vs = panic "Unions not implemented"

union :: ValueSet -> ValueSet -> ValueSet
union (ExplicitSet s1) (ExplicitSet s2) =
     ExplicitSet (S.union s1 s2)
union (IntSetFrom lb1) (IntSetFrom lb2) = 
     IntSetFrom (min lb1 lb2)
union (IntSetFrom lb) Integers =Integers
union Integers (IntSetFrom lb) =Integers
union (IntSetFrom lb1) (RangedSet lb2 ub2) | lb1 <= ub2 =
     IntSetFrom (min lb1 lb2)
union (RangedSet lb2 ub2) (IntSetFrom lb1) | lb1 <= ub2 =
     IntSetFrom (min lb1 lb2)
union x y | x == y =  x
-- TODO: complete

intersection :: ValueSet -> ValueSet -> ValueSet
intersection (ExplicitSet s1) (ExplicitSet s2) =
     ExplicitSet (S.intersection s1 s2)
intersection (IntSetFrom lb1) (IntSetFrom lb2) = 
     IntSetFrom (min lb1 lb2)
intersection (IntSetFrom lb) Integers =IntSetFrom lb
intersection Integers (IntSetFrom lb) =IntSetFrom lb
intersection (IntSetFrom lb1) (RangedSet lb2 ub2) =
    if lb1 <= ub2 then RangedSet (max lb2 lb1) ub2
    else ExplicitSet S.empty
intersection (RangedSet lb2 ub2) (IntSetFrom lb1) | lb1 <= ub2 =
    intersection (IntSetFrom lb1) (RangedSet lb2 ub2)
inter x y | x == y =  x
-- TODO: complete

difference :: ValueSet -> ValueSet -> ValueSet
difference (ExplicitSet s1) (ExplicitSet s2) =
     ExplicitSet (S.difference s1 s2)
difference (IntSetFrom lb1) (IntSetFrom lb2) = 
    if lb1 < lb2 then RangedSet lb1 (lb2-1)
    else ExplicitSet S.empty
difference (IntSetFrom lb) Integers = ExplicitSet S.empty
--difference Integers (IntSetFrom lb) = 
--   InfSetTo (lb-1)
--difference (IntSetFrom lb1) (RangedSet lb2 ub2) =
--  if lb1 <= ub2 thenRangedSet (max lb2 lb1) ub2
--  elseExplicitSet empty
--difference (RangedSet lb2 ub2) (IntSetFrom lb1) | lb1 <= ub2 =
difference x y | x == y = ExplicitSet S.empty 
-- TODO: complete
-- TODO: maybe remove rangedset

valueSetToEventSet :: ValueSet -> CS.Set CE.Event
valueSetToEventSet = CS.fromList . map valueEventToEvent . toList
