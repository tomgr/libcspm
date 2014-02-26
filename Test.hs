
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, FlexibleContexts,
    FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses,
    ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module TranslatedCSPM12136363609249758347M1M12944 where

import Control.Exception
import Control.Monad (filterM)
import Data.Typeable
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ
import Unsafe.Coerce


-- * Static Run Time System 

-- | Class for pretty-printing CSPM values.
class CSPM_Show a where
    cspm_show :: a -> Doc
    cspm_showList :: [a] -> Doc
    cspm_showList xs =
        cspm_prettyPrint_angles (cspm_prettyPrint_list (map cspm_show xs))

cspm_prettyPrint_tabIndent :: Doc -> Doc
cspm_prettyPrint_tabIndent = nest 4

-- | Pretty print a list of values.
cspm_prettyPrint_list :: [Doc] -> Doc
cspm_prettyPrint_list xs = fsep (punctuate (text ",") xs)

-- | Surround a document with '<' and '>'.
cspm_prettyPrint_angles :: Doc -> Doc
cspm_prettyPrint_angles doc = char '<' <> doc <> char '>'

cspm_prettyPrint_dotSep :: [Doc] -> Doc
cspm_prettyPrint_dotSep = hcat . punctuate (char '.')

instance CSPM_Show Int where
    cspm_show x = int x
instance CSPM_Show Bool where
    cspm_show True = text "true"
    cspm_show False = text "false"
instance CSPM_Show Char where
    cspm_show x = char x
    cspm_showList xs = text xs
instance CSPM_Show a => CSPM_Show [a] where
    cspm_show xs = cspm_showList xs
instance (CSPM_Show k, CSPM_Show v) => CSPM_Show (M.Map k v) where
    cspm_show m =
        text "(|" <+> cspm_prettyPrint_list (map
            (\ (k, v) -> cspm_show k <+> text "=>" <+> cspm_show v)
            (M.toList m))
        <+> text "|)"
instance CSPM_Show (a -> b) where
    cspm_show _ = text "<function>"

data CSPM_Exception = CSPM_Exception String
     deriving Typeable

instance Show CSPM_Exception where
    show (CSPM_Exception s) = s

instance Exception CSPM_Exception

-- | Throws an error message, with a location and message.
cspm_panic :: Doc -> Doc -> a
cspm_panic location message = throw $ CSPM_Exception $ show $
    hang (location <> colon) 4 message

cspm_user_error :: String -> a
cspm_user_error = throw . CSPM_Exception

-- | Something of type a.b, where a is not a data constructor.
data CSPM_ExplicitDot a b = CSPM_ExplicitDot a b deriving (Eq, Ord, Typeable)

instance (CSPM_Show a, CSPM_Show b) => CSPM_Show (CSPM_ExplicitDot a b) where
    cspm_show (CSPM_ExplicitDot a b) =
        cspm_prettyPrint_dotSep [cspm_show a, cspm_show b]

-- | Indicates that a.b == c.
class CSPM_Dot a b c | a b -> c where
    cspm_dotOn :: a -> b -> c

-- | Given a set of b's such that b = a.c, returns the set of c.
class (CSPM_Set b, CSPM_Set c) => CSPM_DropPrefix a b c | a b -> c where
    cspm_dropPrefix :: a -> CSPM_SetType b -> CSPM_SetType c

-- | Something of type is extentadble using things of type b to things of type
-- c. Further, the first field that this can be extended by is something of type
-- d (i.e. b == d.x where x might be the empty dot list).
class CSPM_Set b => CSPM_Productions a b | a -> b where
    cspm_productions :: a -> CSPM_SetType b

-- | Computes productions of an item of a, whose first field is of type b,
-- using the specified set of values for the first field. If this set includes
-- values that are not permitted by the datatype, these are simply ignored.
class (CSPM_Productions a c, CSPM_Set b, CSPM_Set c) =>
        CSPM_RestrictedProductions a b c | a -> b, a -> c where
    cspm_restricted_productions :: a -> CSPM_SetType b -> CSPM_SetType c

data CSPM_Yield b c d e =
    forall a . (
        CSPM_Dot a b c,
        CSPM_DropPrefix a e d,
        CSPM_RestrictedProductions a b e,
        CSPM_Show a,
        Eq a,
        Ord a,
        Typeable a) => CSPM_Yield a
    deriving Typeable

cspm_extensions :: (CSPM_DropPrefix a b c, CSPM_Productions a b, CSPM_Set c) =>
    a -> CSPM_SetType c
cspm_extensions a = cspm_dropPrefix a $ cspm_productions a

instance CSPM_Dot (CSPM_Yield b c d e) b c where
    cspm_dotOn (CSPM_Yield a) b = cspm_dotOn a b
instance (CSPM_Set c, CSPM_Set d) => CSPM_DropPrefix (CSPM_Yield a b c d) d c where
    cspm_dropPrefix (CSPM_Yield x) = cspm_dropPrefix x
instance CSPM_Set e => CSPM_Productions (CSPM_Yield b c d e) e where
    cspm_productions (CSPM_Yield a) = cspm_productions a
instance (CSPM_Set a, CSPM_Set d) => CSPM_RestrictedProductions (CSPM_Yield a b c d) a d where
    cspm_restricted_productions (CSPM_Yield x) = cspm_restricted_productions x
instance CSPM_Show (CSPM_Yield b c d e) where
    cspm_show (CSPM_Yield a) = cspm_show a
instance Eq (CSPM_Yield b c d e) where
    CSPM_Yield x == CSPM_Yield y | typeOf x == typeOf y = x == unsafeCoerce y
    _ == _ = False
    CSPM_Yield x /= CSPM_Yield y | typeOf x == typeOf y = x /= unsafeCoerce y
    _ /= _ = True
instance Ord (CSPM_Yield b c d e) where
    compare (CSPM_Yield x) (CSPM_Yield y) =
            if tx == ty then compare x (unsafeCoerce y)
            else compare tx ty
        where
            tx = typeOf x
            ty = typeOf y

-- Trivial dot instances
instance CSPM_Dot Int a (CSPM_ExplicitDot Int a) where
    cspm_dotOn x y = CSPM_ExplicitDot x y
instance CSPM_Dot Bool a (CSPM_ExplicitDot Bool a) where
    cspm_dotOn x y = CSPM_ExplicitDot x y
instance CSPM_Dot Char a (CSPM_ExplicitDot Char a) where
    cspm_dotOn x y = CSPM_ExplicitDot x y
instance CSPM_Dot [a] b (CSPM_ExplicitDot [a] b) where
    cspm_dotOn x y = CSPM_ExplicitDot x y
instance CSPM_Set a => CSPM_Dot (CSPM_SetType a) b (CSPM_ExplicitDot (CSPM_SetType a) b) where
    cspm_dotOn x y = CSPM_ExplicitDot x y
instance CSPM_Dot (M.Map k v) a (CSPM_ExplicitDot (M.Map k v) a) where
    cspm_dotOn x y = (CSPM_ExplicitDot x y)
instance (CSPM_Dot b c d, CSPM_Dot a d e) => CSPM_Dot (CSPM_ExplicitDot a b) c e where
    cspm_dotOn (CSPM_ExplicitDot a b) c = cspm_dotOn a (cspm_dotOn b c)
instance CSPM_Dot c f g => CSPM_Dot (CSPM_Yield b c d e) (CSPM_ExplicitDot b f) g where
    cspm_dotOn y (CSPM_ExplicitDot b f) = cspm_dotOn (cspm_dotOn y b) f

instance (
        CSPM_Dot (CSPM_Yield a b c d) y h,
        CSPM_DropPrefix (CSPM_Yield x y z a) c o,
        CSPM_Set a, CSPM_Set c, CSPM_Set d, CSPM_Set x,
        Typeable a, Typeable b, Typeable c, Typeable d, Typeable x, Typeable y,
        Typeable z
    ) =>
        CSPM_Dot (CSPM_Yield a b c d) (CSPM_Yield x y z a) (CSPM_Yield x h o d) where
    cspm_dotOn x y = CSPM_Yield $ CSPM_ExplicitDot x y
instance (CSPM_RestrictedProductions a c e, CSPM_RestrictedProductions b d c)
        => CSPM_RestrictedProductions (CSPM_ExplicitDot a b) d e where
    cspm_restricted_productions (CSPM_ExplicitDot a b) xs =
        cspm_restricted_productions a (cspm_restricted_productions b xs)
instance (CSPM_Productions b c, CSPM_RestrictedProductions a c d) =>
        CSPM_Productions (CSPM_ExplicitDot a b) d where
    cspm_productions (CSPM_ExplicitDot a b) =
        cspm_restricted_productions a $ cspm_productions b

instance (CSPM_DropPrefix a b c, CSPM_Set d, CSPM_Show d) =>
        CSPM_DropPrefix a (CSPM_ExplicitDot b d) (CSPM_ExplicitDot c d) where
    cspm_dropPrefix a (CSPM_ExplicitDotProduct b d) =
        CSPM_ExplicitDotProduct (cspm_dropPrefix a b) d
    cspm_dropPrefix a s =
        CSPM_ExplicitDotProduct
            (cspm_dropPrefix a $ cspm_set_fromList $
                map (\ (CSPM_ExplicitDot b _) -> b) bds)
            (cspm_set_fromList $ map (\ (CSPM_ExplicitDot _ d) -> d) bds)
        where bds = cspm_set_toList s
instance (CSPM_DropPrefix a b c, CSPM_DropPrefix x c z) =>
        CSPM_DropPrefix (CSPM_ExplicitDot a x) b z where
    cspm_dropPrefix (CSPM_ExplicitDot a b) xs = cspm_dropPrefix b $ cspm_dropPrefix a xs

-- | Integer division, with an error message on division by zero.
cspm_div :: Doc -> Int -> Int -> Int
cspm_div loc _ 0 = cspm_panic loc (text "Attempt to divide by zero.")
cspm_div _ x y = x `div` y

-- | Integer modulo, with an error message on mod by zero.
cspm_mod :: Doc -> Int -> Int -> Int
cspm_mod loc _ 0 = cspm_panic loc (text "Attempt to divide by zero.")
cspm_mod _ x y = x `mod` y

-- | Head of list, with message on empty list.
cspm_head :: Doc -> [a] -> a
cspm_head loc [] = cspm_panic loc (text "Attempt to take head of empty list.")
cspm_head _ (x:_) = x

-- | Tail of list, with message on empty list.
cspm_tail :: Doc -> [a] -> [a]
cspm_tail loc [] = cspm_panic loc (text "Attempt to take tail of empty list.")
cspm_tail _ (_:xs) = xs

-- | If the list is non-empty, returns a tuple consisting of the remainder of
-- the list, and the last n elements of the list, repsectively.
cspm_split_last :: Int -> [a] -> Maybe ([a], [a])
cspm_split_last n xs = if len >= n then Just (splitAt (len - n) xs) else Nothing
    where len = length xs

cspm_thenCmp :: Maybe Ordering -> Maybe Ordering -> Maybe Ordering
cspm_thenCmp Nothing _ = Nothing
cspm_thenCmp (Just EQ) x = x
cspm_thenCmp (Just x) _ = Just x

cspm_flipOrder :: Maybe Ordering -> Maybe Ordering
cspm_flipOrder Nothing = Nothing
cspm_flipOrder (Just EQ) = Just EQ
cspm_flipOrder (Just LT) = Just GT
cspm_flipOrder (Just GT) = Just LT

cspm_set_product_order :: [Maybe Ordering] -> Maybe Ordering
cspm_set_product_order os =
    let 
        order v [] = v
        order (Just LT) (Just x : xs) | x /= GT = order (Just LT) xs
        order (Just EQ) (Just x : xs) = order (Just x) xs
        order (Just GT) (Just x : xs) | x /= LT = order (Just GT) xs
        order _ _ = Nothing
    in order (head os) (tail os)

-- | Implements the CSPM type class Ord.
class Eq a => CSPM_Ord a where
    cspm_compare :: a -> a -> Maybe Ordering

instance CSPM_Ord Int where
    cspm_compare i1 i2 = Just (compare i1 i2)

instance CSPM_Ord Char where
    cspm_compare i1 i2 = Just (compare i1 i2)

instance CSPM_Ord a => CSPM_Ord [a] where
    cspm_compare vs1 vs2 =
        let
            -- for lists comparing means comparing prefixes
            cmp [] [] = Just EQ
            cmp [] (y:ys) = Just LT
            cmp (x:xs) [] = Just GT
            cmp (x:xs) (y:ys) | x == y = cmp xs ys
            cmp (x:xs) (y:ys) = 
                -- x != y, hence neither can be a prefix of the other
                Nothing
        in cmp vs1 vs2

instance (Ord k, CSPM_Ord k, CSPM_Ord v) => CSPM_Ord (M.Map k v) where
    cspm_compare m1 m2 =
        let cmp v1 v2 =
                case cspm_compare v1 v2 of
                    Just LT -> True
                    Just EQ -> True
                    _ -> False
        in if m1 == m2 then Just EQ
        else if M.isProperSubmapOfBy cmp m1 m2 then Just LT
        else if M.isProperSubmapOfBy cmp m2 m1 then Just GT
        else Nothing

-- | The route into the run-time system to evaluate a value.
cspm_evaluateExpression :: CSPM_Show a => a -> IO (Bool, String)
cspm_evaluateExpression value =
    catch (do
        let printed = show (cspm_show value)
        if length printed >= 0 then return (True, printed)
            else error "cspm_evaluateExpression"
        ) (\ (CSPM_Exception message) -> return (False, message))

class (CSPM_Ord (CSPM_SetType a), CSPM_Show a, CSPM_Show (CSPM_SetType a), Ord a) =>
        CSPM_Set a where
    data CSPM_SetType a

    cspm_set_empty :: CSPM_SetType a
    cspm_set_card :: CSPM_SetType a -> Int
    cspm_set_fromList :: [a] -> CSPM_SetType a
    cspm_set_diff :: CSPM_SetType a -> CSPM_SetType a -> CSPM_SetType a
    cspm_set_null :: CSPM_SetType a -> Bool
    cspm_set_inter :: CSPM_SetType a -> CSPM_SetType a -> CSPM_SetType a
    cspm_set_member :: a -> CSPM_SetType a -> Bool
    cspm_set_toList :: CSPM_SetType a -> [a]
    cspm_set_union :: CSPM_SetType a -> CSPM_SetType a -> CSPM_SetType a

instance CSPM_Ord (CSPM_SetType a) => Eq (CSPM_SetType a) where
    s1 == s2 = cspm_compare s1 s2 == Just EQ
    s1 /= s2 = cspm_compare s1 s2 /= Just EQ

-- TODO: optimise if possible (but remember it has to be a total ordering that
-- respects the above definition of Eq - this is hard!).
instance (CSPM_Set a, Ord a) => Ord (CSPM_SetType a) where
    compare xs ys = compare (cspm_set_toList xs) (cspm_set_toList ys)

cspm_show_union_set :: CSPM_Set a => [CSPM_SetType a] -> Doc
cspm_show_union_set xs =
    text "Union" <> parens (braces (cspm_prettyPrint_list (map cspm_show xs)))

cspm_set_make_explicit :: CSPM_Set a => CSPM_SetType a -> CSPM_SetType a
cspm_set_make_explicit = cspm_set_fromList . cspm_set_toList

-- | Union many sets.
cspm_set_unions :: CSPM_Set a => [CSPM_SetType a] -> CSPM_SetType a
cspm_set_unions [] = cspm_set_empty
cspm_set_unions xs = foldl1 cspm_set_union xs

-- | Intersect many sets.
cspm_set_inters :: CSPM_Set a => [CSPM_SetType a] -> CSPM_SetType a
cspm_set_inters [] = cspm_set_empty
cspm_set_inters xs = foldl1 cspm_set_inter xs

cspm_set_card_of_infinite_set_message = undefined
cspm_cannot_diff_sets_message = undefined

instance CSPM_Set Int where
    data CSPM_SetType Int =
        CSPM_IntSetFrom Int
        | CSPM_ExplicitIntSet IS.IntSet
        | CSPM_Integers
        | CSPM_IntSetUnion [CSPM_SetType Int]

    cspm_set_card (CSPM_ExplicitIntSet is) = IS.size is
    cspm_set_card (CSPM_IntSetUnion xs) = sum (map cspm_set_card xs)
    cspm_set_card _ = cspm_panic (text "Unknown")
        (text "Cannot take the cardinality of an infinite set")

    cspm_set_diff (CSPM_ExplicitIntSet s1) (CSPM_ExplicitIntSet s2) =
        CSPM_ExplicitIntSet $ IS.difference s1 s2
    cspm_set_diff (CSPM_IntSetFrom lb1) (CSPM_IntSetFrom lb2) =
        cspm_set_fromList [lb1..(lb2-1)]
    cspm_set_diff _ CSPM_Integers = CSPM_ExplicitIntSet IS.empty
    cspm_set_diff (CSPM_IntSetFrom lb1) (CSPM_ExplicitIntSet s1) =
        let
            ubs = IS.findMax s1
            lbs = IS.findMin s1
            card = IS.size s1
            rangeSize = 1+(ubs-lbs)
            s1' = CSPM_IntSetFrom lb1
            s2' = CSPM_ExplicitIntSet s1
        in if fromIntegral rangeSize == card then
                -- is contiguous
                if lb1 == lbs then CSPM_IntSetFrom (ubs+1)
                else cspm_cannot_diff_sets_message
            else
                -- is not contiguous
                cspm_cannot_diff_sets_message
    cspm_set_diff (CSPM_ExplicitIntSet s1) (CSPM_IntSetFrom lb1) =
        CSPM_ExplicitIntSet $ IS.fromList [i | i <- IS.toList s1, i < lb1]
    cspm_set_diff (CSPM_IntSetUnion ss) s =
        CSPM_IntSetUnion $ map (\s1 -> cspm_set_diff s1 s) ss
    cspm_set_diff s (CSPM_IntSetUnion ss) = foldl cspm_set_diff s ss
    cspm_set_diff CSPM_Integers _ = cspm_cannot_diff_sets_message

    cspm_set_empty = CSPM_ExplicitIntSet IS.empty

    cspm_set_fromList = CSPM_ExplicitIntSet . IS.fromList

    cspm_set_inter (CSPM_ExplicitIntSet s1) (CSPM_ExplicitIntSet s2) =
        CSPM_ExplicitIntSet $ IS.intersection s1 s2
    cspm_set_inter (CSPM_IntSetFrom lb1) (CSPM_IntSetFrom lb2) =
        CSPM_IntSetFrom $ lb1 `max` lb2
    cspm_set_inter x CSPM_Integers = x
    cspm_set_inter CSPM_Integers x = x
    cspm_set_inter (CSPM_ExplicitIntSet s1) (CSPM_IntSetFrom lb1) =
        let
            ubs = IS.findMax s1
            lbs = IS.findMin s1
        in if lbs >= lb1 then CSPM_ExplicitIntSet s1
        else CSPM_ExplicitIntSet $ IS.intersection (IS.fromList [lbs..ubs]) s1
    cspm_set_inter (CSPM_IntSetFrom lb1) (CSPM_ExplicitIntSet s2) =
        cspm_set_inter (CSPM_ExplicitIntSet s2) (CSPM_IntSetFrom lb1)
    cspm_set_inter (CSPM_IntSetUnion ss) s = CSPM_IntSetUnion (map (cspm_set_inter s) ss)
    cspm_set_inter s (CSPM_IntSetUnion ss) = CSPM_IntSetUnion (map (cspm_set_inter s) ss)

    cspm_set_member x (CSPM_IntSetFrom lb) = x >= lb
    cspm_set_member x (CSPM_ExplicitIntSet is) = IS.member x is
    cspm_set_member x CSPM_Integers = True
    cspm_set_member x (CSPM_IntSetUnion xs) = or (map (cspm_set_member x) xs)

    cspm_set_null CSPM_Integers = False
    cspm_set_null (CSPM_IntSetFrom _) = False
    cspm_set_null (CSPM_IntSetUnion ss) = and (map cspm_set_null ss)
    cspm_set_null (CSPM_ExplicitIntSet s) = IS.null s

    cspm_set_toList (CSPM_IntSetFrom lb) = [lb..]
    cspm_set_toList (CSPM_ExplicitIntSet is) = IS.toList is
    cspm_set_toList CSPM_Integers = cspm_panic (text "Unknown")
        (text "Cannot convert the set of all integers into a list.")
    cspm_set_toList (CSPM_IntSetUnion xs) = concatMap cspm_set_toList xs

    cspm_set_union CSPM_Integers _ = CSPM_Integers
    cspm_set_union _ CSPM_Integers = CSPM_Integers
    cspm_set_union (CSPM_IntSetUnion xs) (CSPM_IntSetUnion ys) = CSPM_IntSetUnion $ xs ++ ys
    cspm_set_union (CSPM_IntSetUnion xs) s = CSPM_IntSetUnion $ s:xs
    cspm_set_union s (CSPM_IntSetUnion xs) = CSPM_IntSetUnion $ s:xs
    cspm_set_union (CSPM_IntSetFrom lb1) (CSPM_IntSetFrom lb2) =
        CSPM_IntSetFrom $ lb1 `min` lb2
    cspm_set_union (CSPM_ExplicitIntSet is1) (CSPM_ExplicitIntSet is2) =
        CSPM_ExplicitIntSet $ IS.union is1 is2
    cspm_set_union (s1@(CSPM_IntSetFrom _)) (s2@(CSPM_ExplicitIntSet _)) =
        CSPM_IntSetUnion [s1, s2]
    cspm_set_union (s2@(CSPM_ExplicitIntSet _)) (s1@(CSPM_IntSetFrom _)) =
        CSPM_IntSetUnion [s1, s2]

instance CSPM_Show (CSPM_SetType Int) where
    cspm_show (CSPM_IntSetFrom lb) = braces (cspm_show lb <> text "..")
    cspm_show CSPM_Integers = text "Int"
    cspm_show (CSPM_IntSetUnion xs) =
        text "Union" <> parens (braces (cspm_prettyPrint_list (map cspm_show xs)))
    cspm_show xs = braces (cspm_prettyPrint_list (map cspm_show (cspm_set_toList xs)))

instance CSPM_Ord (CSPM_SetType Int) where
    cspm_compare CSPM_Integers CSPM_Integers = Just EQ
    cspm_compare _ CSPM_Integers = Just LT
    cspm_compare CSPM_Integers _ = Just GT
    cspm_compare (CSPM_IntSetFrom lb1) (CSPM_IntSetFrom lb2) = cspm_compare lb1 lb2
    cspm_compare (CSPM_ExplicitIntSet s1) (CSPM_ExplicitIntSet s2) =
        if s1 == s2 then Just EQ
        else if IS.isProperSubsetOf s1 s2 then Just LT
        else if IS.isProperSubsetOf s2 s1 then Just GT
        else Nothing
    cspm_compare (CSPM_IntSetFrom lb1) (CSPM_ExplicitIntSet s2) =
        if lb1 <= IS.findMin s2 then Just GT else Nothing
    cspm_compare (CSPM_ExplicitIntSet s1) (CSPM_IntSetFrom lb1) =
        cspm_flipOrder (cspm_compare (CSPM_IntSetFrom lb1) (CSPM_ExplicitIntSet s1))
    cspm_compare (CSPM_IntSetUnion ss) s =
        cspm_compare (cspm_set_make_explicit (CSPM_IntSetUnion ss)) s
    cspm_compare s (CSPM_IntSetUnion ss) = 
        cspm_compare s (cspm_set_make_explicit (CSPM_IntSetUnion ss))

instance CSPM_Set a => CSPM_Set (CSPM_SetType a) where

    data CSPM_SetType (CSPM_SetType a) =
        CSPM_Powerset (CSPM_SetType a)
        | CSPM_ExplicitSetSet (S.Set (CSPM_SetType a))
        | CSPM_SetSetUnion [CSPM_SetType (CSPM_SetType a)]

    cspm_set_card (CSPM_Powerset s) = 2^(cspm_set_card s)
    cspm_set_card (CSPM_ExplicitSetSet s) = S.size s
    cspm_set_card (CSPM_SetSetUnion ss) = sum (map cspm_set_card ss)

    cspm_set_empty = CSPM_ExplicitSetSet S.empty

    cspm_set_diff (CSPM_ExplicitSetSet s1) (CSPM_ExplicitSetSet s2) =
        CSPM_ExplicitSetSet $ S.difference s1 s2
    cspm_set_diff (CSPM_SetSetUnion ss) s =
        CSPM_SetSetUnion $ map (\s1 -> cspm_set_diff s1 s) ss
    cspm_set_diff s (CSPM_SetSetUnion ss) = foldl cspm_set_diff s ss
    cspm_set_diff (s1@(CSPM_Powerset _)) s2 =
        cspm_set_diff (cspm_set_make_explicit s1) s2
    cspm_set_diff s1 (s2@(CSPM_Powerset _)) =
        cspm_set_diff s1 (cspm_set_make_explicit s2)

    cspm_set_fromList = CSPM_ExplicitSetSet . S.fromList

    cspm_set_inter (CSPM_ExplicitSetSet s1) (CSPM_ExplicitSetSet s2) =
        CSPM_ExplicitSetSet $ S.intersection s1 s2
    cspm_set_inter (CSPM_SetSetUnion ss) s = CSPM_SetSetUnion (map (cspm_set_inter s) ss)
    cspm_set_inter s (CSPM_SetSetUnion ss) = CSPM_SetSetUnion (map (cspm_set_inter s) ss)
    cspm_set_inter (CSPM_Powerset s1) (CSPM_Powerset s2) =
        CSPM_Powerset (cspm_set_inter s1 s2)
    cspm_set_inter (s1@(CSPM_Powerset _)) s2 =
        cspm_set_inter (cspm_set_make_explicit s1) s2
    cspm_set_inter s1 (s2@(CSPM_Powerset _)) =
        cspm_set_inter (cspm_set_make_explicit s2) s1

    cspm_set_null (CSPM_Powerset _) = False
    cspm_set_null (CSPM_ExplicitSetSet s) = S.null s
    cspm_set_null (CSPM_SetSetUnion s) = and (map cspm_set_null s)

    cspm_set_member s (CSPM_ExplicitSetSet ss) = S.member s ss
    cspm_set_member s (CSPM_Powerset s') =
        case cspm_compare s s' of
            Just EQ -> True
            Just LT -> True
            _ -> False
    cspm_set_member s (CSPM_SetSetUnion ss) = or (map (cspm_set_member s) ss)

    cspm_set_toList (CSPM_Powerset s) =
        (map cspm_set_fromList . filterM (\x -> [True, False]) . cspm_set_toList) s
    cspm_set_toList (CSPM_ExplicitSetSet s) = S.toList s
    cspm_set_toList (CSPM_SetSetUnion s) = concatMap cspm_set_toList s

    cspm_set_union (CSPM_ExplicitSetSet s1) (CSPM_ExplicitSetSet s2) =
        CSPM_ExplicitSetSet (S.union s1 s2)
    cspm_set_union (CSPM_SetSetUnion s1) (CSPM_SetSetUnion s2) =
        CSPM_SetSetUnion (s1++s2)
    cspm_set_union (CSPM_SetSetUnion xs) s = CSPM_SetSetUnion $ s:xs
    cspm_set_union s (CSPM_SetSetUnion xs) = CSPM_SetSetUnion $ s:xs
    cspm_set_union (s1@(CSPM_Powerset _)) s2 = CSPM_SetSetUnion [s1, s2]
    cspm_set_union s1 (s2@(CSPM_Powerset _)) = CSPM_SetSetUnion [s1, s2]

instance (CSPM_Set a, CSPM_Set (CSPM_SetType a)) =>
        CSPM_Show (CSPM_SetType (CSPM_SetType a)) where
    cspm_show (CSPM_Powerset xs) = text "Set" <> parens (cspm_show xs)
    cspm_show (CSPM_SetSetUnion xs) = cspm_show_union_set xs
    cspm_show (CSPM_ExplicitSetSet xs) = braces $ cspm_prettyPrint_list $
        map cspm_show $ S.toList xs

instance (CSPM_Set a, CSPM_Set (CSPM_SetType a)) =>
        CSPM_Ord (CSPM_SetType (CSPM_SetType a)) where
    cspm_compare (s1@(CSPM_SetSetUnion _)) s2 =
        cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_SetSetUnion _))  =
        cspm_flipOrder (cspm_compare s2 s1)
    cspm_compare (CSPM_ExplicitSetSet s1) (CSPM_ExplicitSetSet s2) =
        if s1 == s2 then Just EQ
        else if S.isProperSubsetOf s1 s2 then Just LT
        else if S.isProperSubsetOf s2 s1 then Just GT
        else Nothing
    cspm_compare (CSPM_Powerset xs) (CSPM_Powerset ys) = cspm_compare xs ys
    cspm_compare s1 (s2@(CSPM_Powerset _)) =
        cspm_compare s1 (cspm_set_make_explicit s2)
    cspm_compare (s1@(CSPM_Powerset _)) s2 = cspm_flipOrder $ cspm_compare s2 s1

instance CSPM_Set a => CSPM_Set [a] where

    data CSPM_SetType [a] =
        CSPM_AllSequences (CSPM_SetType a)
        | CSPM_ExplicitSeqSet (S.Set [a])
        | CSPM_SeqSetUnion [CSPM_SetType [a]]

    cspm_set_card (CSPM_AllSequences s) = cspm_set_card_of_infinite_set_message
    cspm_set_card (CSPM_ExplicitSeqSet s) = S.size s
    cspm_set_card (CSPM_SeqSetUnion ss) = sum (map cspm_set_card ss)

    cspm_set_empty = CSPM_ExplicitSeqSet S.empty

    cspm_set_diff (CSPM_ExplicitSeqSet s1) (CSPM_ExplicitSeqSet s2) =
        CSPM_ExplicitSeqSet $ S.difference s1 s2
    cspm_set_diff (CSPM_SeqSetUnion ss) s =
        CSPM_SeqSetUnion $ map (\s1 -> cspm_set_diff s1 s) ss
    cspm_set_diff s (CSPM_SeqSetUnion ss) = foldl cspm_set_diff s ss
    cspm_set_diff (s1@(CSPM_AllSequences _)) s2 =
        cspm_set_diff (cspm_set_make_explicit s1) s2
    cspm_set_diff s1 (s2@(CSPM_AllSequences _)) =
        cspm_set_diff s1 (cspm_set_make_explicit s2)

    cspm_set_fromList = CSPM_ExplicitSeqSet . S.fromList

    cspm_set_inter (CSPM_ExplicitSeqSet s1) (CSPM_ExplicitSeqSet s2) =
        CSPM_ExplicitSeqSet $ S.intersection s1 s2
    cspm_set_inter (CSPM_SeqSetUnion ss) s = CSPM_SeqSetUnion (map (cspm_set_inter s) ss)
    cspm_set_inter s (CSPM_SeqSetUnion ss) = CSPM_SeqSetUnion (map (cspm_set_inter s) ss)
    cspm_set_inter (s1@(CSPM_AllSequences _)) s2 =
        cspm_set_inter (cspm_set_make_explicit s1) s2
    cspm_set_inter s1 (s2@(CSPM_AllSequences _)) =
        cspm_set_inter (cspm_set_make_explicit s2) s1

    cspm_set_null (CSPM_AllSequences s) = cspm_set_null s
    cspm_set_null (CSPM_ExplicitSeqSet s) = S.null s
    cspm_set_null (CSPM_SeqSetUnion s) = and (map cspm_set_null s)

    cspm_set_member s (CSPM_ExplicitSeqSet ss) = S.member s ss
    cspm_set_member s (CSPM_AllSequences s') = and (map (flip cspm_set_member s') s)
    cspm_set_member s (CSPM_SeqSetUnion ss) = or (map (cspm_set_member s) ss)

    cspm_set_toList (CSPM_AllSequences s) | cspm_set_null s = []
    cspm_set_toList (CSPM_AllSequences s) =
        let 
            itemsAsList = cspm_set_toList s
            list 0 = [[]]
            list n = concatMap (\x -> map (x : ) (list (n-1)))  itemsAsList
        in concatMap list [0..]
    cspm_set_toList (CSPM_ExplicitSeqSet s) = S.toList s
    cspm_set_toList (CSPM_SeqSetUnion s) = concatMap cspm_set_toList s

    cspm_set_union (CSPM_ExplicitSeqSet s1) (CSPM_ExplicitSeqSet s2) =
        CSPM_ExplicitSeqSet (S.union s1 s2)
    cspm_set_union (CSPM_SeqSetUnion s1) (CSPM_SeqSetUnion s2) =
        CSPM_SeqSetUnion (s1++s2)
    cspm_set_union (CSPM_SeqSetUnion xs) s = CSPM_SeqSetUnion $ s:xs
    cspm_set_union s (CSPM_SeqSetUnion xs) = CSPM_SeqSetUnion $ s:xs
    cspm_set_union (s1@(CSPM_AllSequences _)) s2 = CSPM_SeqSetUnion [s1, s2]
    cspm_set_union s1 (s2@(CSPM_AllSequences _)) = CSPM_SeqSetUnion [s1, s2]

instance (CSPM_Set a, CSPM_Set [a]) => CSPM_Show (CSPM_SetType [a]) where
    cspm_show (CSPM_AllSequences xs) = text "Seq" <> parens (cspm_show xs)
    cspm_show (CSPM_SeqSetUnion xs) = cspm_show_union_set xs
    cspm_show (CSPM_ExplicitSeqSet xs) = braces $ cspm_prettyPrint_list $
        map cspm_show $ S.toList xs

instance (CSPM_Set a, CSPM_Set [a]) => CSPM_Ord (CSPM_SetType [a]) where
    cspm_compare (s1@(CSPM_SeqSetUnion _)) s2 =
        cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_SeqSetUnion _))  =
        cspm_flipOrder (cspm_compare s2 s1)
    cspm_compare (CSPM_ExplicitSeqSet s1) (CSPM_ExplicitSeqSet s2) =
        if s1 == s2 then Just EQ
        else if S.isProperSubsetOf s1 s2 then Just LT
        else if S.isProperSubsetOf s2 s1 then Just GT
        else Nothing
    cspm_compare (CSPM_AllSequences vs1) (CSPM_AllSequences vs2) =
        cspm_compare vs1 vs2
    cspm_compare (CSPM_ExplicitSeqSet vs) (CSPM_AllSequences vss) =
        if and (map (flip cspm_set_member (CSPM_AllSequences vss)) (S.toList vs)) then Just LT
        -- Otherwise, there is some item in vs that is not in vss. However, unless
        -- vss is empty there must be some value in the second set that is not in
        -- the first, since (AllSequences vss) is infinite and, if the above
        -- finishes, we know the first set is finite. Hence, they would be
        -- incomparable.
        else Nothing
    cspm_compare (CSPM_AllSequences vss) (CSPM_ExplicitSeqSet vs) =
        cspm_flipOrder (cspm_compare (CSPM_ExplicitSeqSet vs) (CSPM_AllSequences vss))

instance (CSPM_Set k, CSPM_Set v) => CSPM_Set (M.Map k v) where

    data CSPM_SetType (M.Map k v) =
        CSPM_AllMaps (CSPM_SetType k) (CSPM_SetType v)
        | CSPM_ExplicitMapSet (S.Set (M.Map k v))
        | CSPM_MapSetUnion [CSPM_SetType (M.Map k v)]

    cspm_set_card (CSPM_AllMaps k v) =
        -- For each key, we can either not map it to anything, or map it to one
        -- of the values
        (cspm_set_card v + 1) ^ (cspm_set_card k)
    cspm_set_card (CSPM_ExplicitMapSet s) = S.size s
    cspm_set_card (CSPM_MapSetUnion ss) = sum (map cspm_set_card ss)

    cspm_set_empty = CSPM_ExplicitMapSet S.empty

    cspm_set_diff (CSPM_ExplicitMapSet s1) (CSPM_ExplicitMapSet s2) =
        CSPM_ExplicitMapSet $ S.difference s1 s2
    cspm_set_diff (CSPM_MapSetUnion ss) s =
        CSPM_MapSetUnion $ map (\s1 -> cspm_set_diff s1 s) ss
    cspm_set_diff s (CSPM_MapSetUnion ss) = foldl cspm_set_diff s ss
    cspm_set_diff (s1@(CSPM_AllMaps _ _)) s2 =
        cspm_set_diff (cspm_set_make_explicit s1) s2
    cspm_set_diff s1 (s2@(CSPM_AllMaps _ _)) =
        cspm_set_diff s1 (cspm_set_make_explicit s2)

    cspm_set_fromList = CSPM_ExplicitMapSet . S.fromList

    cspm_set_inter (CSPM_ExplicitMapSet s1) (CSPM_ExplicitMapSet s2) =
        CSPM_ExplicitMapSet $ S.intersection s1 s2
    cspm_set_inter (CSPM_MapSetUnion ss) s = CSPM_MapSetUnion (map (cspm_set_inter s) ss)
    cspm_set_inter s (CSPM_MapSetUnion ss) = CSPM_MapSetUnion (map (cspm_set_inter s) ss)
    cspm_set_inter (s1@(CSPM_AllMaps _ _)) s2 =
        cspm_set_inter (cspm_set_make_explicit s1) s2
    cspm_set_inter s1 (s2@(CSPM_AllMaps _ _)) =
        cspm_set_inter (cspm_set_make_explicit s2) s1

    cspm_set_null (CSPM_AllMaps k v) = cspm_set_null k || cspm_set_null v
    cspm_set_null (CSPM_ExplicitMapSet s) = S.null s
    cspm_set_null (CSPM_MapSetUnion s) = and (map cspm_set_null s)

    cspm_set_member s (CSPM_ExplicitMapSet ss) = S.member s ss
    cspm_set_member m (CSPM_AllMaps ks vs) = and $
        map (\ (k, v) -> cspm_set_member k ks && cspm_set_member v vs) (M.toList m)
    cspm_set_member s (CSPM_MapSetUnion ss) = or (map (cspm_set_member s) ss)

    cspm_set_toList (CSPM_AllMaps ks' vs') =
        let vs = cspm_set_toList vs'
            -- | Creates all maps that have the given set as its domain
            makeMaps [] = [M.empty]
            makeMaps (k:ks) = [M.insert k v m | m <- makeMaps ks, v <- vs]
        in concatMap (makeMaps . cspm_set_toList) (cspm_set_toList (CSPM_Powerset ks'))
    cspm_set_toList (CSPM_ExplicitMapSet s) = S.toList s
    cspm_set_toList (CSPM_MapSetUnion s) = concatMap cspm_set_toList s

    cspm_set_union (CSPM_ExplicitMapSet s1) (CSPM_ExplicitMapSet s2) =
        CSPM_ExplicitMapSet (S.union s1 s2)
    cspm_set_union (CSPM_MapSetUnion s1) (CSPM_MapSetUnion s2) =
        CSPM_MapSetUnion (s1++s2)
    cspm_set_union (CSPM_MapSetUnion xs) s = CSPM_MapSetUnion $ s:xs
    cspm_set_union s (CSPM_MapSetUnion xs) = CSPM_MapSetUnion $ s:xs
    cspm_set_union (s1@(CSPM_AllMaps _ _)) s2 = CSPM_MapSetUnion [s1, s2]
    cspm_set_union s1 (s2@(CSPM_AllMaps _ _)) = CSPM_MapSetUnion [s1, s2]

instance (CSPM_Set k, CSPM_Set v, CSPM_Set (M.Map k v)) =>
        CSPM_Show (CSPM_SetType (M.Map k v)) where
    cspm_show (CSPM_AllMaps k v) =
        text "Map" <> parens (cspm_prettyPrint_list [cspm_show k, cspm_show v])
    cspm_show (CSPM_MapSetUnion xs) = cspm_show_union_set xs
    cspm_show (CSPM_ExplicitMapSet xs) = braces $ cspm_prettyPrint_list $
        map cspm_show $ S.toList xs

instance (CSPM_Set k, CSPM_Set v, CSPM_Set (M.Map k v)) =>
        CSPM_Ord (CSPM_SetType (M.Map k v)) where
    cspm_compare (s1@(CSPM_MapSetUnion _)) s2 =
        cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_MapSetUnion _))  =
        cspm_flipOrder (cspm_compare s2 s1)
    cspm_compare (CSPM_ExplicitMapSet s1) (CSPM_ExplicitMapSet s2) =
        if s1 == s2 then Just EQ
        else if S.isProperSubsetOf s1 s2 then Just LT
        else if S.isProperSubsetOf s2 s1 then Just GT
        else Nothing
    cspm_compare s1 (s2@(CSPM_AllMaps _ _)) =
        cspm_compare s1 (cspm_set_make_explicit s2)
    cspm_compare (s1@(CSPM_AllMaps _ _)) s2 =
        cspm_flipOrder (cspm_compare s2 s1)

instance (CSPM_Set a1, CSPM_Set a2) => CSPM_Set (CSPM_ExplicitDot a1 a2) where
    data CSPM_SetType (CSPM_ExplicitDot a1 a2) =
        CSPM_ExplicitDotProduct (CSPM_SetType a1) (CSPM_SetType a2)
        | CSPM_ExplicitDotExplicitSet (S.Set (CSPM_ExplicitDot a1 a2))
        | CSPM_ExplicitDotSetUnion [CSPM_SetType (CSPM_ExplicitDot a1 a2)]

    cspm_set_card (CSPM_ExplicitDotProduct a1 a2) = cspm_set_card a1 * cspm_set_card a2
    cspm_set_card (CSPM_ExplicitDotExplicitSet a) = S.size a
    cspm_set_card (CSPM_ExplicitDotSetUnion a) = sum (map cspm_set_card a)

    cspm_set_empty = CSPM_ExplicitDotExplicitSet S.empty

    cspm_set_diff (CSPM_ExplicitDotExplicitSet a) (CSPM_ExplicitDotExplicitSet b) =
        CSPM_ExplicitDotExplicitSet (S.difference a b)
    cspm_set_diff (CSPM_ExplicitDotSetUnion a) b =
        CSPM_ExplicitDotSetUnion $ map (\ s1 -> cspm_set_diff s1 b) a
    cspm_set_diff a (CSPM_ExplicitDotSetUnion b) = foldl cspm_set_diff a b
    cspm_set_diff (a@(CSPM_ExplicitDotProduct {})) b =
        cspm_set_diff (cspm_set_make_explicit a) b
    cspm_set_diff a (b@(CSPM_ExplicitDotProduct {})) =
        cspm_set_diff a (cspm_set_make_explicit b)

    cspm_set_fromList = CSPM_ExplicitDotExplicitSet . S.fromList

    cspm_set_inter (CSPM_ExplicitDotExplicitSet a) (CSPM_ExplicitDotExplicitSet b) =
        CSPM_ExplicitDotExplicitSet $ S.intersection a b
    cspm_set_inter (CSPM_ExplicitDotSetUnion a) b =
        CSPM_ExplicitDotSetUnion (map (cspm_set_inter b) a)
    cspm_set_inter a (CSPM_ExplicitDotSetUnion b) =
        CSPM_ExplicitDotSetUnion (map (cspm_set_inter a) b)
    cspm_set_inter (CSPM_ExplicitDotProduct a1 a2) (CSPM_ExplicitDotProduct b1 b2) =
        CSPM_ExplicitDotProduct (cspm_set_inter a1 b1) (cspm_set_inter a2 b2)
    cspm_set_inter (a@(CSPM_ExplicitDotProduct {})) b =
        cspm_set_inter (cspm_set_make_explicit a) b
    cspm_set_inter a (b@(CSPM_ExplicitDotProduct {})) =
        cspm_set_inter (cspm_set_make_explicit b) a

    cspm_set_null (CSPM_ExplicitDotProduct a1 a2) = cspm_set_null a1 && cspm_set_null a2
    cspm_set_null (CSPM_ExplicitDotExplicitSet a) = S.null a
    cspm_set_null (CSPM_ExplicitDotSetUnion a) = and (map cspm_set_null a)

    cspm_set_member v (CSPM_ExplicitDotExplicitSet a) = S.member v a
    cspm_set_member (CSPM_ExplicitDot a1 a2) (CSPM_ExplicitDotProduct b1 b2) =
        cspm_set_member a1 b1 && cspm_set_member a2 b2
    cspm_set_member v (CSPM_ExplicitDotSetUnion a) = or (map (cspm_set_member v) a)

    cspm_set_toList (CSPM_ExplicitDotExplicitSet a) = S.toList a
    cspm_set_toList (CSPM_ExplicitDotProduct b1 b2) =
        [CSPM_ExplicitDot a1 a2 | a1 <- cspm_set_toList b1, a2 <- cspm_set_toList b2]
    cspm_set_toList (CSPM_ExplicitDotSetUnion a) = concatMap cspm_set_toList a

    cspm_set_union (CSPM_ExplicitDotExplicitSet a) (CSPM_ExplicitDotExplicitSet b) =
        CSPM_ExplicitDotExplicitSet $ S.union a b
    cspm_set_union (CSPM_ExplicitDotSetUnion a) (CSPM_ExplicitDotSetUnion b) =
        CSPM_ExplicitDotSetUnion (a ++ b)
    cspm_set_union (CSPM_ExplicitDotSetUnion a) b =
        CSPM_ExplicitDotSetUnion (b:a)
    cspm_set_union a (CSPM_ExplicitDotSetUnion b) =
        CSPM_ExplicitDotSetUnion (a:b)
    cspm_set_union (a@(CSPM_ExplicitDotProduct {})) b =
        CSPM_ExplicitDotSetUnion [a, b]
    cspm_set_union a (b@(CSPM_ExplicitDotProduct {})) =
        CSPM_ExplicitDotSetUnion [a, b]

instance (CSPM_Set a1, CSPM_Set a2) => CSPM_Show (CSPM_SetType (CSPM_ExplicitDot a1 a2)) where
    cspm_show (CSPM_ExplicitDotProduct a1 a2) =
        parens $ cspm_prettyPrint_dotSep [cspm_show a1, cspm_show a2]
    cspm_show (CSPM_ExplicitDotSetUnion a) = cspm_show_union_set a
    cspm_show (CSPM_ExplicitDotExplicitSet a) =
        braces $ cspm_prettyPrint_list $ map cspm_show $ S.toList a

instance (CSPM_Set a1, CSPM_Set a2, CSPM_Set (CSPM_ExplicitDot a1 a2)) =>
        CSPM_Ord (CSPM_SetType (CSPM_ExplicitDot a1 a2)) where
    cspm_compare (s1@(CSPM_ExplicitDotSetUnion {})) s2 =
        cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_ExplicitDotSetUnion {})) =
        cspm_flipOrder (cspm_compare s1 s1)
    cspm_compare (CSPM_ExplicitDotExplicitSet a) (CSPM_ExplicitDotExplicitSet b) =
        if a == b then Just EQ
        else if S.isProperSubsetOf a b then Just LT
        else if S.isProperSubsetOf b a then Just GT
        else Nothing
    cspm_compare (CSPM_ExplicitDotProduct a1 a2) (CSPM_ExplicitDotProduct b1 b2) =
        cspm_set_product_order [cspm_compare a1 b1, cspm_compare a2 b2]
    cspm_compare s1 (s2@(CSPM_ExplicitDotProduct {})) =
        cspm_compare s1 (cspm_set_make_explicit s2)
    cspm_compare (s1@(CSPM_ExplicitDotProduct {})) s2 =
        cspm_flipOrder $ cspm_compare s2 s1


gen_60 :: CSPM_Show a => a -> [Char]
gen_60 = show . cspm_show
gen_19 :: CSPM_SetType Bool
gen_19 = cspm_set_fromList [True, False]
gen_22 :: CSPM_SetType Char
gen_22 = cspm_set_fromList [minBound :: Char ..]
gen_56 :: (CSPM_Set (a, b), CSPM_Set (b, a)) => CSPM_SetType (a, b) -> CSPM_SetType (b, a)
gen_56 = cspm_set_fromList . map (\ (a, b) -> (b, a)) . cspm_set_toList
gen_67 :: Ord k => M.Map k v -> [(k, v)] -> M.Map k v
gen_67 = foldr (uncurry M.insert)
gen_63 :: Ord k => Doc -> M.Map k v -> k -> v
gen_63 location m k =
    case M.lookup k m of
        Just v -> v
        Nothing -> cspm_panic location (text "")
instance CSPM_Set Bool where
    data CSPM_SetType Bool = CSPM_ExplicitBoolSet (S.Set Bool)
    cspm_set_empty = CSPM_ExplicitBoolSet S.empty
    cspm_set_card (CSPM_ExplicitBoolSet s) = S.size s
    cspm_set_fromList = CSPM_ExplicitBoolSet . S.fromList
    cspm_set_diff (CSPM_ExplicitBoolSet s1) (CSPM_ExplicitBoolSet s2) = CSPM_ExplicitBoolSet (S.difference s1 s2)
    cspm_set_null (CSPM_ExplicitBoolSet s) = S.null s
    cspm_set_inter (CSPM_ExplicitBoolSet s1) (CSPM_ExplicitBoolSet s2) = CSPM_ExplicitBoolSet (S.intersection s1 s2)
    cspm_set_member v (CSPM_ExplicitBoolSet s) = S.member v s
    cspm_set_toList (CSPM_ExplicitBoolSet s) = S.toList s
    cspm_set_union (CSPM_ExplicitBoolSet s1) (CSPM_ExplicitBoolSet s2) = CSPM_ExplicitBoolSet (S.union s1 s2)
instance CSPM_Ord (CSPM_SetType Bool) where
    cspm_compare (CSPM_ExplicitBoolSet s1) (CSPM_ExplicitBoolSet s2) =
        if s1 == s2 then Just EQ
        else if S.isProperSubsetOf s1 s2 then Just LT
        else if S.isProperSubsetOf s2 s1 then Just GT
        else Nothing
instance CSPM_Show (CSPM_SetType Bool) where
    cspm_show (CSPM_ExplicitBoolSet s) = braces (cspm_prettyPrint_list (map cspm_show (S.toList s)))
instance CSPM_Set Char where
    data CSPM_SetType Char = CSPM_ExplicitCharSet (S.Set Char)
    cspm_set_empty = CSPM_ExplicitCharSet S.empty
    cspm_set_card (CSPM_ExplicitCharSet s) = S.size s
    cspm_set_fromList = CSPM_ExplicitCharSet . S.fromList
    cspm_set_diff (CSPM_ExplicitCharSet s1) (CSPM_ExplicitCharSet s2) = CSPM_ExplicitCharSet (S.difference s1 s2)
    cspm_set_null (CSPM_ExplicitCharSet s) = S.null s
    cspm_set_inter (CSPM_ExplicitCharSet s1) (CSPM_ExplicitCharSet s2) = CSPM_ExplicitCharSet (S.intersection s1 s2)
    cspm_set_member v (CSPM_ExplicitCharSet s) = S.member v s
    cspm_set_toList (CSPM_ExplicitCharSet s) = S.toList s
    cspm_set_union (CSPM_ExplicitCharSet s1) (CSPM_ExplicitCharSet s2) = CSPM_ExplicitCharSet (S.union s1 s2)
instance CSPM_Ord (CSPM_SetType Char) where
    cspm_compare (CSPM_ExplicitCharSet s1) (CSPM_ExplicitCharSet s2) =
        if s1 == s2 then Just EQ
        else if S.isProperSubsetOf s1 s2 then Just LT
        else if S.isProperSubsetOf s2 s1 then Just GT
        else Nothing
instance CSPM_Show (CSPM_SetType Char) where
    cspm_show (CSPM_ExplicitCharSet s) = braces (cspm_prettyPrint_list (map cspm_show (S.toList s)))
instance (CSPM_Show a1, CSPM_Show a2) => CSPM_Show (a1, a2) where
    cspm_show (a1, a2) = parens (cspm_prettyPrint_list [cspm_show a1, cspm_show a2])
instance (CSPM_Ord a1, CSPM_Ord a2) => CSPM_Ord (a1, a2) where
    cspm_compare (a1, a2) (b1, b2) = cspm_compare a1 b1 `cspm_thenCmp` cspm_compare a2 b2
instance  CSPM_Dot (a1, a2) a (CSPM_ExplicitDot (a1, a2) a) where
    cspm_dotOn x y = (CSPM_ExplicitDot x y)
instance (CSPM_Set a1, CSPM_Set a2) => CSPM_Set (a1, a2) where
    data CSPM_SetType (a1, a2) =
        CSPM_Tuple2Product (CSPM_SetType a1) (CSPM_SetType a2)
        | CSPM_ExplicitTuple2Set (S.Set (a1, a2))
        | CSPM_Tuple2SetUnion [CSPM_SetType (a1, a2)]
    cspm_set_card (CSPM_Tuple2Product a1 a2) = cspm_set_card a1 * cspm_set_card a2
    cspm_set_card (CSPM_ExplicitTuple2Set a) = S.size a
    cspm_set_card (CSPM_Tuple2SetUnion a) = sum (map cspm_set_card a)
    cspm_set_empty = CSPM_ExplicitTuple2Set S.empty
    cspm_set_diff (CSPM_ExplicitTuple2Set a) (CSPM_ExplicitTuple2Set b) = CSPM_ExplicitTuple2Set (S.difference a b)
    cspm_set_diff (CSPM_Tuple2SetUnion a) b = CSPM_Tuple2SetUnion $ map (\ s1 -> cspm_set_diff s1 b) a
    cspm_set_diff a (CSPM_Tuple2SetUnion b) = foldl cspm_set_diff a b
    cspm_set_diff (a@(CSPM_Tuple2Product {})) b = cspm_set_diff (cspm_set_make_explicit a) b
    cspm_set_diff a (b@(CSPM_Tuple2Product {})) = cspm_set_diff a (cspm_set_make_explicit b)
    cspm_set_fromList = CSPM_ExplicitTuple2Set . S.fromList
    cspm_set_inter (CSPM_ExplicitTuple2Set a) (CSPM_ExplicitTuple2Set b) = CSPM_ExplicitTuple2Set $ S.intersection a b
    cspm_set_inter (CSPM_Tuple2SetUnion a) b = CSPM_Tuple2SetUnion (map (cspm_set_inter b) a)
    cspm_set_inter a (CSPM_Tuple2SetUnion b) = CSPM_Tuple2SetUnion (map (cspm_set_inter a) b)
    cspm_set_inter (CSPM_Tuple2Product a1 a2) (CSPM_Tuple2Product b1 b2) = CSPM_Tuple2Product (cspm_set_inter a1 b1) (cspm_set_inter a2 b2)
    cspm_set_inter (a@(CSPM_Tuple2Product {})) b = cspm_set_inter (cspm_set_make_explicit a) b
    cspm_set_inter a (b@(CSPM_Tuple2Product {})) = cspm_set_inter (cspm_set_make_explicit b) a
    cspm_set_null (CSPM_Tuple2Product a1 a2) = cspm_set_null a1 && cspm_set_null a2
    cspm_set_null (CSPM_ExplicitTuple2Set a) = S.null a
    cspm_set_null (CSPM_Tuple2SetUnion a) = and (map cspm_set_null a)
    cspm_set_member v (CSPM_ExplicitTuple2Set a) = S.member v a
    cspm_set_member (a1, a2) (CSPM_Tuple2Product b1 b2) = cspm_set_member a1 b1 && cspm_set_member a2 b2
    cspm_set_member v (CSPM_Tuple2SetUnion a) = or (map (cspm_set_member v) a)
    cspm_set_toList (CSPM_ExplicitTuple2Set a) = S.toList a
    cspm_set_toList (CSPM_Tuple2Product b1 b2) = [(a1, a2) | a1 <- cspm_set_toList b1, a2 <- cspm_set_toList b2]
    cspm_set_toList (CSPM_Tuple2SetUnion a) = concatMap cspm_set_toList a
    cspm_set_union (CSPM_ExplicitTuple2Set a) (CSPM_ExplicitTuple2Set b) = CSPM_ExplicitTuple2Set $ S.union a b
    cspm_set_union (CSPM_Tuple2SetUnion a) (CSPM_Tuple2SetUnion b) = CSPM_Tuple2SetUnion (a ++ b)
    cspm_set_union (CSPM_Tuple2SetUnion a) b = CSPM_Tuple2SetUnion (b:a)
    cspm_set_union a (CSPM_Tuple2SetUnion b) = CSPM_Tuple2SetUnion (a:b)
    cspm_set_union (a@(CSPM_Tuple2Product {})) b = CSPM_Tuple2SetUnion [a, b]
    cspm_set_union a (b@(CSPM_Tuple2Product {})) = CSPM_Tuple2SetUnion [a, b]
instance (CSPM_Set a1, CSPM_Set a2) => CSPM_Show (CSPM_SetType (a1, a2)) where
    cspm_show (CSPM_Tuple2Product a1 a2) = parens $ cspm_prettyPrint_list [cspm_show a1, cspm_show a2]
    cspm_show (CSPM_Tuple2SetUnion a) = cspm_show_union_set a
    cspm_show (CSPM_ExplicitTuple2Set a) = braces $ cspm_prettyPrint_list $ map cspm_show $ S.toList a
instance (CSPM_Set a1, CSPM_Set a2, CSPM_Set (a1, a2)) => CSPM_Ord (CSPM_SetType (a1, a2)) where
    cspm_compare (s1@(CSPM_Tuple2SetUnion {})) s2 = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_Tuple2SetUnion {})) = cspm_flipOrder (cspm_compare s1 s1)
    cspm_compare (CSPM_ExplicitTuple2Set a) (CSPM_ExplicitTuple2Set b) =
        if a == b then Just EQ
        else if S.isProperSubsetOf a b then Just LT
        else if S.isProperSubsetOf b a then Just GT
        else Nothing
    cspm_compare (CSPM_Tuple2Product a1 a2) (CSPM_Tuple2Product b1 b2) = cspm_set_product_order [cspm_compare a1 b1, cspm_compare a2 b2]
    cspm_compare s1 (s2@(CSPM_Tuple2Product {})) = cspm_compare s1 (cspm_set_make_explicit s2)
    cspm_compare (s1@(CSPM_Tuple2Product {})) s2 = cspm_flipOrder $ cspm_compare s2 s1
instance (CSPM_Show a1, CSPM_Show a2, CSPM_Show a3) => CSPM_Show (a1, a2, a3) where
    cspm_show (a1, a2, a3) = parens (cspm_prettyPrint_list [cspm_show a1, cspm_show a2, cspm_show a3])
instance (CSPM_Ord a1, CSPM_Ord a2, CSPM_Ord a3) => CSPM_Ord (a1, a2, a3) where
    cspm_compare (a1, a2, a3) (b1, b2, b3) = cspm_compare a1 b1 `cspm_thenCmp` cspm_compare a2 b2 `cspm_thenCmp` cspm_compare a3 b3
instance  CSPM_Dot (a1, a2, a3) a (CSPM_ExplicitDot (a1, a2, a3) a) where
    cspm_dotOn x y = (CSPM_ExplicitDot x y)
instance (CSPM_Set a1, CSPM_Set a2, CSPM_Set a3) => CSPM_Set (a1, a2, a3) where
    data CSPM_SetType (a1, a2, a3) =
        CSPM_Tuple3Product (CSPM_SetType a1) (CSPM_SetType a2) (CSPM_SetType a3)
        | CSPM_ExplicitTuple3Set (S.Set (a1, a2, a3))
        | CSPM_Tuple3SetUnion [CSPM_SetType (a1, a2, a3)]
    cspm_set_card (CSPM_Tuple3Product a1 a2 a3) = cspm_set_card a1 * cspm_set_card a2 * cspm_set_card a3
    cspm_set_card (CSPM_ExplicitTuple3Set a) = S.size a
    cspm_set_card (CSPM_Tuple3SetUnion a) = sum (map cspm_set_card a)
    cspm_set_empty = CSPM_ExplicitTuple3Set S.empty
    cspm_set_diff (CSPM_ExplicitTuple3Set a) (CSPM_ExplicitTuple3Set b) = CSPM_ExplicitTuple3Set (S.difference a b)
    cspm_set_diff (CSPM_Tuple3SetUnion a) b = CSPM_Tuple3SetUnion $ map (\ s1 -> cspm_set_diff s1 b) a
    cspm_set_diff a (CSPM_Tuple3SetUnion b) = foldl cspm_set_diff a b
    cspm_set_diff (a@(CSPM_Tuple3Product {})) b = cspm_set_diff (cspm_set_make_explicit a) b
    cspm_set_diff a (b@(CSPM_Tuple3Product {})) = cspm_set_diff a (cspm_set_make_explicit b)
    cspm_set_fromList = CSPM_ExplicitTuple3Set . S.fromList
    cspm_set_inter (CSPM_ExplicitTuple3Set a) (CSPM_ExplicitTuple3Set b) = CSPM_ExplicitTuple3Set $ S.intersection a b
    cspm_set_inter (CSPM_Tuple3SetUnion a) b = CSPM_Tuple3SetUnion (map (cspm_set_inter b) a)
    cspm_set_inter a (CSPM_Tuple3SetUnion b) = CSPM_Tuple3SetUnion (map (cspm_set_inter a) b)
    cspm_set_inter (CSPM_Tuple3Product a1 a2 a3) (CSPM_Tuple3Product b1 b2 b3) = CSPM_Tuple3Product (cspm_set_inter a1 b1) (cspm_set_inter a2 b2) (cspm_set_inter a3 b3)
    cspm_set_inter (a@(CSPM_Tuple3Product {})) b = cspm_set_inter (cspm_set_make_explicit a) b
    cspm_set_inter a (b@(CSPM_Tuple3Product {})) = cspm_set_inter (cspm_set_make_explicit b) a
    cspm_set_null (CSPM_Tuple3Product a1 a2 a3) = cspm_set_null a1 && cspm_set_null a2 && cspm_set_null a3
    cspm_set_null (CSPM_ExplicitTuple3Set a) = S.null a
    cspm_set_null (CSPM_Tuple3SetUnion a) = and (map cspm_set_null a)
    cspm_set_member v (CSPM_ExplicitTuple3Set a) = S.member v a
    cspm_set_member (a1, a2, a3) (CSPM_Tuple3Product b1 b2 b3) = cspm_set_member a1 b1 && cspm_set_member a2 b2 && cspm_set_member a3 b3
    cspm_set_member v (CSPM_Tuple3SetUnion a) = or (map (cspm_set_member v) a)
    cspm_set_toList (CSPM_ExplicitTuple3Set a) = S.toList a
    cspm_set_toList (CSPM_Tuple3Product b1 b2 b3) = [(a1, a2, a3) | a1 <- cspm_set_toList b1, a2 <- cspm_set_toList b2, a3 <- cspm_set_toList b3]
    cspm_set_toList (CSPM_Tuple3SetUnion a) = concatMap cspm_set_toList a
    cspm_set_union (CSPM_ExplicitTuple3Set a) (CSPM_ExplicitTuple3Set b) = CSPM_ExplicitTuple3Set $ S.union a b
    cspm_set_union (CSPM_Tuple3SetUnion a) (CSPM_Tuple3SetUnion b) = CSPM_Tuple3SetUnion (a ++ b)
    cspm_set_union (CSPM_Tuple3SetUnion a) b = CSPM_Tuple3SetUnion (b:a)
    cspm_set_union a (CSPM_Tuple3SetUnion b) = CSPM_Tuple3SetUnion (a:b)
    cspm_set_union (a@(CSPM_Tuple3Product {})) b = CSPM_Tuple3SetUnion [a, b]
    cspm_set_union a (b@(CSPM_Tuple3Product {})) = CSPM_Tuple3SetUnion [a, b]
instance (CSPM_Set a1, CSPM_Set a2, CSPM_Set a3) => CSPM_Show (CSPM_SetType (a1, a2, a3)) where
    cspm_show (CSPM_Tuple3Product a1 a2 a3) = parens $ cspm_prettyPrint_list [cspm_show a1, cspm_show a2, cspm_show a3]
    cspm_show (CSPM_Tuple3SetUnion a) = cspm_show_union_set a
    cspm_show (CSPM_ExplicitTuple3Set a) = braces $ cspm_prettyPrint_list $ map cspm_show $ S.toList a
instance (CSPM_Set a1, CSPM_Set a2, CSPM_Set a3, CSPM_Set (a1, a2, a3)) => CSPM_Ord (CSPM_SetType (a1, a2, a3)) where
    cspm_compare (s1@(CSPM_Tuple3SetUnion {})) s2 = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_Tuple3SetUnion {})) = cspm_flipOrder (cspm_compare s1 s1)
    cspm_compare (CSPM_ExplicitTuple3Set a) (CSPM_ExplicitTuple3Set b) =
        if a == b then Just EQ
        else if S.isProperSubsetOf a b then Just LT
        else if S.isProperSubsetOf b a then Just GT
        else Nothing
    cspm_compare (CSPM_Tuple3Product a1 a2 a3) (CSPM_Tuple3Product b1 b2 b3) = cspm_set_product_order [cspm_compare a1 b1, cspm_compare a2 b2, cspm_compare a3 b3]
    cspm_compare s1 (s2@(CSPM_Tuple3Product {})) = cspm_compare s1 (cspm_set_make_explicit s2)
    cspm_compare (s1@(CSPM_Tuple3Product {})) s2 = cspm_flipOrder $ cspm_compare s2 s1
instance (CSPM_Show a1, CSPM_Show a2, CSPM_Show a3, CSPM_Show a4) => CSPM_Show (a1, a2, a3, a4) where
    cspm_show (a1, a2, a3, a4) = parens (cspm_prettyPrint_list [cspm_show a1, cspm_show a2, cspm_show a3, cspm_show a4])
instance (CSPM_Ord a1, CSPM_Ord a2, CSPM_Ord a3, CSPM_Ord a4) => CSPM_Ord (a1, a2, a3, a4) where
    cspm_compare (a1, a2, a3, a4) (b1, b2, b3, b4) = cspm_compare a1 b1 `cspm_thenCmp` cspm_compare a2 b2 `cspm_thenCmp` cspm_compare a3 b3 `cspm_thenCmp` cspm_compare a4 b4
instance  CSPM_Dot (a1, a2, a3, a4) a (CSPM_ExplicitDot (a1, a2, a3, a4) a) where
    cspm_dotOn x y = (CSPM_ExplicitDot x y)
instance (CSPM_Set a1, CSPM_Set a2, CSPM_Set a3, CSPM_Set a4) => CSPM_Set (a1, a2, a3, a4) where
    data CSPM_SetType (a1, a2, a3, a4) =
        CSPM_Tuple4Product (CSPM_SetType a1) (CSPM_SetType a2) (CSPM_SetType a3) (CSPM_SetType a4)
        | CSPM_ExplicitTuple4Set (S.Set (a1, a2, a3, a4))
        | CSPM_Tuple4SetUnion [CSPM_SetType (a1, a2, a3, a4)]
    cspm_set_card (CSPM_Tuple4Product a1 a2 a3 a4) = cspm_set_card a1 * cspm_set_card a2 * cspm_set_card a3 * cspm_set_card a4
    cspm_set_card (CSPM_ExplicitTuple4Set a) = S.size a
    cspm_set_card (CSPM_Tuple4SetUnion a) = sum (map cspm_set_card a)
    cspm_set_empty = CSPM_ExplicitTuple4Set S.empty
    cspm_set_diff (CSPM_ExplicitTuple4Set a) (CSPM_ExplicitTuple4Set b) = CSPM_ExplicitTuple4Set (S.difference a b)
    cspm_set_diff (CSPM_Tuple4SetUnion a) b = CSPM_Tuple4SetUnion $ map (\ s1 -> cspm_set_diff s1 b) a
    cspm_set_diff a (CSPM_Tuple4SetUnion b) = foldl cspm_set_diff a b
    cspm_set_diff (a@(CSPM_Tuple4Product {})) b = cspm_set_diff (cspm_set_make_explicit a) b
    cspm_set_diff a (b@(CSPM_Tuple4Product {})) = cspm_set_diff a (cspm_set_make_explicit b)
    cspm_set_fromList = CSPM_ExplicitTuple4Set . S.fromList
    cspm_set_inter (CSPM_ExplicitTuple4Set a) (CSPM_ExplicitTuple4Set b) = CSPM_ExplicitTuple4Set $ S.intersection a b
    cspm_set_inter (CSPM_Tuple4SetUnion a) b = CSPM_Tuple4SetUnion (map (cspm_set_inter b) a)
    cspm_set_inter a (CSPM_Tuple4SetUnion b) = CSPM_Tuple4SetUnion (map (cspm_set_inter a) b)
    cspm_set_inter (CSPM_Tuple4Product a1 a2 a3 a4) (CSPM_Tuple4Product b1 b2 b3 b4) = CSPM_Tuple4Product (cspm_set_inter a1 b1) (cspm_set_inter a2 b2) (cspm_set_inter a3 b3) (cspm_set_inter a4 b4)
    cspm_set_inter (a@(CSPM_Tuple4Product {})) b = cspm_set_inter (cspm_set_make_explicit a) b
    cspm_set_inter a (b@(CSPM_Tuple4Product {})) = cspm_set_inter (cspm_set_make_explicit b) a
    cspm_set_null (CSPM_Tuple4Product a1 a2 a3 a4) = cspm_set_null a1 && cspm_set_null a2 && cspm_set_null a3 && cspm_set_null a4
    cspm_set_null (CSPM_ExplicitTuple4Set a) = S.null a
    cspm_set_null (CSPM_Tuple4SetUnion a) = and (map cspm_set_null a)
    cspm_set_member v (CSPM_ExplicitTuple4Set a) = S.member v a
    cspm_set_member (a1, a2, a3, a4) (CSPM_Tuple4Product b1 b2 b3 b4) = cspm_set_member a1 b1 && cspm_set_member a2 b2 && cspm_set_member a3 b3 && cspm_set_member a4 b4
    cspm_set_member v (CSPM_Tuple4SetUnion a) = or (map (cspm_set_member v) a)
    cspm_set_toList (CSPM_ExplicitTuple4Set a) = S.toList a
    cspm_set_toList (CSPM_Tuple4Product b1 b2 b3 b4) = [(a1, a2, a3, a4) | a1 <- cspm_set_toList b1, a2 <- cspm_set_toList b2, a3 <- cspm_set_toList b3, a4 <- cspm_set_toList b4]
    cspm_set_toList (CSPM_Tuple4SetUnion a) = concatMap cspm_set_toList a
    cspm_set_union (CSPM_ExplicitTuple4Set a) (CSPM_ExplicitTuple4Set b) = CSPM_ExplicitTuple4Set $ S.union a b
    cspm_set_union (CSPM_Tuple4SetUnion a) (CSPM_Tuple4SetUnion b) = CSPM_Tuple4SetUnion (a ++ b)
    cspm_set_union (CSPM_Tuple4SetUnion a) b = CSPM_Tuple4SetUnion (b:a)
    cspm_set_union a (CSPM_Tuple4SetUnion b) = CSPM_Tuple4SetUnion (a:b)
    cspm_set_union (a@(CSPM_Tuple4Product {})) b = CSPM_Tuple4SetUnion [a, b]
    cspm_set_union a (b@(CSPM_Tuple4Product {})) = CSPM_Tuple4SetUnion [a, b]
instance (CSPM_Set a1, CSPM_Set a2, CSPM_Set a3, CSPM_Set a4) => CSPM_Show (CSPM_SetType (a1, a2, a3, a4)) where
    cspm_show (CSPM_Tuple4Product a1 a2 a3 a4) = parens $ cspm_prettyPrint_list [cspm_show a1, cspm_show a2, cspm_show a3, cspm_show a4]
    cspm_show (CSPM_Tuple4SetUnion a) = cspm_show_union_set a
    cspm_show (CSPM_ExplicitTuple4Set a) = braces $ cspm_prettyPrint_list $ map cspm_show $ S.toList a
instance (CSPM_Set a1, CSPM_Set a2, CSPM_Set a3, CSPM_Set a4, CSPM_Set (a1, a2, a3, a4)) => CSPM_Ord (CSPM_SetType (a1, a2, a3, a4)) where
    cspm_compare (s1@(CSPM_Tuple4SetUnion {})) s2 = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_Tuple4SetUnion {})) = cspm_flipOrder (cspm_compare s1 s1)
    cspm_compare (CSPM_ExplicitTuple4Set a) (CSPM_ExplicitTuple4Set b) =
        if a == b then Just EQ
        else if S.isProperSubsetOf a b then Just LT
        else if S.isProperSubsetOf b a then Just GT
        else Nothing
    cspm_compare (CSPM_Tuple4Product a1 a2 a3 a4) (CSPM_Tuple4Product b1 b2 b3 b4) = cspm_set_product_order [cspm_compare a1 b1, cspm_compare a2 b2, cspm_compare a3 b3, cspm_compare a4 b4]
    cspm_compare s1 (s2@(CSPM_Tuple4Product {})) = cspm_compare s1 (cspm_set_make_explicit s2)
    cspm_compare (s1@(CSPM_Tuple4Product {})) s2 = cspm_flipOrder $ cspm_compare s2 s1
instance (CSPM_Show a1, CSPM_Show a2, CSPM_Show a3, CSPM_Show a4, CSPM_Show a5) => CSPM_Show (a1, a2, a3, a4, a5) where
    cspm_show (a1, a2, a3, a4, a5) = parens (cspm_prettyPrint_list [cspm_show a1, cspm_show a2, cspm_show a3, cspm_show a4, cspm_show a5])
instance (CSPM_Ord a1, CSPM_Ord a2, CSPM_Ord a3, CSPM_Ord a4, CSPM_Ord a5) => CSPM_Ord (a1, a2, a3, a4, a5) where
    cspm_compare (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) = cspm_compare a1 b1 `cspm_thenCmp` cspm_compare a2 b2 `cspm_thenCmp` cspm_compare a3 b3 `cspm_thenCmp` cspm_compare a4 b4 `cspm_thenCmp` cspm_compare a5 b5
instance  CSPM_Dot (a1, a2, a3, a4, a5) a (CSPM_ExplicitDot (a1, a2, a3, a4, a5) a) where
    cspm_dotOn x y = (CSPM_ExplicitDot x y)
instance (CSPM_Set a1, CSPM_Set a2, CSPM_Set a3, CSPM_Set a4, CSPM_Set a5) => CSPM_Set (a1, a2, a3, a4, a5) where
    data CSPM_SetType (a1, a2, a3, a4, a5) =
        CSPM_Tuple5Product (CSPM_SetType a1) (CSPM_SetType a2) (CSPM_SetType a3) (CSPM_SetType a4) (CSPM_SetType a5)
        | CSPM_ExplicitTuple5Set (S.Set (a1, a2, a3, a4, a5))
        | CSPM_Tuple5SetUnion [CSPM_SetType (a1, a2, a3, a4, a5)]
    cspm_set_card (CSPM_Tuple5Product a1 a2 a3 a4 a5) = cspm_set_card a1 * cspm_set_card a2 * cspm_set_card a3 * cspm_set_card a4 * cspm_set_card a5
    cspm_set_card (CSPM_ExplicitTuple5Set a) = S.size a
    cspm_set_card (CSPM_Tuple5SetUnion a) = sum (map cspm_set_card a)
    cspm_set_empty = CSPM_ExplicitTuple5Set S.empty
    cspm_set_diff (CSPM_ExplicitTuple5Set a) (CSPM_ExplicitTuple5Set b) = CSPM_ExplicitTuple5Set (S.difference a b)
    cspm_set_diff (CSPM_Tuple5SetUnion a) b = CSPM_Tuple5SetUnion $ map (\ s1 -> cspm_set_diff s1 b) a
    cspm_set_diff a (CSPM_Tuple5SetUnion b) = foldl cspm_set_diff a b
    cspm_set_diff (a@(CSPM_Tuple5Product {})) b = cspm_set_diff (cspm_set_make_explicit a) b
    cspm_set_diff a (b@(CSPM_Tuple5Product {})) = cspm_set_diff a (cspm_set_make_explicit b)
    cspm_set_fromList = CSPM_ExplicitTuple5Set . S.fromList
    cspm_set_inter (CSPM_ExplicitTuple5Set a) (CSPM_ExplicitTuple5Set b) = CSPM_ExplicitTuple5Set $ S.intersection a b
    cspm_set_inter (CSPM_Tuple5SetUnion a) b = CSPM_Tuple5SetUnion (map (cspm_set_inter b) a)
    cspm_set_inter a (CSPM_Tuple5SetUnion b) = CSPM_Tuple5SetUnion (map (cspm_set_inter a) b)
    cspm_set_inter (CSPM_Tuple5Product a1 a2 a3 a4 a5) (CSPM_Tuple5Product b1 b2 b3 b4 b5) = CSPM_Tuple5Product (cspm_set_inter a1 b1) (cspm_set_inter a2 b2) (cspm_set_inter a3 b3) (cspm_set_inter a4 b4) (cspm_set_inter a5 b5)
    cspm_set_inter (a@(CSPM_Tuple5Product {})) b = cspm_set_inter (cspm_set_make_explicit a) b
    cspm_set_inter a (b@(CSPM_Tuple5Product {})) = cspm_set_inter (cspm_set_make_explicit b) a
    cspm_set_null (CSPM_Tuple5Product a1 a2 a3 a4 a5) = cspm_set_null a1 && cspm_set_null a2 && cspm_set_null a3 && cspm_set_null a4 && cspm_set_null a5
    cspm_set_null (CSPM_ExplicitTuple5Set a) = S.null a
    cspm_set_null (CSPM_Tuple5SetUnion a) = and (map cspm_set_null a)
    cspm_set_member v (CSPM_ExplicitTuple5Set a) = S.member v a
    cspm_set_member (a1, a2, a3, a4, a5) (CSPM_Tuple5Product b1 b2 b3 b4 b5) = cspm_set_member a1 b1 && cspm_set_member a2 b2 && cspm_set_member a3 b3 && cspm_set_member a4 b4 && cspm_set_member a5 b5
    cspm_set_member v (CSPM_Tuple5SetUnion a) = or (map (cspm_set_member v) a)
    cspm_set_toList (CSPM_ExplicitTuple5Set a) = S.toList a
    cspm_set_toList (CSPM_Tuple5Product b1 b2 b3 b4 b5) = [(a1, a2, a3, a4, a5) | a1 <- cspm_set_toList b1, a2 <- cspm_set_toList b2, a3 <- cspm_set_toList b3, a4 <- cspm_set_toList b4, a5 <- cspm_set_toList b5]
    cspm_set_toList (CSPM_Tuple5SetUnion a) = concatMap cspm_set_toList a
    cspm_set_union (CSPM_ExplicitTuple5Set a) (CSPM_ExplicitTuple5Set b) = CSPM_ExplicitTuple5Set $ S.union a b
    cspm_set_union (CSPM_Tuple5SetUnion a) (CSPM_Tuple5SetUnion b) = CSPM_Tuple5SetUnion (a ++ b)
    cspm_set_union (CSPM_Tuple5SetUnion a) b = CSPM_Tuple5SetUnion (b:a)
    cspm_set_union a (CSPM_Tuple5SetUnion b) = CSPM_Tuple5SetUnion (a:b)
    cspm_set_union (a@(CSPM_Tuple5Product {})) b = CSPM_Tuple5SetUnion [a, b]
    cspm_set_union a (b@(CSPM_Tuple5Product {})) = CSPM_Tuple5SetUnion [a, b]
instance (CSPM_Set a1, CSPM_Set a2, CSPM_Set a3, CSPM_Set a4, CSPM_Set a5) => CSPM_Show (CSPM_SetType (a1, a2, a3, a4, a5)) where
    cspm_show (CSPM_Tuple5Product a1 a2 a3 a4 a5) = parens $ cspm_prettyPrint_list [cspm_show a1, cspm_show a2, cspm_show a3, cspm_show a4, cspm_show a5]
    cspm_show (CSPM_Tuple5SetUnion a) = cspm_show_union_set a
    cspm_show (CSPM_ExplicitTuple5Set a) = braces $ cspm_prettyPrint_list $ map cspm_show $ S.toList a
instance (CSPM_Set a1, CSPM_Set a2, CSPM_Set a3, CSPM_Set a4, CSPM_Set a5, CSPM_Set (a1, a2, a3, a4, a5)) => CSPM_Ord (CSPM_SetType (a1, a2, a3, a4, a5)) where
    cspm_compare (s1@(CSPM_Tuple5SetUnion {})) s2 = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_Tuple5SetUnion {})) = cspm_flipOrder (cspm_compare s1 s1)
    cspm_compare (CSPM_ExplicitTuple5Set a) (CSPM_ExplicitTuple5Set b) =
        if a == b then Just EQ
        else if S.isProperSubsetOf a b then Just LT
        else if S.isProperSubsetOf b a then Just GT
        else Nothing
    cspm_compare (CSPM_Tuple5Product a1 a2 a3 a4 a5) (CSPM_Tuple5Product b1 b2 b3 b4 b5) = cspm_set_product_order [cspm_compare a1 b1, cspm_compare a2 b2, cspm_compare a3 b3, cspm_compare a4 b4, cspm_compare a5 b5]
    cspm_compare s1 (s2@(CSPM_Tuple5Product {})) = cspm_compare s1 (cspm_set_make_explicit s2)
    cspm_compare (s1@(CSPM_Tuple5Product {})) s2 = cspm_flipOrder $ cspm_compare s2 s1
data Type_Gen_1714 = Gen_1715_Remaining0 | Gen_1716_Remaining0 deriving (Eq, Ord, Typeable)
instance CSPM_Show (Type_Gen_1714) where
    cspm_show (Gen_1715_Remaining0) = cspm_prettyPrint_dotSep [text "A"]
    cspm_show (Gen_1716_Remaining0) = cspm_prettyPrint_dotSep [text "B"]
instance CSPM_Set Type_Gen_1714 where
    data CSPM_SetType Type_Gen_1714 =
        CSPM_ExplicitType_Gen_1714Set (S.Set Type_Gen_1714) |
        CSPM_Type_Gen_1714SetUnion [CSPM_SetType Type_Gen_1714]
    cspm_set_card (CSPM_ExplicitType_Gen_1714Set a) = S.size a
    cspm_set_card (CSPM_Type_Gen_1714SetUnion a) = sum (map cspm_set_card a)
    cspm_set_empty = CSPM_ExplicitType_Gen_1714Set S.empty
    cspm_set_diff (CSPM_ExplicitType_Gen_1714Set a) (CSPM_ExplicitType_Gen_1714Set b) = CSPM_ExplicitType_Gen_1714Set (S.difference a b)
    cspm_set_diff (CSPM_Type_Gen_1714SetUnion a) b = CSPM_Type_Gen_1714SetUnion $ map (\ s1 -> cspm_set_diff s1 b) a
    cspm_set_diff a (CSPM_Type_Gen_1714SetUnion b) = foldl cspm_set_diff a b
    cspm_set_diff (a@(CSPM_ExplicitType_Gen_1714Set {})) b = cspm_set_diff a (cspm_set_make_explicit b)
    cspm_set_diff a (b@(CSPM_ExplicitType_Gen_1714Set {})) = cspm_set_diff (cspm_set_make_explicit a) b
    cspm_set_fromList = CSPM_ExplicitType_Gen_1714Set . S.fromList
    cspm_set_inter (CSPM_ExplicitType_Gen_1714Set a) (CSPM_ExplicitType_Gen_1714Set b) = CSPM_ExplicitType_Gen_1714Set $ S.intersection a b
    cspm_set_inter (CSPM_Type_Gen_1714SetUnion a) b = CSPM_Type_Gen_1714SetUnion (map (cspm_set_inter b) a)
    cspm_set_inter a (CSPM_Type_Gen_1714SetUnion b) = CSPM_Type_Gen_1714SetUnion (map (cspm_set_inter a) b)
    cspm_set_inter (a@(CSPM_ExplicitType_Gen_1714Set {})) b = cspm_set_inter a (cspm_set_make_explicit b)
    cspm_set_inter a (b@(CSPM_ExplicitType_Gen_1714Set {})) = cspm_set_inter (cspm_set_make_explicit a) b
    cspm_set_null (CSPM_ExplicitType_Gen_1714Set a) = S.null a
    cspm_set_null (CSPM_Type_Gen_1714SetUnion a) = and (map cspm_set_null a)
    cspm_set_member v (CSPM_ExplicitType_Gen_1714Set a) = S.member v a
    cspm_set_member v (CSPM_Type_Gen_1714SetUnion a) = or (map (cspm_set_member v) a)
    cspm_set_toList (CSPM_ExplicitType_Gen_1714Set a) = S.toList a
    cspm_set_toList (CSPM_Type_Gen_1714SetUnion a) = concatMap cspm_set_toList a
    cspm_set_union (CSPM_ExplicitType_Gen_1714Set a) (CSPM_ExplicitType_Gen_1714Set b) = CSPM_ExplicitType_Gen_1714Set $ S.union a b
    cspm_set_union (CSPM_Type_Gen_1714SetUnion a) (CSPM_Type_Gen_1714SetUnion b) = CSPM_Type_Gen_1714SetUnion (a ++ b)
    cspm_set_union (CSPM_Type_Gen_1714SetUnion a) b = CSPM_Type_Gen_1714SetUnion (b:a)
    cspm_set_union a (CSPM_Type_Gen_1714SetUnion b) = CSPM_Type_Gen_1714SetUnion (a:b)
instance CSPM_Show (CSPM_SetType Type_Gen_1714) where
    cspm_show (CSPM_Type_Gen_1714SetUnion a) = cspm_show_union_set a
    cspm_show (CSPM_ExplicitType_Gen_1714Set a) = braces $ cspm_prettyPrint_list $ map cspm_show $ S.toList a
instance CSPM_Ord (CSPM_SetType Type_Gen_1714) where
    cspm_compare (s1@(CSPM_Type_Gen_1714SetUnion {})) s2 = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_Type_Gen_1714SetUnion {})) = cspm_flipOrder (cspm_compare s1 s1)
    cspm_compare (CSPM_ExplicitType_Gen_1714Set a) (CSPM_ExplicitType_Gen_1714Set b) =
        if a == b then Just EQ
        else if S.isProperSubsetOf a b then Just LT
        else if S.isProperSubsetOf b a then Just GT
        else Nothing
    cspm_compare s1 (s2@(CSPM_ExplicitType_Gen_1714Set {})) = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare (s1@(CSPM_ExplicitType_Gen_1714Set {})) s2 = cspm_flipOrder $ cspm_compare s2 s1
instance CSPM_Productions (Type_Gen_1714) (Type_Gen_1714) where
    cspm_productions x = cspm_set_fromList [x]
gen_1714 :: (CSPM_SetType Type_Gen_1714)
gen_1714 = CSPM_Type_Gen_1714SetUnion [cspm_set_fromList [Gen_1715_Remaining0], cspm_set_fromList [Gen_1716_Remaining0]]
instance CSPM_Dot (Type_Gen_1714) (a) (CSPM_ExplicitDot Type_Gen_1714 a) where
    cspm_dotOn x y = CSPM_ExplicitDot x y
gen_1715_Default :: Type_Gen_1714
gen_1715_Default = Gen_1715_Remaining0
gen_1716_Default :: Type_Gen_1714
gen_1716_Default = Gen_1716_Remaining0
gen_1708 :: Bool
gen_1708 = ((gen_21 :: (CSPM_SetType Type_Gen_21)) == (cspm_set_unions [cspm_productions (gen_1712_Default :: Type_Gen_21), cspm_productions (gen_1713_Default :: (CSPM_Yield Int Type_Gen_21 Int Type_Gen_21)), cspm_productions (gen_1717_Default :: (CSPM_Yield Type_Gen_1714 Type_Gen_21 Type_Gen_1714 Type_Gen_21))] :: (CSPM_SetType Type_Gen_21)) :: Bool)
gen_1709 :: Bool
gen_1709 = (((cspm_set_member :: (Type_Gen_21 -> (CSPM_SetType Type_Gen_21) -> Bool)) (Gen_1717_Remaining0 (gen_1715_Default :: Type_Gen_1714) :: Type_Gen_21) (gen_21 :: (CSPM_SetType Type_Gen_21)) :: Bool) == (True :: Bool) :: Bool)
gen_1710 :: Bool
gen_1710 = (let
                gen_1711 :: Int
                gen_1711 = (1 :: Int)
            in (((cspm_set_member :: (Type_Gen_21 -> (CSPM_SetType Type_Gen_21) -> Bool)) (Gen_1717_Remaining0 (gen_1715_Default :: Type_Gen_1714) :: Type_Gen_21) (gen_21 :: (CSPM_SetType Type_Gen_21)) :: Bool) == (True :: Bool) :: Bool) :: Bool)
data Type_Gen_21 = Gen_27_Remaining0 | Gen_28_Remaining0 | Gen_1712_Remaining0 | Gen_1713_Remaining0 Int | Gen_1717_Remaining0 Type_Gen_1714 deriving (Eq, Ord, Typeable)
instance CSPM_Show (Type_Gen_21) where
    cspm_show (Gen_27_Remaining0) = char 'τ'
    cspm_show (Gen_28_Remaining0) = char '✓'
    cspm_show (Gen_1712_Remaining0) = cspm_prettyPrint_dotSep [text "a"]
    cspm_show (Gen_1713_Remaining0 f1) = cspm_prettyPrint_dotSep [text "b", cspm_show f1]
    cspm_show (Gen_1717_Remaining0 f1) = cspm_prettyPrint_dotSep [text "c", cspm_show f1]
instance CSPM_Set Type_Gen_21 where
    data CSPM_SetType Type_Gen_21 =
        CSPM_ExplicitType_Gen_21Set (S.Set Type_Gen_21) |
        CSPM_Type_Gen_21SetUnion [CSPM_SetType Type_Gen_21] |
        CSPM_Type_Gen_21_Type_Gen_1713_ProductSet (CSPM_SetType Int) |
        CSPM_Type_Gen_21_Type_Gen_1717_ProductSet (CSPM_SetType Type_Gen_1714)
    cspm_set_card (CSPM_ExplicitType_Gen_21Set a) = S.size a
    cspm_set_card (CSPM_Type_Gen_21SetUnion a) = sum (map cspm_set_card a)
    cspm_set_card (CSPM_Type_Gen_21_Type_Gen_1713_ProductSet a1) = cspm_set_card a1
    cspm_set_card (CSPM_Type_Gen_21_Type_Gen_1717_ProductSet a1) = cspm_set_card a1
    cspm_set_empty = CSPM_ExplicitType_Gen_21Set S.empty
    cspm_set_diff (CSPM_ExplicitType_Gen_21Set a) (CSPM_ExplicitType_Gen_21Set b) = CSPM_ExplicitType_Gen_21Set (S.difference a b)
    cspm_set_diff (CSPM_Type_Gen_21SetUnion a) b = CSPM_Type_Gen_21SetUnion $ map (\ s1 -> cspm_set_diff s1 b) a
    cspm_set_diff a (CSPM_Type_Gen_21SetUnion b) = foldl cspm_set_diff a b
    cspm_set_diff (a@(CSPM_ExplicitType_Gen_21Set {})) b = cspm_set_diff a (cspm_set_make_explicit b)
    cspm_set_diff a (b@(CSPM_ExplicitType_Gen_21Set {})) = cspm_set_diff (cspm_set_make_explicit a) b
    cspm_set_diff a b = cspm_set_diff (cspm_set_make_explicit a) (cspm_set_make_explicit b)
    cspm_set_fromList = CSPM_ExplicitType_Gen_21Set . S.fromList
    cspm_set_inter (CSPM_ExplicitType_Gen_21Set a) (CSPM_ExplicitType_Gen_21Set b) = CSPM_ExplicitType_Gen_21Set $ S.intersection a b
    cspm_set_inter (CSPM_Type_Gen_21SetUnion a) b = CSPM_Type_Gen_21SetUnion (map (cspm_set_inter b) a)
    cspm_set_inter a (CSPM_Type_Gen_21SetUnion b) = CSPM_Type_Gen_21SetUnion (map (cspm_set_inter a) b)
    cspm_set_inter (a@(CSPM_ExplicitType_Gen_21Set {})) b = cspm_set_inter a (cspm_set_make_explicit b)
    cspm_set_inter a (b@(CSPM_ExplicitType_Gen_21Set {})) = cspm_set_inter (cspm_set_make_explicit a) b
    cspm_set_inter a b = cspm_set_inter (cspm_set_make_explicit a) (cspm_set_make_explicit b)
    cspm_set_null (CSPM_ExplicitType_Gen_21Set a) = S.null a
    cspm_set_null (CSPM_Type_Gen_21SetUnion a) = and (map cspm_set_null a)
    cspm_set_null (CSPM_Type_Gen_21_Type_Gen_1713_ProductSet a1) = cspm_set_null a1
    cspm_set_null (CSPM_Type_Gen_21_Type_Gen_1717_ProductSet a1) = cspm_set_null a1
    cspm_set_member v (CSPM_ExplicitType_Gen_21Set a) = S.member v a
    cspm_set_member v (CSPM_Type_Gen_21SetUnion a) = or (map (cspm_set_member v) a)
    cspm_set_member (Gen_1713_Remaining0 a1) (CSPM_Type_Gen_21_Type_Gen_1713_ProductSet b1) = cspm_set_member a1 b1
    cspm_set_member (Gen_1717_Remaining0 a1) (CSPM_Type_Gen_21_Type_Gen_1717_ProductSet b1) = cspm_set_member a1 b1
    cspm_set_member _ _ = False
    cspm_set_toList (CSPM_ExplicitType_Gen_21Set a) = S.toList a
    cspm_set_toList (CSPM_Type_Gen_21SetUnion a) = concatMap cspm_set_toList a
    cspm_set_toList (CSPM_Type_Gen_21_Type_Gen_1713_ProductSet b1) = [(Gen_1713_Remaining0 a1) | a1 <- cspm_set_toList b1]
    cspm_set_toList (CSPM_Type_Gen_21_Type_Gen_1717_ProductSet b1) = [(Gen_1717_Remaining0 a1) | a1 <- cspm_set_toList b1]
    cspm_set_union (CSPM_ExplicitType_Gen_21Set a) (CSPM_ExplicitType_Gen_21Set b) = CSPM_ExplicitType_Gen_21Set $ S.union a b
    cspm_set_union (CSPM_Type_Gen_21SetUnion a) (CSPM_Type_Gen_21SetUnion b) = CSPM_Type_Gen_21SetUnion (a ++ b)
    cspm_set_union (CSPM_Type_Gen_21SetUnion a) b = CSPM_Type_Gen_21SetUnion (b:a)
    cspm_set_union a (CSPM_Type_Gen_21SetUnion b) = CSPM_Type_Gen_21SetUnion (a:b)
    cspm_set_union a b = CSPM_Type_Gen_21SetUnion [a, b]
instance CSPM_Show (CSPM_SetType Type_Gen_21) where
    cspm_show (CSPM_Type_Gen_21SetUnion a) = cspm_show_union_set a
    cspm_show (CSPM_ExplicitType_Gen_21Set a) = braces $ cspm_prettyPrint_list $ map cspm_show $ S.toList a
    cspm_show (CSPM_Type_Gen_21_Type_Gen_1713_ProductSet a1) = cspm_prettyPrint_dotSep $ text "b" : [cspm_show a1]
    cspm_show (CSPM_Type_Gen_21_Type_Gen_1717_ProductSet a1) = cspm_prettyPrint_dotSep $ text "c" : [cspm_show a1]
instance CSPM_Ord (CSPM_SetType Type_Gen_21) where
    cspm_compare (s1@(CSPM_Type_Gen_21SetUnion {})) s2 = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_Type_Gen_21SetUnion {})) = cspm_flipOrder (cspm_compare s1 s1)
    cspm_compare (CSPM_ExplicitType_Gen_21Set a) (CSPM_ExplicitType_Gen_21Set b) =
        if a == b then Just EQ
        else if S.isProperSubsetOf a b then Just LT
        else if S.isProperSubsetOf b a then Just GT
        else Nothing
    cspm_compare (CSPM_Type_Gen_21_Type_Gen_1713_ProductSet a1) (CSPM_Type_Gen_21_Type_Gen_1713_ProductSet b1) = cspm_set_product_order [cspm_compare a1 b1]
    cspm_compare (CSPM_Type_Gen_21_Type_Gen_1717_ProductSet a1) (CSPM_Type_Gen_21_Type_Gen_1717_ProductSet b1) = cspm_set_product_order [cspm_compare a1 b1]
    cspm_compare s1 (s2@(CSPM_ExplicitType_Gen_21Set {})) = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare (s1@(CSPM_ExplicitType_Gen_21Set {})) s2 = cspm_flipOrder $ cspm_compare s2 s1
    cspm_compare s1 s2 = cspm_compare (cspm_set_make_explicit s1) (cspm_set_make_explicit s2)
instance CSPM_Productions (Type_Gen_21) (Type_Gen_21) where
    cspm_productions x = cspm_set_fromList [x]
data Type_Gen_1719 = Gen_1713_Remaining1 deriving (Eq, Ord, Typeable)
instance CSPM_Show (Type_Gen_1719) where
    cspm_show (Gen_1713_Remaining1) = cspm_prettyPrint_dotSep [text "b"]
instance CSPM_Set Type_Gen_1719 where
    data CSPM_SetType Type_Gen_1719 =
        CSPM_ExplicitType_Gen_1719Set (S.Set Type_Gen_1719) |
        CSPM_Type_Gen_1719SetUnion [CSPM_SetType Type_Gen_1719]
    cspm_set_card (CSPM_ExplicitType_Gen_1719Set a) = S.size a
    cspm_set_card (CSPM_Type_Gen_1719SetUnion a) = sum (map cspm_set_card a)
    cspm_set_empty = CSPM_ExplicitType_Gen_1719Set S.empty
    cspm_set_diff (CSPM_ExplicitType_Gen_1719Set a) (CSPM_ExplicitType_Gen_1719Set b) = CSPM_ExplicitType_Gen_1719Set (S.difference a b)
    cspm_set_diff (CSPM_Type_Gen_1719SetUnion a) b = CSPM_Type_Gen_1719SetUnion $ map (\ s1 -> cspm_set_diff s1 b) a
    cspm_set_diff a (CSPM_Type_Gen_1719SetUnion b) = foldl cspm_set_diff a b
    cspm_set_diff (a@(CSPM_ExplicitType_Gen_1719Set {})) b = cspm_set_diff a (cspm_set_make_explicit b)
    cspm_set_diff a (b@(CSPM_ExplicitType_Gen_1719Set {})) = cspm_set_diff (cspm_set_make_explicit a) b
    cspm_set_fromList = CSPM_ExplicitType_Gen_1719Set . S.fromList
    cspm_set_inter (CSPM_ExplicitType_Gen_1719Set a) (CSPM_ExplicitType_Gen_1719Set b) = CSPM_ExplicitType_Gen_1719Set $ S.intersection a b
    cspm_set_inter (CSPM_Type_Gen_1719SetUnion a) b = CSPM_Type_Gen_1719SetUnion (map (cspm_set_inter b) a)
    cspm_set_inter a (CSPM_Type_Gen_1719SetUnion b) = CSPM_Type_Gen_1719SetUnion (map (cspm_set_inter a) b)
    cspm_set_inter (a@(CSPM_ExplicitType_Gen_1719Set {})) b = cspm_set_inter a (cspm_set_make_explicit b)
    cspm_set_inter a (b@(CSPM_ExplicitType_Gen_1719Set {})) = cspm_set_inter (cspm_set_make_explicit a) b
    cspm_set_null (CSPM_ExplicitType_Gen_1719Set a) = S.null a
    cspm_set_null (CSPM_Type_Gen_1719SetUnion a) = and (map cspm_set_null a)
    cspm_set_member v (CSPM_ExplicitType_Gen_1719Set a) = S.member v a
    cspm_set_member v (CSPM_Type_Gen_1719SetUnion a) = or (map (cspm_set_member v) a)
    cspm_set_toList (CSPM_ExplicitType_Gen_1719Set a) = S.toList a
    cspm_set_toList (CSPM_Type_Gen_1719SetUnion a) = concatMap cspm_set_toList a
    cspm_set_union (CSPM_ExplicitType_Gen_1719Set a) (CSPM_ExplicitType_Gen_1719Set b) = CSPM_ExplicitType_Gen_1719Set $ S.union a b
    cspm_set_union (CSPM_Type_Gen_1719SetUnion a) (CSPM_Type_Gen_1719SetUnion b) = CSPM_Type_Gen_1719SetUnion (a ++ b)
    cspm_set_union (CSPM_Type_Gen_1719SetUnion a) b = CSPM_Type_Gen_1719SetUnion (b:a)
    cspm_set_union a (CSPM_Type_Gen_1719SetUnion b) = CSPM_Type_Gen_1719SetUnion (a:b)
instance CSPM_Show (CSPM_SetType Type_Gen_1719) where
    cspm_show (CSPM_Type_Gen_1719SetUnion a) = cspm_show_union_set a
    cspm_show (CSPM_ExplicitType_Gen_1719Set a) = braces $ cspm_prettyPrint_list $ map cspm_show $ S.toList a
instance CSPM_Ord (CSPM_SetType Type_Gen_1719) where
    cspm_compare (s1@(CSPM_Type_Gen_1719SetUnion {})) s2 = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_Type_Gen_1719SetUnion {})) = cspm_flipOrder (cspm_compare s1 s1)
    cspm_compare (CSPM_ExplicitType_Gen_1719Set a) (CSPM_ExplicitType_Gen_1719Set b) =
        if a == b then Just EQ
        else if S.isProperSubsetOf a b then Just LT
        else if S.isProperSubsetOf b a then Just GT
        else Nothing
    cspm_compare s1 (s2@(CSPM_ExplicitType_Gen_1719Set {})) = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare (s1@(CSPM_ExplicitType_Gen_1719Set {})) s2 = cspm_flipOrder $ cspm_compare s2 s1
instance CSPM_Dot Type_Gen_1719 (Int) Type_Gen_21 where
    cspm_dotOn a@(Gen_1713_Remaining1) b = if gen_1713_Default_isValid_0 b then Gen_1713_Remaining0 b else cspm_panic (text "<unknown location>") (hang (text "The value:") 4 (cspm_show b) $$ text "is invalid as it is not valid on the right of" <+> cspm_show a)
instance CSPM_Productions (Type_Gen_1719) (Type_Gen_21) where
    cspm_productions (Gen_1713_Remaining1) = CSPM_Type_Gen_21_Type_Gen_1713_ProductSet (gen_1713_Default_field_0)
instance CSPM_RestrictedProductions (Type_Gen_1719) (Int) (Type_Gen_21) where
    cspm_restricted_productions (Gen_1713_Remaining1) b = CSPM_Type_Gen_21_Type_Gen_1713_ProductSet (cspm_set_inter b gen_1713_Default_field_0)
instance CSPM_DropPrefix (Type_Gen_1719) (Type_Gen_21) (Int) where
    cspm_dropPrefix (Gen_1713_Remaining1) s = case s of
        CSPM_Type_Gen_21_Type_Gen_1713_ProductSet (a1) -> a1
        _ -> cspm_set_fromList $ map (\  (Gen_1713_Remaining0 a1) -> a1 ) $ cspm_set_toList s
data Type_Gen_1718 = Gen_1717_Remaining1 deriving (Eq, Ord, Typeable)
instance CSPM_Show (Type_Gen_1718) where
    cspm_show (Gen_1717_Remaining1) = cspm_prettyPrint_dotSep [text "c"]
instance CSPM_Set Type_Gen_1718 where
    data CSPM_SetType Type_Gen_1718 =
        CSPM_ExplicitType_Gen_1718Set (S.Set Type_Gen_1718) |
        CSPM_Type_Gen_1718SetUnion [CSPM_SetType Type_Gen_1718]
    cspm_set_card (CSPM_ExplicitType_Gen_1718Set a) = S.size a
    cspm_set_card (CSPM_Type_Gen_1718SetUnion a) = sum (map cspm_set_card a)
    cspm_set_empty = CSPM_ExplicitType_Gen_1718Set S.empty
    cspm_set_diff (CSPM_ExplicitType_Gen_1718Set a) (CSPM_ExplicitType_Gen_1718Set b) = CSPM_ExplicitType_Gen_1718Set (S.difference a b)
    cspm_set_diff (CSPM_Type_Gen_1718SetUnion a) b = CSPM_Type_Gen_1718SetUnion $ map (\ s1 -> cspm_set_diff s1 b) a
    cspm_set_diff a (CSPM_Type_Gen_1718SetUnion b) = foldl cspm_set_diff a b
    cspm_set_diff (a@(CSPM_ExplicitType_Gen_1718Set {})) b = cspm_set_diff a (cspm_set_make_explicit b)
    cspm_set_diff a (b@(CSPM_ExplicitType_Gen_1718Set {})) = cspm_set_diff (cspm_set_make_explicit a) b
    cspm_set_fromList = CSPM_ExplicitType_Gen_1718Set . S.fromList
    cspm_set_inter (CSPM_ExplicitType_Gen_1718Set a) (CSPM_ExplicitType_Gen_1718Set b) = CSPM_ExplicitType_Gen_1718Set $ S.intersection a b
    cspm_set_inter (CSPM_Type_Gen_1718SetUnion a) b = CSPM_Type_Gen_1718SetUnion (map (cspm_set_inter b) a)
    cspm_set_inter a (CSPM_Type_Gen_1718SetUnion b) = CSPM_Type_Gen_1718SetUnion (map (cspm_set_inter a) b)
    cspm_set_inter (a@(CSPM_ExplicitType_Gen_1718Set {})) b = cspm_set_inter a (cspm_set_make_explicit b)
    cspm_set_inter a (b@(CSPM_ExplicitType_Gen_1718Set {})) = cspm_set_inter (cspm_set_make_explicit a) b
    cspm_set_null (CSPM_ExplicitType_Gen_1718Set a) = S.null a
    cspm_set_null (CSPM_Type_Gen_1718SetUnion a) = and (map cspm_set_null a)
    cspm_set_member v (CSPM_ExplicitType_Gen_1718Set a) = S.member v a
    cspm_set_member v (CSPM_Type_Gen_1718SetUnion a) = or (map (cspm_set_member v) a)
    cspm_set_toList (CSPM_ExplicitType_Gen_1718Set a) = S.toList a
    cspm_set_toList (CSPM_Type_Gen_1718SetUnion a) = concatMap cspm_set_toList a
    cspm_set_union (CSPM_ExplicitType_Gen_1718Set a) (CSPM_ExplicitType_Gen_1718Set b) = CSPM_ExplicitType_Gen_1718Set $ S.union a b
    cspm_set_union (CSPM_Type_Gen_1718SetUnion a) (CSPM_Type_Gen_1718SetUnion b) = CSPM_Type_Gen_1718SetUnion (a ++ b)
    cspm_set_union (CSPM_Type_Gen_1718SetUnion a) b = CSPM_Type_Gen_1718SetUnion (b:a)
    cspm_set_union a (CSPM_Type_Gen_1718SetUnion b) = CSPM_Type_Gen_1718SetUnion (a:b)
instance CSPM_Show (CSPM_SetType Type_Gen_1718) where
    cspm_show (CSPM_Type_Gen_1718SetUnion a) = cspm_show_union_set a
    cspm_show (CSPM_ExplicitType_Gen_1718Set a) = braces $ cspm_prettyPrint_list $ map cspm_show $ S.toList a
instance CSPM_Ord (CSPM_SetType Type_Gen_1718) where
    cspm_compare (s1@(CSPM_Type_Gen_1718SetUnion {})) s2 = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare s1 (s2@(CSPM_Type_Gen_1718SetUnion {})) = cspm_flipOrder (cspm_compare s1 s1)
    cspm_compare (CSPM_ExplicitType_Gen_1718Set a) (CSPM_ExplicitType_Gen_1718Set b) =
        if a == b then Just EQ
        else if S.isProperSubsetOf a b then Just LT
        else if S.isProperSubsetOf b a then Just GT
        else Nothing
    cspm_compare s1 (s2@(CSPM_ExplicitType_Gen_1718Set {})) = cspm_compare (cspm_set_make_explicit s1) s2
    cspm_compare (s1@(CSPM_ExplicitType_Gen_1718Set {})) s2 = cspm_flipOrder $ cspm_compare s2 s1
instance CSPM_Dot Type_Gen_1718 (Type_Gen_1714) Type_Gen_21 where
    cspm_dotOn a@(Gen_1717_Remaining1) b = Gen_1717_Remaining0 b
instance CSPM_Productions (Type_Gen_1718) (Type_Gen_21) where
    cspm_productions (Gen_1717_Remaining1) = CSPM_Type_Gen_21_Type_Gen_1717_ProductSet (gen_1717_Default_field_0)
instance CSPM_RestrictedProductions (Type_Gen_1718) (Type_Gen_1714) (Type_Gen_21) where
    cspm_restricted_productions (Gen_1717_Remaining1) b = CSPM_Type_Gen_21_Type_Gen_1717_ProductSet (b)
instance CSPM_DropPrefix (Type_Gen_1718) (Type_Gen_21) (Type_Gen_1714) where
    cspm_dropPrefix (Gen_1717_Remaining1) s = case s of
        CSPM_Type_Gen_21_Type_Gen_1717_ProductSet (a1) -> a1
        _ -> cspm_set_fromList $ map (\  (Gen_1717_Remaining0 a1) -> a1 ) $ cspm_set_toList s
gen_1713_Default_field_0 :: (CSPM_SetType Int)
gen_1713_Default_field_0 = (cspm_set_fromList [(0 :: Int), (1 :: Int), (2 :: Int)] :: (CSPM_SetType Int))
gen_1713_Default_isValid_0 :: Int -> Bool
gen_1713_Default_isValid_0 x = cspm_set_member x gen_1713_Default_field_0
gen_1717_Default_field_0 :: (CSPM_SetType Type_Gen_1714)
gen_1717_Default_field_0 = (gen_1714 :: (CSPM_SetType Type_Gen_1714))
gen_1717_Default_isValid_0 :: Type_Gen_1714 -> Bool
gen_1717_Default_isValid_0 x = True
gen_21 :: (CSPM_SetType Type_Gen_21)
gen_21 = CSPM_Type_Gen_21SetUnion [cspm_set_fromList [Gen_1712_Remaining0], CSPM_Type_Gen_21_Type_Gen_1713_ProductSet gen_1713_Default_field_0, CSPM_Type_Gen_21_Type_Gen_1717_ProductSet gen_1717_Default_field_0]
instance CSPM_Dot (Type_Gen_21) (a) (CSPM_ExplicitDot Type_Gen_21 a) where
    cspm_dotOn x y = CSPM_ExplicitDot x y
gen_27 :: Type_Gen_21
gen_27 = Gen_27_Remaining0
gen_28 :: Type_Gen_21
gen_28 = Gen_28_Remaining0
gen_1712_Default :: Type_Gen_21
gen_1712_Default = Gen_1712_Remaining0
gen_1713_Default :: (CSPM_Yield Int Type_Gen_21 Int Type_Gen_21)
gen_1713_Default = CSPM_Yield Gen_1713_Remaining1
gen_1717_Default :: (CSPM_Yield Type_Gen_1714 Type_Gen_21 Type_Gen_1714 Type_Gen_21)
gen_1717_Default = CSPM_Yield Gen_1717_Remaining1