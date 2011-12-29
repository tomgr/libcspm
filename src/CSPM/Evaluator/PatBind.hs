{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CSPM.Evaluator.PatBind where

import Control.Monad

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import Util.Annotated
import Util.Exception

-- Bind :: Pattern, value -> (Matches Pattern, Values to Bind
class Bindable a where
    bind :: a -> Value -> (Bool, [(Name, Value)])

instance Bindable a => Bindable (Annotated b a) where
    bind (An _ _ a) v = bind a v

instance Bindable (Pat Name) where
    -- We can decompose any PConcat pattern into three patterns representing:
    -- Begining (of the form PList), middle (either PWildcard or PVar)
    -- and end (of the form PList), With both begining and end possible empty
    bind (PCompList ps Nothing _) (VList xs) | length ps == length xs = 
        bindAll ps xs
    -- By desugaring the middle is not a PConcat or a PList
    bind (PCompList starts (Just (middle, ends)) _) (VList xs) =
        -- Only match if the list contains sufficient items
        if not (atLeastLength (length starts + length ends) xs) then 
            (False, [])
        else
            let
                (b1, nvs1) = bindAll starts xsStart
                (b2, nvs2) = bindAll ends xsEnd
                (b3, nvs3) = bind middle (VList xsMiddle)
            in (b1 && b2 && b3, nvs1++nvs2++nvs3)
        where
            atLeastLength 0 _ = True
            atLeastLength _ [] = False
            atLeastLength n (x:xs) = atLeastLength (n-1) xs
            (xsStart, rest) = splitAt (length starts) xs
            (xsMiddle, xsEnd) = 
                if length ends == 0 then (rest, [])
                else splitAt (length rest - length ends) rest
    bind (PCompDot ps _) (VDot vs) =
        let 
            (psInit, pLast) = initLast ps
            (vsInit, vsLast) = splitAt (length psInit) vs
            lastValue [x] = x
            lastValue vs = VDot vs
            initLast :: [a] -> ([a], a)
            initLast [a] = ([],a)
            initLast (a:as) = let (bs,b) = initLast as in (a:bs,b)

            (b1, nvs1) = bindAll psInit vsInit
            (b2, nvs2) = bind pLast (lastValue vsLast)
        in (b1 && b2, nvs1++nvs2)
    bind (PDoublePattern p1 p2) v =
        let
            (m1, b1) = bind p1 v
            (m2, b2) = bind p2 v
        in (m1 && m2, b1++b2)
    bind (PLit (Int i1)) (VInt i2) | i1 == i2 = (True, [])
    bind (PLit (Bool b1)) (VBool b2) | b1 == b2 = (True, [])
    bind (PSet [p]) (VSet s) = 
        case singletonValue s of
            Just v  -> bind p v
            Nothing -> (False, [])
    bind (PTuple ps) (VTuple vs) = do
        -- NB: len ps == len vs by typechecker
        bindAll ps vs
-- TODO, the following two cases are wrong, consider calling f(x) = x
-- with f(done). ARGH.
--    bind (PVar n) (VChannel n') = return (n == n', []) 
--    bind (PVar n) (VDataType n') = return (n == n', [])
-- Must just be a normal variable
    bind (PVar n) v | isNameDataConstructor n = 
        case v of
            VChannel n' -> (n == n', [])
            VDataType n' -> (n == n', [])
    bind (PVar n) v = (True, [(n, v)])
    bind PWildCard v = (True, [])
    bind _ _ = (False, [])

bindAll :: Bindable a => [a] -> [Value] -> (Bool, [(Name, Value)])
bindAll ps xs =
    let
        rs = zipWith bind ps xs
    in (and (map fst rs), concat (map snd rs))
