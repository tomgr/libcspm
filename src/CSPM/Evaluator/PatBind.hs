{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CSPM.Evaluator.PatBind where

import CSPM.DataStructures.Literals
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import Util.Annotated
import Util.Exception
import Util.PrettyPrint

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
            -- Matches a compiled dot pattern, given a list of patterns for
            -- the fields and the values that each field takes.
            matchCompDot :: [Pat Name] -> [Value] -> (Bool, [(Name, Value)])
            matchCompDot [] [] = (True, [])
            matchCompDot (PVar n:ps) (VDot (VDataType n':vfs):vs2) | isNameDataConstructor n = 
                -- In this case, we are matching within a subfield of the
                -- current field. Therefore, add all the values that this
                -- subfield has.
                if n /= n' then (False, []) 
                else matchCompDot ps (vfs++vs2)
            matchCompDot (PVar n:ps) (VDot (VChannel n':vfs):vs2) | isNameDataConstructor n = 
                if n /= n' then (False, []) 
                else matchCompDot ps (vfs++vs2)
            matchCompDot [p] [v] = bind p v
            matchCompDot [p] vs = bind p (VDot vs)
            matchCompDot (p:ps) (v:vs) = 
                let
                    (b1, nvs1) = bind p v
                    (b2, nvs2) = matchCompDot ps vs
                in (b1 && b2, nvs1++nvs2)
            matchCompDot _ _ = (False, [])
        in matchCompDot (map unAnnotate ps) vs
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
    bind (PVar n) v | isNameDataConstructor n = 
        case v of
            VChannel n' -> (n == n', [])
            VDataType n' -> (n == n', [])
            _ -> panic $ show $ prettyPrint v <+> text "is not a data constructor."
    bind (PVar n) v = (True, [(n, v)])
    bind PWildCard v = (True, [])
    bind _ _ = (False, [])

bindAll :: Bindable a => [a] -> [Value] -> (Bool, [(Name, Value)])
bindAll ps xs =
    let
        rs = zipWith bind ps xs
    in (and (map fst rs), concat (map snd rs))
