module CSPM.Evaluator.PatBind (
    bind,
    fieldsConsumed,
    bindAll,
) where

import CSPM.Evaluator.AnalyserMonad
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet
import CSPM.Syntax.AST
import CSPM.Syntax.Literals
import CSPM.Syntax.Names
import Util.Annotated
import Util.Exception

thenBind :: Maybe [(Name, Value)] -> Maybe [(Name, Value)] -> Maybe [(Name, Value)]
thenBind Nothing _ = Nothing
thenBind _ Nothing = Nothing
thenBind (Just xs) (Just ys) = Just (xs ++ ys)

-- | Returns the number of complete fields consumed by a pattern.
fieldsConsumed :: TCPat -> Int
fieldsConsumed (An _ _ (PCompDot ps _)) = sum (map fieldsConsumed ps)
fieldsConsumed (An _ _ (PDoublePattern p1 p2)) = fieldsConsumed p1 `max` fieldsConsumed p2
fieldsConsumed _ = 1

bind :: TCPat -> AnalyserMonad (Value -> Maybe [(Name, Value)])
-- We can decompose any PConcat pattern into three patterns representing:
-- Begining (of the form PList), middle (either PWildcard or PVar)
-- and end (of the form PList), With both begining and end possible empty
bind (An _ _ (PCompList ps Nothing _)) = do
    binder <- bindAll ps
    let required = length ps
    return $! \ (VList xs) ->
        if required == length xs then
            binder xs
        else
            Nothing
-- By desugaring the middle is not a PConcat or a PList
bind (An _ _ (PCompList starts (Just (middle, ends)) _)) = do
    startBinder <- bindAll starts
    middleBinder <- bind middle
    endBinder <- bindAll ends
    let reqLength = length starts + length ends
    return $! \ (VList xs) -> 
        let 
            atLeastLength 0 _ = True
            atLeastLength _ [] = False
            atLeastLength n (x:xs) = atLeastLength (n-1) xs
            (xsStart, rest) = splitAt (length starts) xs
            (xsMiddle, xsEnd) = 
                if length ends == 0 then (rest, [])
                else splitAt (length rest - length ends) rest
        in
            -- Only match if the list contains sufficient items
            if not (atLeastLength reqLength xs) then 
                Nothing
            else
                startBinder xsStart
                `thenBind` middleBinder (VList xsMiddle)
                `thenBind` endBinder xsEnd
bind (An _ _ (PCompDot ps _)) = do
    let
        -- Matches a compiled dot pattern, given a list of patterns for
        -- the fields and the values that each field takes.
        matchCompDot :: [TCPat] -> AnalyserMonad ([Value] -> Maybe [(Name, Value)])
        matchCompDot [] = return $! \ xs ->
            case xs of 
                [] -> Just []
                _ -> Nothing
        matchCompDot [p] = do
            binder <- bind p 
            return $! \ vs ->
                case vs of
                    [v] -> binder v
                    _ -> binder (VDot vs)
        matchCompDot (An _ _ (PVar n):ps) | isNameDataConstructor n = do
            bindRest <- matchCompDot ps
            return $! \ v ->
                case v of
                    VChannel n' : vs2 | n == n' -> bindRest vs2
                    VDataType n' : vs2 | n == n' -> bindRest vs2
                    (VDot (VDataType n':vfs):vs2) | n == n' ->
                        -- In this case, we are matching within a subfield of the
                        -- current field. Therefore, add all the values that this
                        -- subfield has.
                        bindRest (vfs++vs2)
                    (VDot (VChannel n':vfs):vs2) | n == n' ->
                        bindRest (vfs++vs2)
                    _ -> Nothing
        matchCompDot (p:ps) = do
            binder <- bind p
            let fs = fieldsConsumed p
                split n left right | n == fs = Just (reverse left, right)
                split n left (v:vs) = split (n+1) (v:left) vs
                split _ _ _ = Nothing
            bindRest <- matchCompDot ps
            if fs == 1 then 
                return $! \ vs ->
                    case vs of
                        (v:vs) -> binder v `thenBind` bindRest vs
                        _ -> Nothing
            else
                return $! \ vs ->
                    case split 0 [] vs of
                        Just (left, right) -> binder (VDot left) `thenBind` bindRest right
                        _ -> Nothing
    binder <- matchCompDot ps
    return $! \ (VDot xs) -> binder xs
bind (An _ _ (PDoublePattern p1 p2)) = do
    bind1 <- bind p1
    bind2 <- bind p2
    return $! \ v -> bind1 v `thenBind` bind2 v
bind (An _ _ (PLit (Int i1))) = return $! \ (VInt i2) -> emptyJust (i1 == i2)
bind (An _ _ (PLit (Bool b1))) = return $! \ (VBool b2) -> emptyJust (b1 == b2)
bind (An _ _ (PLit (Char c1))) = return $! \ (VChar c2) -> emptyJust (c1 == c2)
bind (An _ _ (PSet [])) = return $! \ (VSet s) -> emptyJust (empty s)
bind (An _ _ (PSet [p])) = do
    binder <- bind p
    return $! \ (VSet s) ->
        case singletonValue s of
            Just v  -> binder v
            Nothing -> Nothing
bind (An _ _ (PTuple ps)) = do
    binder <- bindAll ps
    return $! \ (VTuple vs) -> binder (elems vs)
bind (An _ _ (PVar n)) | isNameDataConstructor n = return $! \ v ->
    case v of
        VChannel n' -> emptyJust (n == n')
        VDataType n' -> emptyJust (n == n')
        -- We have to allow these to enable patterns such as f(J) where
        -- J has arity 0.
        VDot [VChannel n'] -> emptyJust (n == n')
        VDot [VDataType n'] -> emptyJust (n == n')
        _ -> Nothing
bind (An _ _ (PVar n)) = return $! \ v -> Just [(n, v)]
bind (An _ _ (PWildCard)) = return $! \ _ -> Just []
bind _ = panic "Unknown pattern"

emptyJust True = Just []
emptyJust _ = Nothing

bindAll :: [TCPat] -> AnalyserMonad ([Value] -> Maybe [(Name, Value)])
bindAll ps =
    let
        accumulate [] [] bs = Just bs
        accumulate (p:ps) (x:xs) bs =
            case p x of
                Just bs' -> accumulate ps xs (bs' ++ bs)
                Nothing -> Nothing
    in do
        binders <- mapM bind ps
        return $! \vs -> accumulate binders vs []
