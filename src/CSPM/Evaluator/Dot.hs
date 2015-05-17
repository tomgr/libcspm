module CSPM.Evaluator.Dot (
    combineDots, dataTypeInfo,
    extensions, extensionsSet, oneFieldExtensions,
    productions, productionsSet, splitIntoFields,
    compressIntoEnumeratedSet,
) where

import CSPM.Syntax.Names
import {-# SOURCE #-} CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet hiding (cartesianProduct)
import qualified CSPM.Evaluator.ValueSet as S
import Data.List (groupBy, sortBy)
import Data.Maybe (catMaybes, isJust)
import Util.Annotated
import Util.List
import Util.Prelude

dataTypeInfo :: Name -> EvaluationMonad (Value, Int, Array Int ValueSet)
dataTypeInfo n = do
    VTuple dta <- lookupVar n
    let VInt a = dta!1
        VTuple fs = dta!2
    return $ (dta!0, a, fmap (\(VSet s) -> s) fs)
{-# INLINE dataTypeInfo #-}

-- | The number of fields this datatype or channel has.
arityOfDataTypeClause :: Name -> EvaluationMonad Int
arityOfDataTypeClause n = do
    (_, a, _) <- dataTypeInfo n
    return a

-- | Returns true if the value is a complete field.
isCompleteField :: Value -> EvaluationMonad Bool
isCompleteField (v@(VDot vs)) =
    case maybeNamedDot v of
        Nothing -> return True
        Just n -> do
            arity <- arityOfDataTypeClause n
            if arity == length vs -1 then
                isCompleteField (last vs)
            else return False
isCompleteField _ = return True

-- | Takes two values and dots then together appropriately.
combineDots :: SrcSpan -> Value -> Value -> EvaluationMonad Value
combineDots loc v1 v2 =
    let
        -- | Dots the given value onto the right of the given base, providing
        -- the left hand value is a field.
        maybeDotFieldOn :: Value -> Value -> EvaluationMonad (Maybe Value)
        maybeDotFieldOn vbase v = do
            fields <-
                case maybeNamedDot vbase of
                    Just n -> do
                        (_, _, fs) <- dataTypeInfo n
                        let VDot (nd:_) = vbase
                        return $! S.cartesianProduct CartDot $
                            fromList [nd] : elems fs
                    Nothing -> return S.emptySet
            dotNamedFieldOn (maybeNamedDot vbase) fields vbase v

        dotNamedFieldOn :: Maybe Name -> ValueSet -> Value -> Value ->
            EvaluationMonad (Maybe Value)
        dotNamedFieldOn (Just n) allowedValues (VDot vs) v = do
            let fieldCount = length vs -1
                lastField = last vs
                getField ix = splitFieldSet ix (VDot vs) allowedValues
            b <- isCompleteField lastField
            arity <- arityOfDataTypeClause n
            if b then 
                if arity == fieldCount then return Nothing
                else do
                    let newValue = VDot (vs++[v])
                        fieldSet = getField fieldCount
                    checkIsValidForField fieldSet newValue fieldCount v $
                        return $ Just newValue
            else do
                let fieldSet = getField (fieldCount-1)
                vLast <- dotNamedFieldOn (maybeNamedDot lastField) fieldSet
                            lastField v
                case vLast of
                    Nothing -> return Nothing
                    Just vLast -> do
                        let newValue = VDot (replaceLast vs vLast)
                        checkIsValidForField fieldSet newValue fieldCount vLast $
                            return $ Just newValue
        dotNamedFieldOn Nothing _ _ _ = return Nothing

        checkIsValidForField :: ValueSet -> Value -> Int ->
            Value -> EvaluationMonad a -> EvaluationMonad a
        checkIsValidForField allowedSet overallValue field v result = do
            b <- isCompleteField v
            if not b then result else do
            if member v allowedSet then result
            else throwError' $
                dotIsNotValidMessage overallValue field v allowedSet loc

        splitFieldSet :: Int -> Value -> ValueSet -> ValueSet
        splitFieldSet ix v fieldSet =
            case fastUnDotCartProduct fieldSet v of
                Just restrictByField ->restrictByField!!(ix+1)
                Nothing -> slowMatchDotPrefix (\ _ vs -> vs!!(ix+1)) fieldSet v

        -- | Dots the two values together, ensuring that if either the left or
        -- the right value is a dot list combines them into one dot list.
        -- This function assumes that any data values are not meant to be split
        -- apart.
        dotAndReduce :: Value -> Value -> Value
        -- We don't need to split v2 into fields because whenever we call
        -- this function the second value is simply being dotted onto the right
        -- and not put into a field of any sort
        dotAndReduce v1 v2 = VDot (splitIntoFields v1 ++ [v2])

        -- | Given a base value and the value of a field dots the field onto
        -- the right of the base. Assumes that the value provided is a field.
        dotFieldOn :: Value -> Value -> EvaluationMonad Value
        dotFieldOn vBase vField = do
            mv <- maybeDotFieldOn vBase vField
            case mv of
                Just v -> return v
                Nothing -> return $ dotAndReduce vBase vField
        
        -- | Split a value up into the values that could be used as fields.
        splitIntoFields :: Value -> [Value]
        splitIntoFields (v@(VDot (VDataType n:_))) = [v]
        splitIntoFields (v@(VDot (VChannel n:_))) = [v]
        splitIntoFields (VDot vs) = vs
        splitIntoFields v = [v]

        -- | Given a base value and a list of many fields dots the fields onto
        -- the base. Assumes that the values provided are fields.
        dotManyFieldsOn :: Value -> [Value] -> EvaluationMonad Value
        dotManyFieldsOn v [] = return v
        dotManyFieldsOn vBase (v:vs) = do
            vBase' <- dotFieldOn vBase v
            dotManyFieldsOn vBase' vs
    in
        -- Split v2 up into its composite fields and then dot them onto v1.
        dotManyFieldsOn v1 (splitIntoFields v2)

-- | Returns an x such that ev.x has been extended by exactly one atomic field.
-- This could be inside a subfield or elsewhere.
oneFieldExtensions :: Value -> EvaluationMonad [Value]
oneFieldExtensions v =
    let
        exts :: [ValueSet] -> Value -> EvaluationMonad [Value]
        exts fieldSets (VDot vs) = do
            case maybeNamedDot (VDot vs) of
                Nothing -> return [VDot []]
                Just n -> do
                    let fieldCount = length vs -1
                    b <- isCompleteField (last vs)
                    if b then return $!
                        if length fieldSets == fieldCount then [VDot []]
                        else toList (fieldSets!!fieldCount)
                    else do
                        let field = fieldSets!!(fieldCount-1)
                        case fastUnDotCartProduct field (last vs) of
                            Just restrictByField ->
                                exts (tail restrictByField) (last vs)
                            Nothing -> return $! toList $ slowMatchDotPrefix
                                (\ i v -> v!!i) field (last vs)
        exts _ _ = return [VDot []]
    in do
        case maybeNamedDot v of
            Just n -> do
                (_, _, fieldSets) <- dataTypeInfo n
                exts (elems fieldSets) v
            Nothing -> return [VDot []]

maybeNamedDot :: Value -> Maybe Name
maybeNamedDot (VDot (VChannel n : _)) = Just n
maybeNamedDot (VDot (VDataType n : _)) = Just n
maybeNamedDot _ = Nothing

-- | Takes a datatype or a channel value and then computes all x such that 
-- ev.x is a full datatype/event. Each of the returned values is guaranteed
-- to be a VDot.
extensions :: Value -> EvaluationMonad [Value]
extensions v = extensionsSet v >>= return . toList

extensionsSet :: Value -> EvaluationMonad ValueSet
extensionsSet v = do
    case maybeNamedDot v of
        Nothing -> return S.emptySet
        Just n -> do
            b <- isCompleteField v
            if b then return $! S.fromList [VDot []] else do
            (_, _, fieldSets) <- dataTypeInfo n
            sets <- extensionsSets (elems fieldSets) v
            return $
                case sets of
                    [s] -> s
                    sets -> S.cartesianProduct CartDot sets

-- | Takes a value and returns a set of fields such that ev.x is a full thing.
-- Further, the field sets are guaranteed to be representable as a full
-- carteisan product.
extensionsSets :: [ValueSet] -> Value -> EvaluationMonad [ValueSet]
extensionsSets fieldSets (VDot vs) = do
    let fieldCount = length vs - 1
        maybeWrap [v] = v
        maybeWrap vs = VDot vs
        -- Firstly, complete the last field in the current value (in case
        -- it is only half formed).
    exsLast <-
        if fieldCount == 0 || not (isJust (maybeNamedDot (last vs))) then
            return []
        else do
            b <- isCompleteField (last vs) 
            if b then return []
            else do
                let field = fieldSets!!(fieldCount-1)
                case fastUnDotCartProduct field (last vs) of
                    Just restrictByField ->
                        extensionsSets (tail restrictByField) (last vs)
                    Nothing -> -- Need to do a slow scan
                        return $! 
                            [slowMatchDotPrefix (\ i v -> maybeWrap (drop i v))
                            field (last vs)]
    return $! exsLast ++ drop fieldCount fieldSets
extensionsSets _ _ = return []

-- | Given a set of dotted values, and a dotted value, scans the set of dotted
-- values and calls the specified function for each value that matches.
slowMatchDotPrefix :: (Int -> [Value] -> Value) -> ValueSet -> Value -> ValueSet
slowMatchDotPrefix f set v1 =
    let
        matches v2 | v2 `isProductionOf` v1 = 
            let VDot vs' = v2
                VDot vs = v1
            in [f (length vs) vs']
        matches _ = []
    in
        fromList (concatMap matches (toList set))

-- | Given two dot lists, the second of which may be an incomplete dot-list,
-- returns True if the first is a production of the second.
isProductionOf :: Value -> Value -> Bool
isProductionOf (VDot (n1:fs1)) (VDot (n2:fs2)) =
        n1 == n2 && length fs1 >= length fs2 && listIsProductionOf fs1 fs2
    where
        listIsProductionOf _ [] = True
        listIsProductionOf [] _ = False
        listIsProductionOf (f1:fs1) [f2] = f1 `isProductionOf` f2
        listIsProductionOf (f1:fs1) (f2:fs2) =
            f1 == f2 && listIsProductionOf fs1 fs2
isProductionOf v1 v2 = v1 == v2

-- | Takes a datatype or a channel value and computes v.x for all x that
-- complete the value.
productions :: Value -> EvaluationMonad [Value]
productions v = productionsSet v >>= return . toList

productionsSet :: Value -> EvaluationMonad ValueSet
productionsSet v = do
    case maybeNamedDot v of
        Nothing -> return S.emptySet
        Just n -> do
            b <- isCompleteField v
            if b then return $! S.fromList [v] else do
            (_, _, fieldSets) <- dataTypeInfo n
            sets <- productionsSets (elems fieldSets) v
            return $! S.cartesianProduct CartDot sets
    
productionsSets :: [ValueSet] -> Value -> EvaluationMonad [ValueSet]
productionsSets fieldSets (VDot vs) = do
    let fieldCount = length vs - 1
    psLast <-
        if fieldCount == 0 then return []
        else if not (isJust (maybeNamedDot (last vs))) then return []
        else do
            b <- isCompleteField (last vs)
            if b then return []
            else do
                let field = fieldSets!!(fieldCount-1)
                case fastUnDotCartProduct field (last vs) of
                    Just restrictByField -> do
                        sets <- productionsSets (tail restrictByField) (last vs)
                        return [S.cartesianProduct CartDot sets]
                    Nothing -> return
                        [slowMatchDotPrefix (\ _ -> VDot) field (last vs)]

    let psSets = case psLast of
                    [] -> map (\v -> fromList [v]) vs
                    _ ->
                        -- We cannot express this as a simple cart product, as
                        -- the resulting item has dots at two levels. Thus,
                        -- dot together this lot and form an explicit set,
                        -- then we proceed as before
                        map (\v -> fromList [v]) (init vs) ++ psLast
    return $! psSets ++ drop fieldCount fieldSets
productionsSets _ v = return []

takeFields :: Int -> [Value] -> EvaluationMonad ([Value], [Value])
takeFields 0 vs = return ([], vs)
takeFields 1 vs = do
    (f, vs) <- takeFirstField False vs
    return ([f], vs)
takeFields n vs = do
    (f, vs') <- takeFirstField False vs
    (fs, vs'') <- takeFields (n-1) vs'
    return (f:fs, vs'')

takeFirstField :: Bool -> [Value] -> EvaluationMonad (Value, [Value])
takeFirstField True (VDataType n : vs) = return (VDataType n, vs)
takeFirstField True (VChannel n : vs) = return (VChannel n, vs)
takeFirstField False (VDataType n : vs) = do
    (_, arity, fieldSets) <- dataTypeInfo n
    (fs, vs) <- takeFields arity vs
    return $ (VDot (VDataType n : fs), vs)
takeFirstField False (VChannel n : vs) = do
    (_, arity, fieldSets) <- dataTypeInfo n
    (fs, vs) <- takeFields arity vs
    return $ (VDot (VChannel n : fs), vs)
takeFirstField forceSplit (v:vs) = return (v, vs)

-- | Takes a set of dotted values (i.e. a set of VDot _) and returns a list of
-- sets such that the cartesian product is equal to the original set.
--
-- This throws an error if the set cannot be decomposed.
splitIntoFields :: Bool -> Name -> ValueSet -> EvaluationMonad [ValueSet]
splitIntoFields forceSplit n vs = do
    case unDotProduct vs of
        Just ss -> return ss
        Nothing -> manuallySplitValues forceSplit n vs (toList vs)

isDot :: Value -> Bool
isDot (VDot _ ) = True
isDot _ = False

manuallySplitValues :: Bool -> Name -> ValueSet -> [Value] ->
    EvaluationMonad [ValueSet]
manuallySplitValues forceSplit n vs (values@(VDot fs : _)) = do
    let extract (VDot vs) = vs
        -- | Splits a dot list into the separate fields.
        split :: [Value] -> EvaluationMonad [Value]
        split [] = return []
        split vs = do
            (v, vs') <- takeFirstField forceSplit vs
            ss <- split vs'
            return $ v:ss
    splitValues <- mapM (split . extract) (toList vs)
    if splitValues == [] then return [] else do
    let fieldCount = length (head splitValues)
        combine [] = replicate fieldCount []
        combine (vs:vss) = zipWith (:) vs (combine vss)
        -- | The list of values such that cart producting them together should
        -- yield the overall datatype.
        cartProductFields :: [[Value]]
        cartProductFields = combine splitValues
        -- | Given a set, recursively checks that it is ok, and reconstruct the
        -- set as a cart product.
        recursivelySplit vs = do
            if length vs > 0 && isDot (head vs)
                    && length (extract (head vs)) > 1 then do
                -- We've got a dotted field - check to see if this field is
                -- recursively decomposable
                sets <- splitIntoFields True n (fromList vs)
                if length sets == 1 then return $ head sets
                else return $ S.cartesianProduct S.CartDot sets
            else return $! fromList vs

    if or (map isMixedList cartProductFields) then
        if forceSplit || length cartProductFields == 1 then return [vs]
        else throwError $ setNotRectangularErrorMessage (nameDefinition n) vs
            Nothing
    else do
    sets <- mapM recursivelySplit cartProductFields

    let cartProduct =
            if length sets == 1 && isDot (head (toList (head sets))) then do
                -- Don't wrap with extra dots if we already have some
                head sets
            else S.cartesianProduct S.CartDot sets
    if cartProduct /= vs then
        if forceSplit then 
            return [vs]
        else throwError $
            setNotRectangularErrorMessage (nameDefinition n) vs
                (Just cartProduct)
    else return $ sets
manuallySplitValues _ _ vs _ = return [vs]

isMixedList :: [Value] -> Bool
isMixedList [] = False
isMixedList [x] = False
isMixedList (VInt _ : (xs@(VInt _ : _))) = isMixedList xs
isMixedList (VBool _ : (xs@(VBool _ : _))) = isMixedList xs
isMixedList (VChar _ : (xs@(VChar _ : _))) = isMixedList xs
isMixedList (VTuple t1 : (xs@(VTuple t2 : _))) =
    let vs1 = elems t1
        vs2 = elems t2
    in length vs1 /= length vs2
        || or (zipWith (\ x y -> isMixedList [x,y]) vs1 vs2)
        || isMixedList xs
isMixedList (VDot vs1 : (xs@(VDot vs2 : _))) = 
    length vs1 /= length vs2
    || or (zipWith (\ x y -> isMixedList [x,y]) vs1 vs2)
    || isMixedList xs
isMixedList (VChannel _ : (xs@(VChannel _ : _))) = isMixedList xs
isMixedList (VDataType _ : (xs@(VDataType _ : _))) = isMixedList xs
isMixedList (VProc _ : (xs@(VProc _ : _))) = isMixedList xs
isMixedList (VList vs1 : (xs@(VList vs2 : _))) =
    (length vs1 > 0 && length vs2 > 0 && isMixedList [head vs1, head vs2])
    || isMixedList xs
isMixedList (VSet s1 : (xs@(VSet s2 : _))) =
    let vs1 = toList s1
        vs2 = toList s2
    in (length vs1 > 0 && length vs2 > 0 && isMixedList [head vs1, head vs2])
    || isMixedList xs
isMixedList _ = True

-- | Takes a set and returns a list of values xs such that 
-- Union({productions(x) | x <- xs}) == xs. For example, if c is a channel of
-- type {0,1} then {c.0, c.1} would return [c].
--
-- This is primarily used for display purposes.
compressIntoEnumeratedSet :: ValueSet -> EvaluationMonad (Maybe [Value])
compressIntoEnumeratedSet vs =
    let 
        haveAllOfLastField :: [[Value]] -> EvaluationMonad Bool
        haveAllOfLastField ys = do
            let n = case head (head ys) of
                        VDataType n -> n
                        VChannel n -> n
                fieldIx = length (head ys) - 2
            (_, _, fieldSets) <- dataTypeInfo n
            if fromList (map last ys) == fieldSets!fieldIx then
                -- All values are used
                return True
            else return False

        splitGroup :: [[Value]] -> EvaluationMonad (Maybe [Value])
        splitGroup ([_]:_) = return Nothing
        splitGroup vs = do
            b <- haveAllOfLastField vs
            if b then
                -- have everything, and inits are equal, so can compress.
                -- Since the inits are equal just take the init of the first
                -- item.
                return $ Just $ init (head vs)
            else return $ Nothing

        forceRepeatablyCompress :: [[Value]] -> EvaluationMonad [Value]
        forceRepeatablyCompress vs = do
            mt <- repeatablyCompress vs
            return $! case mt of
                        Just vs -> vs
                        Nothing -> map VDot vs

        -- | Repeatably compresses the supplied values from the back, returning
        -- the compressed set.
        repeatablyCompress :: [[Value]] -> EvaluationMonad (Maybe [Value])
        repeatablyCompress [] = return Nothing
        repeatablyCompress vs = do
            let initiallyEqual :: [[[Value]]]
                initiallyEqual = groupBy (\ xs ys ->
                    head xs == head ys && init xs == init ys) $
                    sortBy (\ xs ys -> compare (head xs) (head ys)
                            `thenCmp` compare (init xs) (init ys)) vs
                -- head is required above (consider [x]).
                processNothing Nothing vss = map VDot vss
                processNothing (Just _) vss = []
            gs <- mapM splitGroup initiallyEqual
            let vsDone = zipWith processNothing gs initiallyEqual
            -- Now, repeatably compress the prefixes that were equal.
            case catMaybes gs of
                [] -> return Nothing
                xs -> do
                    vsRecursive <- forceRepeatablyCompress xs
                    return $! Just (vsRecursive ++ concat vsDone)
    in case toList vs of
            [] -> return Nothing
            (vs @ (VDot ((VChannel _) :_) : _)) ->
                repeatablyCompress (map (\ (VDot xs) -> xs) vs)
            _ -> return Nothing -- must be a set that we cannot handle
