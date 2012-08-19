module CSPM.Evaluator.Dot (
    combineDots, dataTypeInfo,
    extensions, extensionsSet, oneFieldExtensions,
    productions, productionsSet, splitIntoFields,
    compressIntoEnumeratedSet,
) where

import CSPM.DataStructures.Names
import {-# SOURCE #-} CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet hiding (cartesianProduct)
import qualified CSPM.Evaluator.ValueSet as S
import Data.List (groupBy)
import Data.Maybe (catMaybes)
import Util.Exception
import Util.List

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

-- | Takes two values and dots then together appropriately.
combineDots :: Value -> Value -> EvaluationMonad Value
combineDots v1 v2 =
    let
        -- | Dots the given value onto the right of the given base, providing
        -- the left hand value is a field.
        maybeDotFieldOn :: Value -> Value -> EvaluationMonad (Maybe Value)
        maybeDotFieldOn (VDot (nd:vs)) v = do
            let 
                mn = case nd of
                        VDataType n -> Just n
                        VChannel n -> Just n
                        _ -> Nothing
            case mn of
                Nothing -> return Nothing
                Just n -> do
                    a <- arityOfDataTypeClause n
                    let fieldCount = length vs
                    if a == 0 then return Nothing
                    else if fieldCount == 0 then do
                        b <- checkIsValidForField mn (VDot [nd, v]) 0 v
                        if b then return $ Just (VDot [nd, v])
                        else return $ Nothing
                    else do
                        -- Try and dot it onto our last field
                        mv <- maybeDotFieldOn (last vs) v
                        case mv of
                            Just vLast -> do
                                let newValue = VDot (nd:replaceLast vs vLast)
                                b <- checkIsValidForField mn newValue (fieldCount-1) vLast
                                return $ if b then Just newValue else Nothing
                            -- Start a new field, or return nothing if we
                            -- are full
                            Nothing | fieldCount < a -> do
                                checkIsValidForField mn (VDot (nd:vs++[v])) fieldCount v
                                return $ Just (VDot (nd:vs++[v]))
                            Nothing | fieldCount == a -> return Nothing
                            Nothing | fieldCount > a -> panic "Malformed dot encountered."
        maybeDotFieldOn vbase v = return Nothing

        isComplete :: Value -> EvaluationMonad Bool
        isComplete (VDot (VChannel n : vs)) = do
            (_, arity, _) <- dataTypeInfo n
            return $ length vs == arity
        isComplete (VDot (VDataType n : vs)) = do
            (_, arity, _) <- dataTypeInfo n
            return $ length vs == arity
        isComplete _ = return True

        checkIsValidForField :: Maybe Name -> Value -> Int -> Value -> EvaluationMonad Bool
        checkIsValidForField Nothing overallValue field v = return True
        checkIsValidForField (Just n) overallValue field v = do
            b <- isComplete v
            if not b then return True else do
            (_, _, fieldSets) <- dataTypeInfo n
            if member v (fieldSets!field) then return True
            else throwError' $ dotIsNotValidMessage overallValue field v (fieldSets!field)

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
oneFieldExtensions (VDot (dn:vs)) = do
    let 
       mn = case dn of
                VChannel n -> Just n
                VDataType n -> Just n
                _ -> Nothing
    case mn of
        Nothing -> return [VDot []]
        Just n -> do
            let fieldCount = length vs
            -- Get the information about the channel
            (_, arity, fieldSets) <- dataTypeInfo n

            -- Firstly, try completing the last field in the current value 
            -- (in case it is only half formed).
            mexs <- 
                if fieldCount > 0 then do
                    exs <- oneFieldExtensions (last vs)
                    if exs /= [VDot []] then return $ Just exs
                    else return Nothing
                else return Nothing
            
            return $ case mexs of
                Just exs -> exs
                Nothing -> 
                    if arity == fieldCount then [VDot []]
                    else -- We still have fields to complete
                        map (\ v -> VDot [v]) (toList (fieldSets!(length vs)))
oneFieldExtensions _ = return [VDot []]

-- | Takes a datatype or a channel value and then computes all x such that 
-- ev.x is a full datatype/event. Each of the returned values is guaranteed
-- to be a VDot.
extensions :: Value -> EvaluationMonad [Value]
extensions v = extensionsSet v >>= return . toList

extensionsSet :: Value -> EvaluationMonad ValueSet
extensionsSet v = extensionsSets v >>= return . S.cartesianProduct CartDot

-- | Takes a value and returns a set of fields such that ev.x is a full thing.
-- Further, the field sets are guaranteed to be representable as a full
-- carteisan product.
extensionsSets :: Value -> EvaluationMonad [ValueSet]
extensionsSets (VDot (dn:vs)) = do
    let 
       mn = case dn of
                VChannel n -> Just n
                VDataType n -> Just n
                _ -> Nothing
    case mn of
        Nothing -> return []
        Just n -> do
            let fieldCount = length vs
            -- Get the information about the datatype/channel
            (_, arity, fieldSets) <- dataTypeInfo n

            -- Firstly, complete the last field in the current value (in case it is only
            -- half formed).
            exsLast <- if fieldCount == 0 then return [] else extensionsSets (last vs)

            return $ exsLast ++ drop fieldCount (elems fieldSets)

extensionsSets v = return  []

-- | Takes a datatype or a channel value and computes v.x for all x that
-- complete the value.
productions :: Value -> EvaluationMonad [Value]
productions v = productionsSet v >>= return . toList

productionsSet :: Value -> EvaluationMonad ValueSet
productionsSet v = productionsSets v >>= return . S.cartesianProduct CartDot

productionsSets :: Value -> EvaluationMonad [ValueSet]
productionsSets (VDot (dn:vs)) = do
    let 
       mn = case dn of
                VChannel n -> Just n
                VDataType n -> Just n
                _ -> Nothing
    case mn of
        Nothing -> return []
        Just n -> do
            let fieldCount = length vs
            -- Get the information about the datatype/channel
            (_, arity, fieldSets) <- dataTypeInfo n
            -- Firstly, complete the last field in the current value (in case it is only
            -- half formed).
            psLast <- if fieldCount == 0 then return [] else productionsSets (last vs)
            let psSets = case psLast of
                            [] -> map (\v -> fromList [v]) (dn:vs)
                            _ ->
                                -- We cannot express this as a simple cart product, as
                                -- the resulting item has dots at two levels. Thus,
                                -- dot together this lot and form an explicit set,
                                -- then we proceed as before
                                map (\v -> fromList [v]) (dn:init vs)
                                ++ [S.cartesianProduct CartDot psLast]
            return $ psSets ++ drop fieldCount (elems fieldSets)
productionsSets v = return []

takeFields :: Int -> [Value] -> EvaluationMonad ([Value], [Value])
takeFields 0 vs = return ([], vs)
takeFields 1 vs = do
    (f, vs) <- takeFirstField vs
    return ([f], vs)
takeFields n vs = do
    (f, vs') <- takeFirstField vs
    (fs, vs'') <- takeFields (n-1) vs'
    return (f:fs, vs'')

takeFirstField :: [Value] -> EvaluationMonad (Value, [Value])
takeFirstField (VDataType n : vs) = do
    (_, arity, fieldSets) <- dataTypeInfo n
    (fs, vs) <- takeFields arity vs
    return $ (VDot (VDataType n : fs), vs)
takeFirstField (VChannel n : vs) = do
    (_, arity, fieldSets) <- dataTypeInfo n
    (fs, vs) <- takeFields arity vs
    return $ (VDot (VChannel n : fs), vs)
takeFirstField (v:vs) = return (v, vs)

-- | Takes a set of dotted values (i.e. a set of VDot _) and returns a list of
-- sets such that the cartesian product is equal to the original set.
--
-- This throws an error if the set cannot be decomposed.
splitIntoFields :: ValueSet -> EvaluationMonad [ValueSet]
splitIntoFields vs = do
    let values = toList vs
        extract (VDot vs) = vs
        -- | Splits a dot list into the separate fields.
        split :: [Value] -> EvaluationMonad [Value]
        split [] = return []
        split vs = do
            (v, vs') <- takeFirstField vs
            ss <- split vs'
            return $ v:ss
    case unDotProduct vs of
        Just ss -> return ss
        Nothing -> case values of
                        (VDot fs : _) -> do
                            splitValues <- mapM (split . extract) values
                            if splitValues == [] then return [] else do
                            let fieldCount = length (head splitValues)
                                combine [] = replicate fieldCount []
                                combine (vs:vss) = zipWith (:) vs (combine vss)
                                sets = map fromList $ combine splitValues
                                cartProduct =
                                    case sets of
                                        -- If we have a single field don't wrap it in a VDot.
                                        [x] -> x
                                        _ -> S.cartesianProduct S.CartDot sets
                            if cartProduct /= vs then
                                throwError $ setNotRectangularErrorMessage vs cartProduct
                            else return $ sets
                        _ -> return $ [vs]

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
                    head xs == head ys && init xs == init ys) vs
                -- head is required above (consider [x]).
                processNothing Nothing vs = [VDot vs]
                processNothing (Just _) vs = []
            gs <- mapM splitGroup initiallyEqual
            let vsDone = zipWith processNothing gs vs
            -- Now, repeatably compress the prefixes that were equal.
            case catMaybes gs of
                [] -> return Nothing
                xs -> do
                    vsRecursive <- forceRepeatablyCompress xs
                    return $! Just (vsRecursive ++ concat vsDone)

        setValues = toList vs
    in case toList vs of
            [] -> return Nothing
            (vs @ (VDot ((VChannel _) :_) : _)) ->
                repeatablyCompress (map (\ (VDot xs) -> xs) vs)
            _ -> return Nothing -- must be a set that we cannot handle
