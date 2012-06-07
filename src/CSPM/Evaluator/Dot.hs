module CSPM.Evaluator.Dot (
    combineDots,
    extensions, oneFieldExtensions,
    productions, splitIntoFields,
) where

import CSPM.DataStructures.Names
import CSPM.Evaluator.Exceptions
import CSPM.Evaluator.Monad
import CSPM.Evaluator.Values
import CSPM.Evaluator.ValueSet hiding (cartesianProduct)
import qualified CSPM.Evaluator.ValueSet as S
import Util.Exception
import Util.List

-- | The number of fields this datatype or channel has.
arityOfDataTypeClause :: Name -> EvaluationMonad Int
arityOfDataTypeClause n = do
    VTuple [_, VInt a,_] <- lookupVar n
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
            VTuple [_, VInt arity, _] <- lookupVar n
            return $ length vs == arity
        isComplete (VDot (VDataType n : vs)) = do
            VTuple [_, VInt arity, _] <- lookupVar n
            return $ length vs == arity
        isComplete _ = return True

        checkIsValidForField :: Maybe Name -> Value -> Int -> Value -> EvaluationMonad Bool
        checkIsValidForField Nothing overallValue field v = return True
        checkIsValidForField (Just n) overallValue field v = do
            b <- isComplete v
            if not b then return True else do
            VTuple [_, VInt arity, VList fieldSets] <- lookupVar n
            let VSet vs = fieldSets!!field
            if member v vs then return True
            else throwError $ dotIsNotValidMessage overallValue field v vs

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
            VTuple [_, VInt arity, VList fieldSets] <- lookupVar n

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
                        map (\ v -> VDot [v]) 
                            (head [toList s | VSet s <- drop (length vs) fieldSets])
oneFieldExtensions _ = return [VDot []]

-- | Takes a datatype or a channel value and then computes all x such that 
-- ev.x is a full datatype/event. Each of the returned values is guaranteed
-- to be a VDot.
extensions :: Value -> EvaluationMonad [Value]
extensions (VDot (dn:vs)) = do
    let 
       mn = case dn of
                VChannel n -> Just n
                VDataType n -> Just n
                _ -> Nothing
    case mn of
        Nothing -> return [VDot []]
        Just n -> do
            let fieldCount = length vs
            -- Get the information about the datatype/channel
            VTuple [_, VInt arity, VList fieldSets] <- lookupVar n

            -- Firstly, complete the last field in the current value (in case it is only
            -- half formed).
            exsLast <- 
                if fieldCount == 0 then return [VDot []]
                else extensions (last vs)
            
            if arity == fieldCount then return exsLast
            else 
                -- We still have fields to complete
                let 
                    remainingFields = [toList s | VSet s <- drop (length vs) fieldSets]
                    combineDots ((VDot vs1):vs2) = VDot (vs1++vs2)
                    fields = exsLast:remainingFields
                in return $ map combineDots (cartesianProduct fields)
extensions v = return [VDot []]

-- | Takes a datatype or a channel value and computes v.x for all x that
-- complete the value.
productions :: Value -> EvaluationMonad [Value]
productions v = do
    pss <- extensions v
    mapM (combineDots v) pss

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
    VTuple [_, VInt arity, VList fieldSets] <- lookupVar n
    (fs, vs) <- takeFields arity vs
    return $ (VDot (VDataType n : fs), vs)
takeFirstField (VChannel n : vs) = do
    VTuple [_, VInt arity, VList fieldSets] <- lookupVar n
    (fs, vs) <- takeFields arity vs
    return $ (VDot (VChannel n : fs), vs)
takeFirstField (v:vs) = return (v, vs)

splitIntoFields :: ValueSet -> EvaluationMonad [ValueSet]
splitIntoFields vs = do
    let values = toList vs
        extract (VDot vs) = vs
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
                                cartProduct = S.cartesianProduct S.CartDot sets
                            if cartProduct /= vs then
                                throwError $ setNotRectangularErrorMessage vs cartProduct
                            else return $ sets
                        _ -> return $ [vs]
