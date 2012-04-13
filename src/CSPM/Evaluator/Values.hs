module CSPM.Evaluator.Values (
    Value(..), Proc(..), ProcOperator(..), Event(..),
    compareValues,
    procId, annonymousProcId,
    valueEventToEvent,
    combineDots,
    extensions, oneFieldExtensions,
    productions,
) where

import Control.Monad
import Data.Foldable (foldrM)
import Data.Hashable

import CSPM.Compiler.Events
import CSPM.Compiler.Processes
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.Monad
import {-# SOURCE #-} CSPM.Evaluator.ValueSet hiding (cartesianProduct)
import CSPM.PrettyPrinter
import Util.Exception
import Util.List
import Util.Prelude
import Util.PrettyPrint

data Value =
    VInt Int
    | VBool Bool
    | VTuple [Value]
    -- | If A is a datatype clause that has 3 fields a b c then a runtime
    -- instantiation of this would be VDot [VDataType "A", a, b, c] where a,b
    -- and c can contain other VDots.
    | VDot [Value]
    -- The following two never appear on their own, they are always part of a 
    -- VDot (even if the VDot has no values).
    | VChannel Name
    | VDataType Name
    | VList [Value]
    | VSet ValueSet
    | VFunction ([Value] -> EvaluationMonad Value)
    | VProc Proc

instance Hashable Value where
    hash (VInt i) = combine 1 (hash i)
    hash (VBool b) = combine 2 (hash b)
    hash (VTuple vs) = combine 5 (hash vs)
    hash (VDot vs) = combine 6 (hash vs)
    hash (VChannel n) = combine 7 (hash n)
    hash (VDataType n) = combine 8 (hash n)
    hash (VList vs) = combine 9 (hash vs)
    hash (VSet vset) = combine 10 (hash vset)
    hash (VFunction f) = panic "Cannot hash a function"
    hash (VProc (PProcCall n _)) = combine 12 (hash n)
    hash (VProc _) = panic "Cannot hash a process"

instance Eq Value where
    VInt i1 == VInt i2 = i1 == i2
    VBool b1 == VBool b2 = b1 == b2
    VTuple vs1 == VTuple vs2 = vs1 == vs2
    VDot vs1 == VDot vs2 = vs1 == vs2
    VChannel n1 == VChannel n2 = n1 == n2
    VDataType n1 == VDataType n2 = n1 == n2
    VList vs1 == VList vs2 = vs1 == vs2
    VSet s1 == VSet s2 = s1 == s2
    
    v1 == v2 = False
    
-- | Implements CSPM comparisons (note that Ord Value does not).
compareValues :: Value -> Value -> Maybe Ordering
-- The following are all orderable and comparable
compareValues (VInt i1) (VInt i2) = Just (compare i1 i2)
compareValues (VBool b1) (VBool b2) = Just (compare b1 b2)
compareValues (VTuple vs1) (VTuple vs2) =
    -- Tuples must be same length by type checking
    -- Tuples are ordered lexiographically
    let
        cmp [] [] = EQ
        cmp (x:xs) (y:ys) = compare x y `thenCmp` cmp xs ys
    in Just (cmp vs1 vs2)
compareValues (VList vs1) (VList vs2) =
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
compareValues (VSet s1) (VSet s2) = compareValueSets s1 s2

-- The following can only be compared for equality, hence if they are not
-- equal we return Nothing.
compareValues (VChannel n1) (VChannel n2) = 
    if n1 == n2 then Just EQ else Nothing
compareValues (VDataType n1) (VDataType n2) = 
    if n1 == n2 then Just EQ else Nothing
compareValues (VDot vs1) (VDot vs2) =
    if vs1 == vs2 then Just EQ else Nothing

-- Every other possibility is invalid
compareValues v1 v2 = panic $
    "Cannot compare "++show v1++" "++show v2

instance Ord Value where
    -- This implementation is used for various internal measures, but not
    -- for implementing actual comparisons in CSPM.
    compare (VInt i1) (VInt i2) = compare i1 i2
    compare (VBool b1) (VBool b2) = compare b1 b2
    compare (VTuple vs1) (VTuple vs2) = compare vs1 vs2
    compare (VList vs1) (VList vs2) = compare vs1 vs2
    compare (VSet s1) (VSet s2) = compare s1 s2    
    -- These are only ever used for the internal set implementation
    compare (VDot vs1) (VDot vs2) = compare vs1 vs2
    compare (VChannel n) (VChannel n') = compare n n'
    compare (VDataType n) (VDataType n') = compare n n'

    compare v1 v2 = panic $
        -- Must be as a result of a mixed set of values, which cannot happen
        -- as a result of type checking.
        "Internal sets - cannot order "++show v1++" "++show v2

instance PrettyPrintable Value where
    prettyPrint (VInt i) = int i
    prettyPrint (VBool True) = text "true"
    prettyPrint (VBool False) = text "false"
    prettyPrint (VTuple vs) = parens (list $ map prettyPrint vs)
    prettyPrint (VDot vs) = dotSep (map prettyPrint vs)
    prettyPrint (VChannel n) = prettyPrint n
    prettyPrint (VDataType n) = prettyPrint n
    prettyPrint (VList vs) = angles (list $ map prettyPrint vs)
    prettyPrint (VSet s) = prettyPrint s
    prettyPrint (VFunction _) = text "<function>"
    prettyPrint (VProc p) = prettyPrint p

instance Show Value where
    show v = show (prettyPrint v)

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
                    else if length vs == 0 then
                        return $ Just (VDot [nd, v])
                    else do
                        -- Try and dot it onto our last field
                        mv <- maybeDotFieldOn (last vs) v
                        case mv of
                            Just vLast ->
                                return $ Just (VDot (nd:replaceLast vs vLast))
                            -- Start a new field, or return nothing if we
                            -- are full
                            Nothing | fieldCount < a -> 
                                return $ Just (VDot (nd:vs++[v]))
                            Nothing | fieldCount == a -> return Nothing
                            Nothing | fieldCount > a -> panic "Malformed dot encountered."
        maybeDotFieldOn vbase v = return Nothing

        -- | Dots the two values together, ensuring that if either the left or
        -- the right value is a dot list combines them into one dot list.
        dotAndReduce :: Value -> Value -> Value
        dotAndReduce (VDot (VDataType n1:vs1)) (VDot (VDataType n2:vs2)) =
            VDot [VDot (VDataType n1:vs1), VDot (VDataType n2:vs2)]
        dotAndReduce (VDot (VDataType n1:vs1)) (VDot vs2) =
            VDot (VDot (VDataType n1:vs1) : vs2)
        dotAndReduce (VDot vs1) (VDot (VDataType n2:vs2)) =
            VDot (vs1 ++ [VDot (VDataType n2:vs2)])
        dotAndReduce v1 v2 = VDot [v1, v2]

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

procId :: Name -> [[Value]] -> Maybe ProcName -> ProcName
procId n vss pn = ProcName n vss pn

annonymousProcId :: [[Value]] -> Maybe ProcName -> ProcName
annonymousProcId vss pn = AnnonymousProcName vss pn

-- | This assumes that the value is a VDot with the left is a VChannel
valueEventToEvent :: Value -> Event
valueEventToEvent v = UserEvent (show (prettyPrint v))

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
                            (head [s | VList s <- drop (length vs) fieldSets])
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
                    remainingFields = [s | VList s <- drop (length vs) fieldSets]
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
