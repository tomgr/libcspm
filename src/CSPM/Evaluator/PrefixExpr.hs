{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
module CSPM.Evaluator.PrefixExpr (
    evalPrefix,
) where

import qualified Data.Foldable as F
import Data.Maybe
import qualified Data.Set as St

import CSPM.DataStructures.FreeVars
import CSPM.DataStructures.Names
import CSPM.DataStructures.Syntax
import CSPM.Evaluator.AnalyserMonad
import CSPM.Evaluator.Dot
import CSPM.Evaluator.Exceptions
import {-# SOURCE #-} CSPM.Evaluator.Expr
import CSPM.Evaluator.Monad
import CSPM.Evaluator.PatBind
import CSPM.Evaluator.Values
import qualified CSPM.Evaluator.ValueSet as S
import Util.Annotated
import Util.Exception

-- | Returns true if the field binds no values.
fieldBindsNoValues :: TCField -> Bool
fieldBindsNoValues (An _ _ (Input (An _ _ PWildCard) _)) = True
fieldBindsNoValues (An _ _ (Output _)) = True
fieldBindsNoValues (An _ _ (NonDetInput (An _ _ PWildCard) _)) = True
fieldBindsNoValues _ = False

-- | True iff a field is guaranteed to match any proposed value. This is true
-- for fields such as ?x or !x, but not of ?0:... etc.
fieldAlwaysMatches :: TCField -> Bool
fieldAlwaysMatches (An _ _ (Input p _)) = patternAlwaysMatches p
fieldAlwaysMatches (An _ _ (Output _)) = True
fieldAlwaysMatches (An _ _ (NonDetInput p _)) = patternAlwaysMatches p

-- | True iff the variables bound by the field are not used by other fields
-- For example, it would be true on ?x?y, but false on ?x?y:f(x).
fieldsAreIndependent :: [TCField] -> Bool
fieldsAreIndependent fields =
        and (map (not . flip St.member fvs) (boundNames fields))
    where
        fvs = St.fromList (freeVars fields)

-- | Returns true if the field is a nondeterministic input.
isNonDet :: TCField -> Bool
isNonDet (An _ _ (NonDetInput _ _)) = True
isNonDet _ = False

-- | Converts a pattern to its constituent fields.
patToFields :: TCPat -> [TCPat]
patToFields (An _ _ (PCompDot ps _)) = ps
patToFields (An _ _ (PDoublePattern p1 p2)) = panic "double prefix pat not implemented"
    -- patToFields p1
patToFields p = [p]

type Binder = Value -> Maybe [(Name, Value)]

evalInputField ::
    SrcSpan -> Value -> 
    Binder ->
    S.ValueSet -> 
    (Value -> EvaluationMonad [a]) ->
    EvaluationMonad [a]
evalInputField loc evBase binder s evalRest =
    F.foldrM (\ v foundValues -> do
        case binder v of
            Just binds -> do
                ev' <- combineDots loc evBase v
                p <- addScopeAndBind binds $ evalRest ev'
                return $! foundValues ++ p
            Nothing -> return foundValues) [] (S.toList s)

-- | Evalutates an input field, deducing the correct set of values
-- to input over.
evalInputField2 :: forall a . 
    SrcSpan -> Bool -> TCPat ->
    AnalyserMonad (Value -> EvaluationMonad [a]) ->
    AnalyserMonad (Value -> EvaluationMonad [a])
evalInputField2 loc isLastField p evalRest = 
    let
        -- | The function to use to generate the options. If this
        -- is the last field AND the last pattern in the current
        -- field it uses 'extensions' to extend to a fully formed 
        -- event, otherwise we use 'oneFieldExtensions' to extend 
        -- by precisely one field.
        extensionsOperator :: 
            [TCPat] -> Value -> EvaluationMonad [Value]
        extensionsOperator ps | not isLastField = oneFieldExtensions
        extensionsOperator [p] = extensions 
        extensionsOperator _ = oneFieldExtensions

        allPats = patToFields p
        
        -- | Given a value and a list of patterns (from 
        -- 'patToFields') computes the appropriate set of events and
        -- then evaluates it.
        evExtensions :: [TCPat] ->
            AnalyserMonad (Value -> [(Name, Value)] ->
                EvaluationMonad [a])
        evExtensions [] = do
            createVariableFrame allPats $! \ _ -> do
                rest <- evalRest
                return $! \ evBase bs -> addScopeAndBind bs $ rest evBase
        evExtensions (An _ _ (PVar n):ps) | isNameDataConstructor n = do
            rest <- evExtensions ps
            return $! \ evBase bs -> do
                (dc, _, _) <- dataTypeInfo n
                evBase' <- combineDots loc evBase dc
                rest evBase' bs
        evExtensions (p:ps) = do
            let extOp = extensionsOperator (p:ps)
            binder <- bind p
            rest <- evExtensions ps
            return $! \ evBase bs -> do
                vs <- extOp evBase
                mps <- mapM (\v -> do
                        case binder v of
                            Just bs' -> do
                                evBase' <- combineDots loc evBase v
                                proc <- rest evBase' (bs++bs')
                                return $ Just proc
                            _ -> return Nothing) vs
                return $ F.msum $ catMaybes mps
    in do
        computeField <- evExtensions allPats
        return $! \ evBase -> computeField evBase []
            

evalPrefix :: TCExp -> AnalyserMonad (EvaluationMonad Value)
evalPrefix (An _ _ (Prefix e1 fs e2)) = do
    let
        evalNonDetFields :: [TCField] -> AnalyserMonad (
            Value -> EvaluationMonad [Proc])
        evalNonDetFields (An loc _ (NonDetInput p (Just e')):fs) = do
            e <- eval e'
            binder <- bind p
            rest <- createVariableFrame' p $ evalNonDetFields fs
            return $! \ evBase -> do
                VSet s <- e
                ps <- evalInputField loc evBase binder s rest
                if null ps then
                    throwError' $ replicatedInternalChoiceOverEmptySetMessage e'
                else return ps
        evalNonDetFields (An loc _ (NonDetInput p Nothing):fs) = do
            let isLastField = fs == []
            evalField <- evalInputField2 loc isLastField p (evalNonDetFields fs)
            return $! \ evBase -> do
                ps <- evalField evBase
                if null ps then
                    throwError' $ replicatedInternalChoiceOverEmptySetMessage' p
                else return ps
        evalNonDetFields fs = do
            rest <- evalFields fs
            return $! \evBase -> do
                ps <- rest evBase
                return $! [POp PExternalChoice ps]

        evalFields :: [TCField] -> AnalyserMonad (
            Value -> EvaluationMonad [Proc])
        evalFields [] = do
            e2 <- eval e2
            return $! \ ev -> do
                VProc p <- e2
                return $! [PUnaryOp (PPrefix (valueEventToEvent ev)) p]
        evalFields (An loc _ (Output e):fs) = do
            e <- eval e
            rest <- evalFields fs
            return $! \ evBase -> do
                v <- e
                ev' <- combineDots loc evBase v
                rest ev'
        evalFields (An loc _ (Input p (Just e)):fs) = do
            e <- eval e
            binder <- bind p
            rest <- createVariableFrame' p $ evalFields fs
            return $! \ evBase -> do
                VSet s <- e
                evalInputField loc evBase binder s rest
        evalFields (An loc _ (Input p Nothing):fs) = do
            let isLastField = fs == []
            evalInputField2 loc isLastField p (evalNonDetFields fs)
        evalFields (An _ _ (NonDetInput _ _):fs) = 
            panic "Evaluation of $ after ! or ? is not supported."


        noNonDetFields = and (map (not . isNonDet) fs)
        fieldsBindNothing = and (map fieldBindsNoValues fs)
        indepFields = fieldsAreIndependent fs

    --    dotAppFieldTypes e@(An _ _ (DotApp e1 e2)) =
    --        getType e1 : extractFieldTypes e2
    --    dotAppFieldTypes e = getType e

    --    extractFieldTypes (An _ _ (Input p _)) = getType p
    --    extractFieldTypes (An _ _ (NonDetInput p _)) = getType p
    --    extractFieldTypes (An _ _ (Output e)) = getType e

        fallback =
            if noNonDetFields && fieldsBindNothing then do
                -- Then the fields are independent of the result, and only contain
                -- deterministic inputs, so this is equivalent to offering a set of
                -- events with a common destination.
                e1 <- eval e1
                computeProc <- evalFieldsNoBranching fs e2
                return $! do
                    ev <- e1
                    p <- computeProc ev
                    return $ VProc p
            else do
                e1 <- eval e1
                computeProc <- evalNonDetFields fs
                return $! do
                    ev <- e1
                    ps <- computeProc ev
                    let p = case F.toList ps of
                                [p] -> p
                                _ -> POp PInternalChoice ps
                    return $ VProc p


    fallback
    
    --case e1 of
    --    An _ _ (DotApp (An _ _ (Var n)) rhs) | isNameDataConstructor n -> do
    --        channelInfo <- channelInformationForName n
    --        let actualFieldTypes = dotAppFieldTypes rhs ++ extractFieldTypes fields
    --            decomposedFieldCount = length fieldTypes
    --        if noNonDetFields
    --            && constructorFieldCount channelInfo == decomposedFieldCount
    --            && constructorFieldTypes channelInfo == actualFieldTypes
    --            && indepFields
    --            && and (map fieldAlwaysMatches fs) then do

    --            e1 <- eval e1
    --            evalFields <- evalFieldsUsingCartProduct fs
    --            return $! do
    --                ev <- e1
    --                p <- evalFields ev
    --                return $! VProc p
    --        else
    --            fallback

    --    _ -> fallback

-- Other cases we should optimise for:
--
-- c?x:X?y:Y?z:Y when:
--      c can be statically determined;
--      all fields are external choice inputs or outputs;
--      the number of fields matches the arity of the channel;
--      the types of each field correspond to the types of the channel's fields;
--      the fields are indepenendent (i.e. the vars bound by the fields are only
--      only used on the RHS process, if anywhere).
--      the patterns are guaranteed to match (i.e. are _ or a simple var).
--
-- If all of the above are true, we can:
--      For any field of the form ?x:X just check X is a subset of the field
--      set.
--      For any field of the form ?x just use the field set.
--      Take the cart product of the above sets and compute all events that
--      we obtain.
--      If the RHS is completely independent, compute the RHS once and construct
--      an event set.
--      Otherwise, iterate over all things in the cart product and eval the RHS
--      in each case. BUT HOW DO WE DO THE BINDING? Do the binding after doing
--      the cart product. The main difference is that we will compute the set
--      once only, rather than once for each value of the prior fields.
--
-- This should cover a large number of common cases.
--
--
-- We could probably optimise more agressively than this and simply not branch
-- the computation so much in evalFields (and evalFieldsNonDet). Instead, these
-- functions would take a set of values which will be prefixes, or something
-- like that. 

--evalFieldsUsingCartProduct :: Name -> [TCField] ->
--    AnalyserMonad (Value -> EvaluationMonad (S.Seq Value))
--evalFieldsUsingCartProduct channelName fs =
--    let
--        --buildBinder :: Int -> [TCField] -> AnalyserMonad (Value -> EvaluationMonad Value)

--        extractSet :: Int -> [TCField] -> AnalyserMonad (EvaluationMonad [S.ValueSet])
--        extractSet _ [] = return $! return []
--        extractSet fieldIx (An loc _ (Output e) : fs) = do
--            e <- eval e
--            rest <- extractSet (fieldIx+1) fs
--            return $! do
--                v <- e
--                ss <- rest
--                return $! S.singleton v : ss
--        extractSet fieldIx (An loc _ (Input p (Just e)) : fs) = do
--            e <- eval e
--            rest <- extractSet (fieldIx+1) fs
--            return $! do
--                v <- e
--                ss <- rest
--                return $! v : ss
--        extractSet fieldIx (An loc _ (Input p Nothing) : fs) = do
--            rest <- extractSet (fieldIx+1) fs
--            return $! do
--                (_, _, fieldSets) <- dataTypeInfo channelName
--                let v = fieldSets!fieldIx
--                ss <- rest
--                return $! v : ss
--    in do
--        computeSets <- extractSet (length prefixFields) fs
--        return $! \ ev -> do
--            sets <- computeSets
--            panic "What to do"

-- | Evaluates the given fields, assuming that the fields introduce no branching
-- and are all external choice inputs, not nondet inputs. This optimises this
-- case and only computes the resulting process precisely once.
evalFieldsNoBranching :: [TCField] -> TCExp ->
    AnalyserMonad (Value -> EvaluationMonad Proc)
evalFieldsNoBranching fs e2 = do
    let
        evalFields :: [TCField] -> AnalyserMonad
            (Value -> EvaluationMonad [Value])
        evalFields [] = return $! \ v -> return [v]
        evalFields (An loc _ (Output e) : fs) = do
            e <- eval e
            rest <- evalFields fs
            return $! \ evBase -> do
                v <- e
                ev' <- combineDots loc evBase v
                rest ev'
        evalFields  (An loc _ (Input p (Just e)):fs) = do
            e <- eval e
            binder <- bind p
            rest <- createVariableFrame' p $ evalFields fs
            return $! \ evBase -> do
                VSet s <- e
                evalInputField loc evBase binder s rest
        evalFields  (An loc _ (Input p Nothing):fs) = do
            let isLastField = fs == []
            evalField <- evalInputField2 loc isLastField p (evalFields fs)
            return $! evalField

    computeEvents <- evalFields fs
    computeProc <- eval e2
    return $! \ ev -> do
        evs <- computeEvents ev
        VProc p <- computeProc
        case F.toList evs of
            [ev] -> return $ PUnaryOp (PPrefix (valueEventToEvent ev)) p
            _ -> return $ PUnaryOp (PPrefixEventSet (fmap valueEventToEvent evs)) p
