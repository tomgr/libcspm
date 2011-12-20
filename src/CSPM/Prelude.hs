-- | This module contains all the builtin definitions for the input CSPM
-- language.
module CSPM.Prelude (
    BuiltIn(..),
    builtins,
    transparentFunctionForOccName,
    externalFunctionForOccName
) 
where

import System.IO.Unsafe

import CSPM.DataStructures.Names
import CSPM.DataStructures.Types

data BuiltIn = 
    BuiltIn {
        name :: Name,
        stringName :: String,
        isDeprecated :: Bool,
        deprecatedReplacement :: Maybe Name,
        typeScheme :: TypeScheme,
        isTypeUnsafe :: Bool,
        isExternal :: Bool,
        isTransparent :: Bool
    }

instance Eq BuiltIn where
    b1 == b2 = name b1 == name b2

builtins :: Bool -> [BuiltIn]
builtins includeHidden = 
    if includeHidden then allBuiltins 
    else [b | b <- allBuiltins, not (isExternal b), not (isTransparent b)]

transparentFunctionForOccName :: OccName -> Maybe BuiltIn
transparentFunctionForOccName (OccName s) =
    let bs = [b | b <- allBuiltins, isTransparent b, stringName b == s] in
    if bs == [] then Nothing else Just (head bs)

externalFunctionForOccName :: OccName -> Maybe BuiltIn
externalFunctionForOccName (OccName s) =
    let bs = [b | b <- allBuiltins, isExternal b, stringName b == s] in
    if bs == [] then Nothing else Just (head bs)


allBuiltins :: [BuiltIn]
allBuiltins = unsafePerformIO makeBuiltins

makeBuiltins :: IO [BuiltIn] --Type -> (String, [Type], Type)]
makeBuiltins = 
    let
        cspm_union fv = ("union", [TSet fv, TSet fv], TSet fv)
        cspm_inter fv = ("inter", [TSet fv, TSet fv], TSet fv)
        cspm_diff fv = ("diff", [TSet fv, TSet fv], TSet fv)
        cspm_Union fv = ("Union", [TSet (TSet fv)], TSet fv)
        cspm_Inter fv = ("Inter", [TSet (TSet fv)], TSet fv)
        cspm_member fv = ("member", [fv, TSet fv], TBool)
        cspm_card fv = ("card", [TSet fv], TInt)
        cspm_empty fv = ("empty", [TSet fv], TBool)
        cspm_set fv = ("set", [TSeq fv], TSet fv)
        cspm_Set fv = ("Set", [TSet fv], TSet (TSet fv))
        cspm_Seq fv = ("Seq", [TSet fv], TSet (TSeq fv))
        cspm_seq fv = ("seq", [TSet fv], TSeq fv)

        sets = [cspm_union, cspm_inter, cspm_diff, cspm_Union, cspm_Inter,
                cspm_member, cspm_card, cspm_empty, cspm_set, cspm_Set,
                cspm_Seq, cspm_seq]

        cspm_length fv = ("length", [TSeq fv], TInt)
        cspm_null fv = ("null", [TSeq fv], TBool)
        cspm_head fv = ("head", [TSeq fv], fv)
        cspm_tail fv = ("tail", [TSeq fv], TSeq fv)
        cspm_concat fv = ("concat", [TSeq (TSeq fv)], TSeq fv)
        cspm_elem fv = ("elem", [fv, TSeq fv], TBool)       
        
        seqs = [cspm_length, cspm_null, cspm_head, cspm_tail, cspm_concat,
                cspm_elem]

        cspm_STOP = ("STOP", TProc)
        cspm_SKIP = ("SKIP", TProc)
        cspm_CHAOS = ("CHAOS", TFunction [TSet TEvent] TProc)
        cspm_prioritise = ("prioritise", TFunction [TProc, TSeq (TSet TEvent)] TProc)

        builtInProcs :: [(String, Type)]
        builtInProcs = [cspm_STOP, cspm_SKIP, cspm_CHAOS, cspm_prioritise]

        cspm_Int = ("Int", TSet TInt)
        cspm_Bool = ("Bool", TSet TBool)
        cspm_Proc = ("Proc", TSet TProc)
        cspm_Events = ("Events", TSet TEvent)
        cspm_true = ("true", TBool)
        cspm_false = ("false", TBool)
        cspm_True = ("True", TBool)
        cspm_False = ("False", TBool)        

        typeConstructors :: [(String, Type)]
        typeConstructors = [cspm_Int, cspm_Bool, cspm_Proc, cspm_Events, 
            cspm_true, cspm_false, cspm_True, cspm_False]

        externalFunctions :: [(String, Type)]
        externalFunctions = []

        transparentFunctions :: [(String, Type)]
        transparentFunctions = [
            ("diamond", TFunction [TProc] TProc),
            ("normal", TFunction [TProc] TProc),
            ("sbisim", TFunction [TProc] TProc),
            ("tau_loop_factor", TFunction [TProc] TProc),
            ("model_compress", TFunction [TProc] TProc),
            ("explicate", TFunction [TProc] TProc),
            ("wbisim", TFunction [TProc] TProc),
            ("chase", TFunction [TProc] TProc)
            ]

        externalNames = map fst externalFunctions
        transparentNames = map fst transparentFunctions

        mkFuncType cs func = do
            fv @ (TVar (TypeVarRef tv _ _)) <- freshTypeVarWithConstraints cs
            let (n, args, ret) = func fv
            let t = ForAll [(tv, cs)] (TFunction args ret)
            return (n, t)
        mkUnsafeFuncType n = do
            fv1 @ (TVar (TypeVarRef tv1 _ _)) <- freshTypeVarWithConstraints []
            fv2 @ (TVar (TypeVarRef tv2 _ _)) <- freshTypeVarWithConstraints []
            let t = ForAll [(tv1, []), (tv2, [])] (TFunction [fv1] fv2)
            return (n, t)
        mkPatternType func = do 
            let (n, t) = func
            return (n, ForAll [] t)

        unsafeFunctionNames :: [String]
        unsafeFunctionNames = ["productions", "extensions"]

        deprecatedNames :: [String]
        deprecatedNames = ["True", "False"]

        replacementForDeprecatedName :: String -> Maybe String
        replacementForDeprecatedName "True" = Just "true"
        replacementForDeprecatedName "False" = Just "false"
        replacementForDeprecatedName _ = Nothing

        makeBuiltIn :: (String, TypeScheme) -> IO BuiltIn
        makeBuiltIn (s, ts) = do
            n <- mkWiredInName (OccName s)
            return $ BuiltIn {
                name = n,
                stringName = s,
                isDeprecated = s `elem` deprecatedNames,
                deprecatedReplacement = Nothing,
                typeScheme = ts,
                isTypeUnsafe = s `elem` unsafeFunctionNames,
                isExternal = s `elem` externalNames,
                isTransparent = s `elem` transparentNames
            }
        
        makeReplacements :: [BuiltIn] -> BuiltIn -> IO BuiltIn
        makeReplacements bs b | isDeprecated b =
            case replacementForDeprecatedName (stringName b) of
                Just s' -> 
                    case filter (\b' -> stringName b' == s') bs of
                        [b'] -> return $ b { deprecatedReplacement = Just (name b') }
                        [] -> return b
                Nothing -> return b
        makeReplacements _ b = return b
    in do
        bs1 <- mapM (mkFuncType []) seqs
        bs2 <- mapM (mkFuncType [Eq]) sets
        bs3 <- mapM mkPatternType typeConstructors
        bs4 <- mapM mkPatternType builtInProcs
        bs5 <- mapM mkUnsafeFuncType unsafeFunctionNames
        bs6 <- mapM mkPatternType externalFunctions
        bs7 <- mapM mkPatternType transparentFunctions

        let bs = bs1++bs2++bs3++bs4++bs5++bs6++bs7

        bs' <- mapM makeBuiltIn bs
        bs'' <- mapM (makeReplacements bs') bs'

        return bs''
