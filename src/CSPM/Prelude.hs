-- | This module contains all the builtin definitions for the input CSPM
-- language.
module CSPM.Prelude (
    BuiltIn(..),
    builtins,
    builtInName,
    transparentFunctionForOccName,
    externalFunctionForOccName
) 
where

import qualified Data.Map as M
import System.IO.Unsafe

import CSPM.DataStructures.Names
import CSPM.DataStructures.Types
import Util.Exception

data BuiltIn = 
    BuiltIn {
        name :: Name,
        stringName :: String,
        isDeprecated :: Bool,
        deprecatedReplacement :: Maybe Name,
        typeScheme :: TypeScheme,
        isTypeUnsafe :: Bool,
        isExternal :: Bool,
        isHidden :: Bool,
        isTransparent :: Bool
    }

instance Eq BuiltIn where
    b1 == b2 = name b1 == name b2

bMap = M.fromList [(stringName b, name b) | b <- builtins True]
builtInName s = M.findWithDefault (panic "builtin not found") s bMap

builtins :: Bool -> [BuiltIn]
builtins includeHidden = 
    if includeHidden then allBuiltins 
    else [b | b <- allBuiltins, not (isHidden b), not (isExternal b),
            not (isTransparent b)]

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

makeBuiltins :: IO [BuiltIn]
makeBuiltins = do
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

        setsSets = [cspm_union, cspm_inter, cspm_diff, cspm_Union, cspm_Inter,
                    cspm_set, cspm_Set, cspm_Seq]
        -- The following require Eq as they allowing queries to be made about
        -- the set. In particular, the following all allow holes to be punched
        -- through the type checker and process values to be compared. For
        -- instance, member(P, {STOP}) card({STOP, P}) == 1,
        -- empty(diff({STOP}, {P})), length(seq({STOP, P})) == 1 all test if
        -- P == STOP.
        eqSets = [cspm_empty, cspm_card, cspm_member, cspm_seq]

        cspm_length fv = ("length", [TSeq fv], TInt)
        cspm_null fv = ("null", [TSeq fv], TBool)
        cspm_head fv = ("head", [TSeq fv], fv)
        cspm_tail fv = ("tail", [TSeq fv], TSeq fv)
        cspm_concat fv = ("concat", [TSeq (TSeq fv)], TSeq fv)
        cspm_elem fv = ("elem", [fv, TSeq fv], TBool)       
        
        seqs = [cspm_length, cspm_null, cspm_head, cspm_tail, cspm_concat]
        eqSeqs = [cspm_elem]

        cspm_STOP = ("STOP", TProc)
        cspm_SKIP = ("SKIP", TProc)
        cspm_CHAOS = ("CHAOS", TFunction [TSet TEvent] TProc)
        csp_tskip = ("TSKIP", TFunction [] TProc)
        csp_tstop = ("TSTOP", TFunction [] TProc)

        builtInProcs :: [(String, Type)]
        builtInProcs = [cspm_STOP, cspm_SKIP, cspm_CHAOS, csp_tstop, csp_tskip]

        cspm_Int = ("Int", TSet TInt)
        cspm_Bool = ("Bool", TSet TBool)
        cspm_Proc = ("Proc", TSet TProc)
        cspm_Events = ("Events", TSet TEvent)
        cspm_Char = ("Char", TSet TChar)
        cspm_true = ("true", TBool)
        cspm_false = ("false", TBool)
        cspm_True = ("True", TBool)
        cspm_False = ("False", TBool)        

        typeConstructors :: [(String, Type)]
        typeConstructors = [cspm_Int, cspm_Bool, cspm_Proc, cspm_Events, 
            cspm_Char, cspm_true, cspm_false, cspm_True, cspm_False]

        externalAndTransparentFunctions :: [(String, Type)]
        externalAndTransparentFunctions = [
            ("chase", TFunction [TProc] TProc),
            ("chase_nocache", TFunction [TProc] TProc)
            ]

        externalFunctions :: [(String, Type)]
        externalFunctions = [
            ("loop", TFunction [TProc] TProc),
            ("prioritise", TFunction [TProc, TSeq (TSet TEvent)] TProc)
            ]

        complexExternalFunctions :: IO [(String, TypeScheme)]
        complexExternalFunctions = do
            mtransclose <- do
                fv @ (TVar (TypeVarRef tv _ _)) <- freshTypeVarWithConstraints [CEq]
                return $ ForAll [(tv, [CEq])] 
                    (TFunction [TSet (TTuple [fv,fv]), TSet fv] (TSet (TTuple [fv,fv])))
            relational_image <- do
                fv1 @ (TVar (TypeVarRef tv1 _ _)) <- freshTypeVarWithConstraints [CEq]
                fv2 @ (TVar (TypeVarRef tv2 _ _)) <- freshTypeVarWithConstraints [CSet]
                return $ ForAll [(tv1, [CEq]), (tv2, [CSet])] 
                    (TFunction [TSet (TTuple [fv1,fv2])] (TFunction [fv1] (TSet fv2)))
            relational_inverse_image <- do
                fv1 @ (TVar (TypeVarRef tv1 _ _)) <- freshTypeVarWithConstraints [CEq]
                fv2 @ (TVar (TypeVarRef tv2 _ _)) <- freshTypeVarWithConstraints [CSet]
                return $ ForAll [(tv2, [CEq]), (tv1, [CSet])] 
                    (TFunction [TSet (TTuple [fv1,fv2])] (TFunction [fv2] (TSet fv1)))
            transpose <- do
                fv1 @ (TVar (TypeVarRef tv1 _ _)) <- freshTypeVarWithConstraints [CSet]
                fv2 @ (TVar (TypeVarRef tv2 _ _)) <- freshTypeVarWithConstraints [CSet]
                return $ ForAll [(tv1, [CSet]), (tv2, [CSet])] 
                    (TFunction [TSet (TTuple [fv1,fv2])] (TSet (TTuple [fv2,fv1])))
            return [
                ("mtransclose", mtransclose),
                ("relational_image", relational_image),
                ("relational_inverse_image", relational_inverse_image),
                ("transpose", transpose)
                ]

        transparentFunctions :: [(String, Type)]
        transparentFunctions = [
            ("diamond", TFunction [TProc] TProc),
            ("normal", TFunction [TProc] TProc),
            ("sbisim", TFunction [TProc] TProc),
            ("tau_loop_factor", TFunction [TProc] TProc),
            ("model_compress", TFunction [TProc] TProc),
            ("explicate", TFunction [TProc] TProc),
            ("wbisim", TFunction [TProc] TProc)
            ]

        cspm_error fv = ("error", [TSeq TChar], fv)
        cspm_show fv = ("show", [fv], TSeq TChar)

        fdr3Extensions = [cspm_error, cspm_show]

    complexExternals <- complexExternalFunctions

    let
        externalNames =
            map fst externalAndTransparentFunctions
            ++map fst externalFunctions++map fst complexExternals
        transparentNames =
            map fst transparentFunctions
            ++map fst externalAndTransparentFunctions

        hiddenNames = ["TSKIP", "TSTOP"]

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
        unsafeFunctionNames = []

        deprecatedNames :: [String]
        deprecatedNames = []

        replacementForDeprecatedName :: String -> Maybe String
        replacementForDeprecatedName _ = Nothing

        makeBuiltIn :: (String, TypeScheme) -> IO BuiltIn
        makeBuiltIn (s, ts) = do
            n <- mkWiredInName (UnQual (OccName s)) False
            return $ BuiltIn {
                name = n,
                stringName = s,
                isDeprecated = s `elem` deprecatedNames,
                deprecatedReplacement = Nothing,
                typeScheme = ts,
                isHidden = s `elem` hiddenNames,
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

        makeExtensionsProductions = do
            fv1 @ (TVar (TypeVarRef tv1 _ _)) <- freshTypeVarWithConstraints []
            fv2 @ (TVar (TypeVarRef tv2 _ _)) <- freshTypeVarWithConstraints []
            let t1 = ForAll [(tv1, []), (tv2, [CYieldable])]
                        (TFunction [TDotable fv1 fv2] (TSet fv1))
            fv2 @ (TVar (TypeVarRef tv2 _ _)) <- freshTypeVarWithConstraints []
            fv2ref <- freshTypeVarRef []
            let t2 = ForAll [(tv2, [CYieldable])]
                        (TFunction [TExtendable fv2 fv2ref] (TSet fv2))
            return [("extensions", t1), ("productions", t2)]

    bs1 <- mapM (mkFuncType []) seqs
    bs2 <- mapM (mkFuncType [CSet]) setsSets
    bs2' <- mapM (mkFuncType [CEq]) (eqSets ++ eqSeqs)
    bs3 <- mapM mkPatternType typeConstructors
    bs4 <- mapM mkPatternType builtInProcs
    bs5 <- makeExtensionsProductions
    bs6 <- mapM mkPatternType externalFunctions
    bs7 <- mapM mkPatternType transparentFunctions
    bs8 <- mapM mkPatternType externalAndTransparentFunctions
    bs9 <- mapM (mkFuncType []) fdr3Extensions

    let bs = bs1++bs2++bs2'++bs3++bs4++bs5++bs6++bs7++complexExternals++bs8++bs9

    bs' <- mapM makeBuiltIn bs
    bs'' <- mapM (makeReplacements bs') bs'
    
    return bs''
