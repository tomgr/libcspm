module CSPM.TypeChecker.Unification (
    generaliseGroup, generaliseSubGroup, instantiate, unify, unifyAll,
    evaluateDots, typeToDotList, dotableToDotList, substituteTypes, instantiate',
    substituteTypeScheme, freeTypeVars,
) where

import Control.Monad
import Data.List (nub, sort)
import qualified Data.Set as S
import Prelude

import CSPM.Syntax.Names
import CSPM.Syntax.Types
import CSPM.TypeChecker.Exceptions
import CSPM.TypeChecker.Monad
import Util.Exception
import Util.Monad

-- | Return the free type variables (and their constraints) for all 'TypeVar's 
-- that occur in 'Type'.
freeTypeVars :: Type -> TypeCheckMonad [(TypeVar, [Constraint])]
freeTypeVars = liftM (nub . sort) . freeTypeVars'    
freeTypeVars' :: Type -> TypeCheckMonad [(TypeVar, [Constraint])]
freeTypeVars' (TVar tv) = do
    typ <- readTypeRef tv
    case typ of
        Left (tv, cs)   -> return [(tv, cs)]
        Right t         -> freeTypeVars' t
freeTypeVars' (TFunction targs tr) = 
    liftM concat (mapM freeTypeVars' (tr:targs))
freeTypeVars' (TSeq t) = freeTypeVars' t
freeTypeVars' (TSet t) = freeTypeVars' t
freeTypeVars' (TTuple ts) = liftM concat (mapM freeTypeVars' ts)
freeTypeVars' (TDotable t1 t2) = liftM concat (mapM freeTypeVars' [t1,t2])
freeTypeVars' (TDot t1 t2) = liftM concat (mapM freeTypeVars' [t1,t2])
freeTypeVars' (TMap t1 t2) = liftM concat (mapM freeTypeVars' [t1,t2])
freeTypeVars' (TDatatype n1) = return []
freeTypeVars' TInt = return []
freeTypeVars' TBool = return []
freeTypeVars' TEvent = return []
freeTypeVars' TChar = return []
freeTypeVars' (TExtendable t pt) = do
    fvs1 <- freeTypeVars' t
    res <- readTypeRef pt
    case res of 
        Left (tv, cs) -> do
            return $ (tv, cs):fvs1
        Right t -> do
            fvs2 <- freeTypeVars' t
            return $ fvs1 ++ fvs2
freeTypeVars' TProc = return []
freeTypeVars' TExtendableEmptyDotList = return []

-- | Generalise the types of the declarations. The parameter 'names' gives the 
-- names that were bound by all the declarations that we are interested in. This
-- is done because we convert a type T into forall vs T where 
--      vs = fvts (T) - fvts(Env)
-- where Env does not contain the function whose type we are generalizing
-- (this is because when we type a declaration we are really typing a 
-- lambda function).
generaliseGroup :: [TypeCheckMonad [(Name, Type)]] -> 
    TypeCheckMonad [[(Name, TypeScheme)]]
generaliseGroup tsm = do
    -- Perform the type checking
    ts <- sequence tsm
    (_ : envfvs'' : _) <- currentTypeVariableContexts
    -- envfvs'' consists of all TypeRefs that were registered before this type
    -- context was opened. However, it's possible that during unification inside
    -- the current type context, some typerefs inside envfvs'' were actually
    -- subsituted or. Hence, we compute the freeTypeVars of every type in
    -- envfvs''.
    envfvs' <- mapM (freeTypeVars . TVar) (S.toList envfvs'')
    let envfvs = S.fromList $! map fst (concat envfvs')
    mapM (\ nts -> mapM (\ (n,t) -> do
        -- The free vars in this type
        deffvs <- freeTypeVars t
        -- All the free variables that were actually bound by this declaration 
        -- (rather than some other declaration in the environment).
        let unboundVars = filter (\ (fv, cs) -> not (S.member fv envfvs)) deffvs
            ts = ForAll unboundVars t
        setType n ts
        return (n, ts)) nts) ts

generaliseSubGroup :: [Name] -> [TypeCheckMonad [(Name, Type)]] -> 
                    TypeCheckMonad [[(Name, TypeScheme)]]
generaliseSubGroup toGeneralise tsm = do
    -- Perform the type checking
    ts <- sequence tsm
    (_ : envfvs'' : _) <- currentTypeVariableContexts
    envfvs' <- mapM (freeTypeVars . TVar) (S.toList envfvs'')
    let envfvs = S.fromList $! map fst (concat envfvs')
    mapM (\ nts -> mapM (\ (n,t) -> do
        -- The free vars in this type
        deffvs <- freeTypeVars t
        -- All the free variables that were actually bound by this declaration 
        -- (rather than some other declaration in the environment).
        let unboundVars = filter (\ (fv, cs) -> not (S.member fv envfvs)) deffvs
            ts = if n `elem` toGeneralise then ForAll unboundVars t else ForAll [] t
        setType n ts
        return (n, ts)) nts) ts

-- | Instantiates the typescheme with some fresh type variables.
instantiate :: TypeScheme -> TypeCheckMonad Type
instantiate ts = instantiate' ts >>= return . fst

instantiate' :: TypeScheme -> TypeCheckMonad (Type, [(TypeVar, Type)])
instantiate' (ForAll ts t) = do
    tvs <- mapM (freshRegisteredTypeVarWithConstraints . snd) ts
    let sub = (zip (map fst ts) tvs)
    t <- substituteTypes sub t
    return (t, sub)

-- | Does 'a' occur somewhere in 't'.
occurs :: TypeVar -> Type -> TypeCheckMonad Bool
occurs a (TVar tvref) = do
    res <- readTypeRef tvref
    case res of 
        Left (tv, cs) -> return $ a == tv
        Right t -> occurs a t
occurs a (TSet t) = occurs a t
occurs a (TSeq t) = occurs a t
occurs a (TDot t1 t2) = liftM or (mapM (occurs a) [t1,t2])
occurs a (TMap t1 t2) = liftM or (mapM (occurs a) [t1,t2])
occurs a (TTuple ts) = liftM or (mapM (occurs a) ts)
occurs a (TFunction ts t) = liftM or (mapM (occurs a) (t:ts))
occurs a (TDatatype n) = return False
occurs a (TDotable t1 t2) = liftM or (mapM (occurs a) [t1,t2])
occurs a TInt = return False
occurs a TBool = return False
occurs a TProc = return False
occurs a TEvent = return False
occurs a TChar = return False
occurs a (TExtendable t pt) = do
    b <- occurs a t
    if b then return True else do 
    res <- readTypeRef pt
    case res of 
        Left (tv, cs) | a == tv -> return True
        Left _ -> return False
        Right t -> occurs a t
occurs a TExtendableEmptyDotList = return False

-- | Unifys all types to a single type. The first type is  used as the 
-- expected Type in error messages.
unifyAll :: [Type] -> TypeCheckMonad Type
unifyAll [] = freshRegisteredTypeVar
unifyAll [t] = return t
unifyAll (t1:ts) = do
    t2 <- unifyAll ts
    unify t1 t2

-- | Takes a constraint and a type and returns True iff the type satisfies the
-- constraint, or can be made to satsify the constraint by appropriate type
-- substitutions, in which case the type substitutions are performed.
unifyConstraint :: Constraint -> Type -> TypeCheckMonad ()
unifyConstraint c typ =
    addConstraintUnificationPair (c, typ) $! unifyConstraintNoStk c typ

unifyConstraintNoStk c (TVar v) | isRigid v = do
    when (not (or (map (constraintImpliedBy c) (constraints v)))) $ do
        raiseConstraintErrorMessage
unifyConstraintNoStk c (TVar v) = do
    res <- readTypeRef v
    case res of
        Left (tva, cs)  -> 
            when (not (or (map (constraintImpliedBy c) cs))) $ do
                fv <- freshRegisteredTypeVarWithConstraints (nub (sort (c:cs)))
                applySubstitution v fv
                return ()
        Right t         -> unifyConstraintNoStk c t
unifyConstraintNoStk c (TExtendable t pt) = do
    res <- readTypeRef pt
    case res of
        Left _  ->
            if c == CYieldable then
                safeWriteTypeRef pt TExtendableEmptyDotList
            else if c == CComplete then do
                unifyConstraint c t
                safeWriteTypeRef pt TExtendableEmptyDotList
            else 
                when (c /= CEq && c /= CSet) $ raiseConstraintErrorMessage
        Right t -> do
            t' <- compress (TExtendable t pt)
            unifyConstraint c t'
unifyConstraintNoStk CYieldable (TDatatype n) = return ()
unifyConstraintNoStk CYieldable TEvent = return ()
unifyConstraintNoStk CYieldable (TDot t1 t2) = do
    t <- evaluateDots (TDot t1 t2)
    case t of
        TDot _ _ -> raiseConstraintErrorMessage
        _ -> unifyConstraint CYieldable t
unifyConstraintNoStk CYieldable t = raiseConstraintErrorMessage
unifyConstraintNoStk CSet TProc = return ()
unifyConstraintNoStk CComplete TProc = return ()
unifyConstraintNoStk c TInt = return ()
unifyConstraintNoStk c TChar = return ()
 -- Bools are not orderable
unifyConstraintNoStk c TBool | c /= COrd = return ()
unifyConstraintNoStk CEq (TDatatype n) = do
    b <- datatypeIsComparableForEquality n
    if b then return ()
    else raiseConstraintErrorMessage
-- User data types are not orderable
unifyConstraintNoStk c (TDatatype n) | c /= COrd = return ()
unifyConstraintNoStk c TEvent | c /= COrd = return ()
unifyConstraintNoStk CComplete (TSeq _) = return ()
unifyConstraintNoStk CSet (TSeq t) = unifyConstraint CSet t
unifyConstraintNoStk CEq (TSeq t) = unifyConstraint CEq t
-- Ordering sequenecs means prefixing testing, which requires only equality
unifyConstraintNoStk COrd (TSeq t) = unifyConstraint CEq t
unifyConstraintNoStk CComplete (TTuple _) = return ()
unifyConstraintNoStk c (TTuple ts) = mapM_ (unifyConstraint c) ts
-- Dotable types are not orderable (as events and datatypes are not). Nor are
-- they Complete. We can create sets of them though.
unifyConstraintNoStk CEq (TDotable a b) = unifyConstraint CEq a >> unifyConstraint CEq b
unifyConstraintNoStk CSet (TDotable a b) = unifyConstraint CSet a >> unifyConstraint CSet b
unifyConstraintNoStk CComplete (TDot t1 t2) = do
    t <- evaluateDots (TDot t1 t2)
    case t of
        TDot t1 t2 -> mapM_ (unifyConstraint CComplete) [t1,t2]
        _ -> unifyConstraint CComplete t
unifyConstraintNoStk c (TDot t1 t2) | c /= COrd =
    unifyConstraint c t1 >> unifyConstraint c t2
unifyConstraintNoStk CSet (TSet t) = unifyConstraint CSet t
unifyConstraintNoStk CComplete (TSet t) = return ()
unifyConstraintNoStk CEq (TSet t) = unifyConstraint CEq t
-- sets are comparable for equality/orderable iff their inner type is
-- comparable for equality (as set ordering means subset testing, which requires
-- only equality)
unifyConstraintNoStk COrd (TSet t) = unifyConstraint CEq t
unifyConstraintNoStk CComplete (TMap _ _) = return ()
unifyConstraintNoStk c (TMap k v) = unifyConstraint c k >> unifyConstraint c v
unifyConstraintNoStk c t = raiseConstraintErrorMessage

raiseConstraintErrorMessage :: TypeCheckMonad a
raiseConstraintErrorMessage = do
    constraintStack <- getConstraintUnificationStack
    cts <- safeGetUnificationStack False
    raiseMessageAsError $ constraintUnificationErrorMessage constraintStack cts

-- | Takes a type and converts TDot t1 t2 to [t1, t2].
typeToDotList :: Type -> TypeCheckMonad [Type]
typeToDotList t = compress t >>= \ t ->
    case t of
        TDot t1 t2 -> do
            (t:ts1) <- typeToDotList t1
            ts2 <- typeToDotList t2
            return (t:ts1++ts2)
        _   -> return [t]

dotableToDotList :: Type -> TypeCheckMonad ([Type], Type)
dotableToDotList t = compress t >>= return . reduceDotable

-- | Takes a 'TDotable' and returns a tuple consisting of:
-- the arguments that it takes and the ultimate return type. Note
-- that due to the way that TDotables are introduced the return type
-- is guaranteed to be simple.
-- This requires that its argument is compressed.
reduceDotable :: Type -> ([Type], Type)
reduceDotable (TDotable argt rt) = 
    let (args, urt) = reduceDotable rt in (argt:args, urt)
reduceDotable x = ([], x)

-- | We convert all TDotable (TDot t1 t2) to 
-- TDotable t1 (TDotable t2...). Thus every argument of a TDotable
-- is not a TDot.
toNormalForm :: Type -> Type
toNormalForm (TDotable (TDot t1 t2) rt) = 
    toNormalForm (TDotable t1 (TDotable t2 rt))
-- Hence, t1 is atomic (of some sort)
toNormalForm (TDotable t1 (TDotable t2 rt)) = 
    TDotable t1 (toNormalForm (TDotable t2 rt))
toNormalForm x = x

isVar :: Type -> Bool
isVar (TVar _) = True
isVar _ = False

isDotable :: Type -> Bool
isDotable (TDotable _ _) = True
isDotable _ = False

isExtendable :: Type -> Bool
isExtendable (TExtendable _ _) = True
isExtendable _ = False

isSimple :: Type -> Bool
isSimple a = not (isDotable a) && not (isVar a)

-- Assumption, argument of TDotable is always simple
-- and that result of TDotable is either another TDotable or
-- simple.

-- Also, we assume that if a type list is of the form 
-- [TDotable argt rt, arg] then arg must contribute to argt (though 
-- obviously argt could itself by a TDotable). In reality, this means
-- that we prohibit definitions such as:
--   datatype A = B.{0..1}
-- f(x) = B.true
-- Intuitively this can be thought of as prohibiting dots usage
-- as a functional programming construct.

-- | Takes two type lists and unifies them into one type list.
combineTypeLists :: [Type] -> [Type] -> TypeCheckMonad [Type]
combineTypeLists [] [] = return []

-- If either of the front components is a TDot, compute the dot list.
combineTypeLists ((TDot a1 a2):as) bs = do
    ts <- typeToDotList (TDot a1 a2)
    combineTypeLists (ts++as) bs
combineTypeLists as ((TDot b1 b2):bs) = do
    ts <- typeToDotList (TDot b1 b2)
    combineTypeLists as (ts++bs)

-- If one type list has just one component left then this must be equal
-- to the dotted type of the other.
combineTypeLists (a:as) [b] = do
    -- IMPORTANT: The expected type is a:as
    t <- unify (foldr1 TDot (a:as)) b
    return [t]
combineTypeLists [a] (b:bs) = do
    -- IMPORTANT The expected type is a
    t <- unify a (foldr1 TDot (b:bs))
    return [t]

-- Hence, as /= [], bs /= []

-- Otherwise, if both arguments are simple, and not equal to TDot,
-- then we can just unify them.
-- Note, if the first item in the list is a var, and b is not dotable 
-- then, providing there is another item after the var, (by the shortest
-- match rule) we unify the var and b.
combineTypeLists (a:as) (b:bs) | not (isDotable a) && not (isDotable b) = do
    t <- unify a b
    ts <- combineTypeLists as bs
    return (t:ts)

-- ASSUMPTION: argt is not a TDot

-- Otherwise, if the head of one of the lists is dotable then we proceed
-- as follows
combineTypeLists ((a0@(TDotable argt rt)):a:as) (b:bs)
    | isDotable argt = do
        -- No choice but to unify a with argt
        unify argt a
        combineTypeLists (rt:as) (b:bs)
    | (isSimple a || (isVar a && as /= [])) = do
        -- By assumption a is not a TDot and so either, a is simple and
        -- hence we unify argt and a or, it is a var. Then, providing 
        -- as /= [] we can use the shortest match rule to justify 
        -- matching with just with one component.
        unify argt a
        combineTypeLists (rt:as) (b:bs)
    | isVar a = do
        -- as == [] otherwise the above case applies. Hence, we need to 
        -- set a to be equal to the args necessary to remove all 
        -- TDotables from the front, plus any extension needed in order
        -- to make it match (b:bs). Firstly, compute what the ultimate
        -- return type (urt) and args to get this will be.
        let (args, urt) = reduceDotable (TDotable argt rt)
        -- As urt is simple it immediately follows that the length of
        -- the urt type list is 1. Therefore, even if bs has a var on 
        -- the end we still want the b:bs type list to be as short as
        -- possible (and thus don't need to extend it). Hence, we can
        -- use the evalTypeListList function.
        t:ts <- evalTypeList LongestMatch (b:bs)
        -- The first type in this list must be equal to urt
        t1 <- unify urt t
        -- We want to set the var a to all the args, plus any extension
        -- from bs
        combineTypeLists (args++ts) [a]
        return (t1:ts)
    -- Else, a is not simple, or a var. Hence is a TDotable. 
    | isDotable a = do
        -- Compute the ultimate return type of A, and the args to get 
        -- to it.
        let (argsA, rtA) = reduceDotable a
        -- We know rtA has to be the same type as argt
        unify argt rtA
        -- Hence, if we can find all the arguments required to produce
        -- rtA then, we can produce rt. Thus we reduce as follows.
        combineTypeLists (foldr TDotable rt argsA : as) (b:bs)

-- Symmetric case of above
combineTypeLists (a:as) ((TDotable argt rt):b:bs)
    | isDotable argt = do
        unify argt b
        combineTypeLists (a:as) (rt:bs)
    | (isSimple b || (isVar b && bs /= [])) = do
        unify b argt
        combineTypeLists (a:as) (rt:bs)
    | isVar b = do
        let (args, urt) = reduceDotable (TDotable argt rt)
        t:ts <- evalTypeList LongestMatch (a:as)
        t1 <- unify t urt
        combineTypeLists [b] (args++ts)
        return (t1:ts)
    | isDotable b = do
        let (argsB, rtB) = reduceDotable b
        unify rtB argt
        combineTypeLists (a:as) (foldr TDotable rt argsB : bs)

-- TODO: explain why we can't do the unification (it may be because of
-- a type error, but may well be because of an unsupported type list).
combineTypeLists as bs = raiseUnificationError True




-- | The main type unification algorithm. This adds values to the unification 
-- stack in order to ensure error messages are helpful.
unify :: Type -> Type -> TypeCheckMonad Type
unify texp tact = do
    texp <- compress texp
    tact <- compress tact
    addUnificationPair (texp, tact) (unifyNoStk texp tact)

-- | Unifies the types but doesn't add a pair to the stack.
unifyNoStk :: Type -> Type -> TypeCheckMonad Type
unifyNoStk (TVar t1) (TVar t2) | t1 == t2 = 
    return (TVar t1)
unifyNoStk (TVar t1) (TVar t2) = do
    res1 <- readTypeRef t1
    res2 <- readTypeRef t2
    case (res1, res2) of
        (Left (tv1, cs1), Left (tv2, cs2)) -> do
            let cs = nub $ sort $ cs1++cs2
            if isRigid t1 && isRigid t2 then do
                when (t1 /= t2) $ raiseUnificationError False
                return $ TVar t1
            else if isRigid t1 then do
                mapM_ (flip unifyConstraint (TVar t1)) cs
                applySubstitution t2 (TVar t1)
            else if isRigid t2 then do
                mapM_ (flip unifyConstraint (TVar t2)) cs
                applySubstitution t1 (TVar t2)
            else do
                fv <- freshRegisteredTypeVarWithConstraints cs
                applySubstitution t1 fv
                applySubstitution t2 fv
                return fv
        (Left _, Right t) -> unify (TVar t1) t
        (Right t, Left _) -> unify t (TVar t2)
        (Right t1, Right t2) -> unify t1 t2
unifyNoStk (TVar a) b = do
    res <- readTypeRef a
    case res of
        Left (tva, cs)  -> do
            mapM_ (\ c -> unifyConstraint c b) cs
            applySubstitution a b
        Right t         -> unify t b
unifyNoStk a (TVar b) = do
    res <- readTypeRef b
    case res of
        Left (tvb, cs)  -> do
            mapM_ (\ c -> unifyConstraint c a) cs
            applySubstitution b a
        Right t         -> unify a t


-- Type Atoms
unifyNoStk TInt TInt = return TInt
unifyNoStk TBool TBool = return TBool
unifyNoStk TProc TProc = return TProc
unifyNoStk TEvent TEvent = return TEvent
unifyNoStk TChar TChar = return TChar
unifyNoStk (TDatatype n1) (TDatatype n2) 
    | n1 == n2 = return $ TDatatype n1

-- Simple cases
unifyNoStk (TFunction ts1 rt1) (TFunction ts2 rt2) | length ts1 == length ts2 = do
    ts <- zipWithM unify ts1 ts2
    rt <- unify rt1 rt2
    return $ TFunction ts rt
unifyNoStk (TSeq t1) (TSeq t2) = do
    t <- unify t1 t2
    return $ TSeq t
unifyNoStk (TSet t1) (TSet t2) = do
    t <- unify t1 t2
    return $ TSet t
unifyNoStk (TMap k1 v1) (TMap k2 v2) = do
    k <- unify k1 k2
    v <- unify v1 v2
    return $ TMap k v
unifyNoStk (TTuple ts1) (TTuple ts2) | length ts1 == length ts2 = do
    ts <- zipWithM unify ts1 ts2
    return $ TTuple ts

unifyNoStk (a@(TDotable _ _)) (b@(TDotable _ _)) = do
    a <- compress a
    b <- compress b
    let
        -- Compute the ultimate return types and the arguments required
        -- to get to this return type
        (argsA, rtA) = (reduceDotable . toNormalForm) a
        (argsB, rtB) = (reduceDotable . toNormalForm) b
    case (rtA, rtB) of
        (TExtendable _ _, TExtendable _ _) -> do
            rt <- unify rtA rtB
            -- In this case we have two things of the form X=>Eventable,
            -- X'=> Eventable. WLOG suppose X'=X^Y. Then, we unify to X.Y=>Eventable.
            as <- evalTypeList LongestMatch argsA
            bs <- evalTypeList LongestMatch argsB
            let as' = map (snd . reduceDotable . toNormalForm) as
                bs' = map (snd . reduceDotable . toNormalForm) bs
            ts <- zipWithM unify as' bs'
            let ts' = drop (length ts) (if length as == length ts then bs else as)
            return $ TDotable (foldl1 TDot (ts++ts')) rtA
        (TExtendable _ _, _) -> do
            -- Firstly, evaluate each type list to reduce it; this means
            -- that it will not have any terms like TDotable TInt ..., TInt..
            as <- evalTypeList LongestMatch argsA
            bs <- evalTypeList LongestMatch argsB
            -- As the left argument is eventable we compute what arguments
            -- would be required to make it into a TExtendable (by computing
            -- the ultimate return types of each element).
            let as' = map (snd . reduceDotable . toNormalForm) as
            -- These must be equal to the argument types that are required to
            -- reach rtB, hence we unify.
            ts <- zipWithM unify as' bs
            let remainingArgs = drop (length as') bs
            unify rtA (foldr TDotable rtB remainingArgs)
            -- The most general type will have the arguments of bs, rather
            -- than the arguments of as (bs provide more information).
            let ts' = drop (length ts) (if length as' == length ts then bs else as')
                args = ts ++ ts'
            return $ TDotable (foldl1 TDot args) rtB
        (_, TExtendable _ _) -> do
            rt <- unify rtA rtB
            as <- evalTypeList LongestMatch argsA
            bs <- evalTypeList LongestMatch argsB
            let bs' = map (snd . reduceDotable . toNormalForm) bs
            ts <- zipWithM unify as bs'
            let remainingArgs = drop (length bs') as
            unify (foldr TDotable rtA remainingArgs) rtB
            -- The most general type will have the arguments of bs, rather
            -- than the arguments of as (bs provide more information).
            let ts' = drop (length ts) (if length bs' == length ts then as else bs')
                args = ts ++ ts'
            return $ TDotable (foldl1 TDot args) rtA
        (_, _) -> do
            -- The return type of the combined dotable must be the unified version
            -- of the return types
            rt <- unify rtA rtB
            -- If neither is a TExtendable then the args must be the same.
            -- Hence, unify the two argument lists.
            args <- combineTypeLists argsA argsB
            return $ TDotable (foldl1 TDot args) rt

unifyNoStk (TDot t1 t2) (TDot t1' t2') = do
    a0 <- typeToDotList (TDot t1 t2)
    b0 <- typeToDotList (TDot t1' t2')
    let a = map toNormalForm a0
    let b = map toNormalForm b0
    ts <- combineTypeLists a b
    return $ foldl1 TDot ts

-- TDot + TEvent/TExtendable/TDatatype/TDotable

unifyNoStk (TDot t1 t2) (TDatatype n) = do
    b <- symmetricUnificationAllowed
    if not b then raiseUnificationError False else return ()
    unify (TDotable t2 (TDatatype n)) t1
    return $ TDatatype n

unifyNoStk (TDatatype n) (TDot t1 t2) = do
    unify (TDotable t2 (TDatatype n)) t1
    return $ TDatatype n

unifyNoStk (TDot t1 t2) TEvent = do
    b <- symmetricUnificationAllowed
    if not b then raiseUnificationError False else return ()
    unify (TDotable t2 TEvent) t1
    return TEvent

unifyNoStk TEvent (TDot t1 t2) = do
    unify (TDotable t2 TEvent) t1
    return TEvent

unifyNoStk (TDot t1 t2) (TExtendable t pt) = do
    b <- symmetricUnificationAllowed
    if not b then raiseUnificationError False else return ()
    tl <- unify (TDotable t2 (TExtendable t pt)) t1
    return $ TDot tl t2

unifyNoStk (TExtendable t pt) (TDot t1 t2) = do
    -- We actually want the 'shortest match rule' here, so we inline a version
    -- of evalTypeList that does so.
    tl <- typeToDotList (TDot t1 t2)
    tl <- mapM (\x -> compress x >>= return . toNormalForm) tl
    -- We actually want the shortest match here since the expected type is
    -- extendable, and thus the user is most likely to want just to consume the
    -- first item.
    tl <- evalTypeList ShortestMatch tl
    -- Now, unify normally. If we still have a dot list then we fall back to
    -- the usual way (which will require the longest match rule).
    case foldr1 TDot tl of
        TDot t1 t2 -> do
            t1' <- unify (TDotable t2 (TExtendable t pt)) t1
            return $ TDot t1' t2
        tl -> unify (TExtendable t pt) tl

unifyNoStk (TDot t1 t2) (TDotable argt rt) = do
    b <- symmetricUnificationAllowed
    if not b then raiseUnificationError False else return ()
    unify t1 (TDotable t2 (TDotable argt rt))
    return $ TDotable argt rt

unifyNoStk (TDotable argt rt) (TDot t1 t2) = do
    unify (TDotable t2 (TDotable argt rt)) t1
    return $ TDotable argt rt

unifyNoStk (TDotable argt rt) (TExtendable t pt) = do
    -- unify argt=>rt and pt=>*t.
    -- Check rt is of the form pt'=>*t.
    -- Args for pt thus become argt:pt'
    pt' <- freshRegisteredTypeVarRef []
    rt' <- unify (TExtendable t pt') rt
    safeWriteTypeRef pt $! TDotable argt (TVar pt')
    return $! TDotable argt (TExtendable t pt')
unifyNoStk (TExtendable t pt) (TDotable argt rt) = do
    pt' <- freshRegisteredTypeVarRef []
    rt' <- unify (TExtendable t pt') rt
    safeWriteTypeRef pt $! TDotable argt (TVar pt')
    return $! TDotable argt (TExtendable t pt')

unifyNoStk (TExtendable t1 pt1) (TExtendable t2 pt2) | pt1 == pt2 = do
    t <- unify t1 t2
    return $ TExtendable t pt1
unifyNoStk (TExtendable t1 pt1) (TExtendable t2 pt2) = do
    t <- unify t1 t2
    -- They need to use the same variable now
    if isRigid pt2 then safeWriteTypeRef pt1 (TVar pt2)
    else safeWriteTypeRef pt2 (TVar pt1)
    return $ TExtendable t pt1
unifyNoStk (TExtendable t1 pt) t2 = do
    t <- unify t1 t2
    safeWriteTypeRef pt TExtendableEmptyDotList
    return t
unifyNoStk t1 (TExtendable t2 pt) = do
    t <- unify t1 t2
    safeWriteTypeRef pt TExtendableEmptyDotList
    return t

unifyNoStk t1 t2 = raiseUnificationError False

safeWriteTypeRef :: TypeVarRef -> Type -> TypeCheckMonad ()
safeWriteTypeRef (RigidTypeVarRef tv cs n) t = raiseUnificationError False
safeWriteTypeRef tvref t = writeTypeRef tvref t

-- | Raises a unification error. If the passed flag is True then 
-- any dots are not evaluated in the error. This is to avoid infinite loops that
-- can occur, for example, whilst unifiying:
-- [TDotable TInt (TDatatype (Name "A")),TBool]
-- [TDotable TInt (TDatatype (Name "A")),TBool]
raiseUnificationError :: Bool -> TypeCheckMonad a
raiseUnificationError isDotError = do
    cts <- safeGetUnificationStack isDotError
    raiseMessageAsError $ unificationErrorMessage False cts

safeGetUnificationStack :: Bool -> TypeCheckMonad [(Type, Type)]
safeGetUnificationStack isDotError = do
    b <- getInError
    if b then throwException $ UserError else setInError True $ do
    ts <- getUnificationStack
    mapM (\ (t1, t2) -> do
        t1 <- compress t1
        t2 <- compress t2
        -- Try and tidy any dot lists
        (t1, t2) <- tryAndRecover False (do
            t1 <- evaluateDots t1
            t2 <- evaluateDots t2
            return (t1, t2)) (return (t1,t2))
        return (t1, t2)) ts

-- Returns the type that we substitute for
-- NB: in a quantified type we do not apply the substitution to any 
-- quantified variables
applySubstitution :: TypeVarRef -> Type -> TypeCheckMonad Type
applySubstitution tvref typ = do
    t' <- compress typ
    b <- occurs (typeVar tvref) typ
    if b then do
        t <- evaluateDots t'
        if t == TVar tvref then return t else do
        b <- occurs (typeVar tvref) t
        errorIfFalse (not b) (infiniteUnificationMessage (TVar tvref) typ)
        safeWriteTypeRef tvref t
        return t
    else do
        safeWriteTypeRef tvref typ
        return typ

substituteTypes :: [(TypeVar, Type)] -> Type -> TypeCheckMonad Type
substituteTypes sub t = foldM (\ x y -> substituteType y x) t sub

substituteTypeScheme :: [(TypeVar, Type)] -> TypeScheme -> TypeCheckMonad TypeScheme
substituteTypeScheme sub (ForAll xs t) = do
    t' <- substituteTypes sub t
    let subConstraint (tv, cs) =
            case lookup tv sub of
                Nothing -> (tv, cs)
                Just (TVar tv') -> (typeVar tv', cs)
        xs' = map subConstraint xs
    return $! ForAll xs' t'

-- | Applies a subtitution directly to the type. This is used in
-- type instantiation where we create a fresh type for each universal 
-- variable
substituteType :: (TypeVar, Type) -> Type -> TypeCheckMonad Type
substituteType (tv, t) (b @ (TVar tvref)) = do
    res <- readTypeRef tvref
    case res of
        Left (tv', _) -> if tv == tv' then return t else return b
        Right t' -> substituteType (tv, t) t'
substituteType sub (TFunction targs tr) = do
    targs' <- mapM (substituteType sub) targs
    tr' <- substituteType sub tr
    return $ TFunction targs' tr'
substituteType sub (TSeq t) = substituteType sub t >>= return . TSeq
substituteType sub (TDot t1 t2) = do 
    t1' <- substituteType sub t1
    t2' <- substituteType sub t2
    return $ TDot t1' t2'
substituteType sub (TSet t) = substituteType sub t >>= return . TSet
substituteType sub (TMap t1 t2) =
    return TMap $$ substituteType sub t1 $$ substituteType sub t2
substituteType sub (TDotable t1 t2) = do
    t1' <- substituteType sub t1
    t2' <- substituteType sub t2
    return $ TDotable t1' t2'
substituteType sub (TTuple ts) = 
    mapM (substituteType sub) ts >>= return . TTuple
substituteType sub (TDatatype n) = return $ TDatatype n
substituteType sub TInt = return TInt
substituteType sub TBool = return TBool
substituteType sub TProc = return TProc
substituteType sub TEvent = return TEvent
substituteType sub TChar = return TChar
substituteType sub TExtendableEmptyDotList = return TExtendableEmptyDotList
substituteType (sub@(tv, TVar spt)) (TExtendable t tvref) = do
    t' <- substituteType sub t
    res <- readTypeRef tvref
    case res of
        Left (tv', _) -> do
            let pt' = if tv == tv' then spt else tvref
            return $ TExtendable t' pt'
        Right t' -> do
            substituteType sub t'
            return $ TExtendable t' tvref

-- | Takes a type and attempts to simplify all TDots inside
-- by combining TDotable t1 t2 and arguments.
evaluateDots :: Type -> TypeCheckMonad Type
evaluateDots (TVar t) = do
    res <- readTypeRef t
    case res of
        Left (tv, cs) -> return $ TVar t
        Right t       -> evaluateDots t
evaluateDots (TSet t) = evaluateDots t >>= return . TSet
evaluateDots (TMap t1 t2) = return TMap $$ evaluateDots t1 $$ evaluateDots t2
evaluateDots (TSeq t) = evaluateDots t >>= return . TSeq
evaluateDots (TTuple ts) = mapM evaluateDots ts >>= return . TTuple
evaluateDots (TExtendable t pt) = compress (TExtendable t pt) >>= \ t -> do
    case t of
        TExtendable t pt -> return $! TExtendable t pt
        t -> evaluateDots t
evaluateDots (TFunction t1 t2) = do
    t1' <- mapM evaluateDots t1
    t2' <- evaluateDots t2
    return $ TFunction t1' t2'
evaluateDots t = do
    ts <- typeToDotList t
    ts <- mapM (\t -> compress t >>= return . toNormalForm) ts
    ts <- evalTypeList LongestMatch ts
    return $ foldr1 TDot ts

data TypeListMode = ShortestMatch | LongestMatch deriving Eq

evalTypeList m ts = mapM compress ts >>= \ ts -> evalTypeList' m ts

-- Assumption, argument of TDotable is always simple
-- and that result of TDotable is either another TDotable or
-- simple.
evalTypeList' :: TypeListMode -> [Type] -> TypeCheckMonad [Type]
evalTypeList' _ (t:[]) = return [t]
evalTypeList' m ((TDot t1 t2):ts) = evalTypeList m (t1:t2:ts)
evalTypeList' m (TDotable argt rt : arg : args)
    | isVar arg && args == [] && m == LongestMatch = do
        let (args, urt) = reduceDotable (TDotable argt rt)
        -- Implement longest match rule
        unify arg (foldr1 TDot args)
        return [urt]
    | isVar arg && args == [] && m == ShortestMatch = do
        let (arg':args, urt) = reduceDotable (TDotable argt rt)
        -- Implement SHORTEST match rule
        unify arg' arg
        return [foldr TDotable urt args]
    | isExtendable arg = do
        let TExtendable rtA ptref = arg
        t <- unify argt rtA
        evalTypeList m (TExtendable rt ptref : args)
    | not (isDotable arg) = do
        -- Implement shortest match rule (if isVar ag)
        t <- unify argt arg
        evalTypeList m (rt:args)
    | isDotable argt && isDotable arg = do
        unify argt arg
        evalTypeList m (rt:args)
    | isDotable arg = do
        let (argsA, rtA) = reduceDotable arg
        t <- unify argt rtA
        evalTypeList m (foldr TDotable rt argsA : args)
evalTypeList' LongestMatch (TExtendable urt pt : args) = do
    args <- evalTypeList LongestMatch args
    safeWriteTypeRef pt $! foldr TDotable TExtendableEmptyDotList args
    return [urt]
evalTypeList' ShortestMatch (TExtendable urt pt : arg : args) = do
    res <- readTypeRef pt
    safeWriteTypeRef pt $! TDotable arg TExtendableEmptyDotList
    evalTypeList ShortestMatch (urt : args)
-- If the first argument isn't a dotable we ignore it.
evalTypeList' m (t:ts) = do
    ts <- evalTypeList m ts
    return $ t:ts
