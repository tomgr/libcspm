{-# LANGUAGE DoAndIfThenElse #-}
module CSPM.TypeChecker.Unification (
    generaliseGroup, instantiate, unify, unifyAll, evaluateDots,
    typeToDotList,
) where

import Control.Monad
import Control.Monad.Trans
import Data.List (nub, (\\), intersect, group, sort)
import Prelude

import CSPM.DataStructures.Names
import CSPM.DataStructures.Types
import CSPM.TypeChecker.Environment
import CSPM.TypeChecker.Exceptions
import CSPM.TypeChecker.Monad
import Util.Exception
import Util.Monad
import Util.PrettyPrint

-- | Return the free type variables (and their constraints) for all 'TypeVar's 
-- that occur in 'Type'.
freeTypeVars :: Type -> TypeCheckMonad [(TypeVar, [Constraint])]
freeTypeVars = liftM nub . freeTypeVars'    
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
freeTypeVars' (TDatatype n1) = return []
freeTypeVars' TInt = return []
freeTypeVars' TBool = return []
freeTypeVars' TEvent = return []
freeTypeVars' TEventable = return []
freeTypeVars' TProc = return []

-- | Generalise the types of the declarations. The parameter 'names' gives the 
-- names that were bound by all the declarations that we are interested in. This
-- is done because we convert a type T into forall vs T where 
--      vs = fvts (T) - fvts(Env)
-- where Env does not contain the function whose type we are generalizing
-- (this is because when we type a declaration we are really typing a 
-- lambda function).
generaliseGroup :: [Name] -> [TypeCheckMonad [(Name, Type)]] -> 
                    TypeCheckMonad [[(Name, TypeScheme)]]
generaliseGroup names tsm = do
    -- Perform the type checking
    ts <- sequence tsm
    env <- getEnvironment
    -- Get all the free variables that are currently in the environment that 
    -- were not bound by any of this group.
    envfvs <- (liftM nub . concatMapM freeTypeVars)
            [t | (n, SymbolInformation { 
                        typeScheme = ForAll _ t 
                }) <- flatten env, not (n `elem` names)]
    mapM (\ nts -> mapM (\ (n,t) -> do
        -- The free vars in this type
        deffvs <- freeTypeVars t
        -- All the free variables that were actually bound by this declaration 
        -- (rather than some other declaration in the environment).
        let 
            unboundVars = 
                filter (\ (fv, cs) -> not (fv `elem` map fst envfvs)) deffvs
            ts = ForAll unboundVars t
        setType n ts
        return (n, ts)) nts) ts

-- | Instantiates the typescheme with some fresh type variables.
instantiate :: TypeScheme -> TypeCheckMonad Type
instantiate (ForAll ts t) = do
    tvs <- mapM (freshTypeVarWithConstraints . snd) ts
    foldM (\ x y -> substituteType y x) t (zip (map fst ts) tvs)
        
-- | Does 'a' occur somewhere in 't'.
occurs :: TypeVar -> Type -> TypeCheckMonad Bool
occurs a (TVar (tvref @ (TypeVarRef tv _ _))) = do
    res <- readTypeRef tvref
    case res of 
        Left (tv,cs)-> return $ a == tv
        Right t     -> occurs a t
occurs a (TSet t) = occurs a t
occurs a (TSeq t) = occurs a t
occurs a (TDot t1 t2) = liftM or (mapM (occurs a) [t1,t2])
occurs a (TTuple ts) = liftM or (mapM (occurs a) ts)
occurs a (TFunction ts t) = liftM or (mapM (occurs a) (t:ts))
occurs a (TDatatype n) = return False
occurs a (TDotable t1 t2) = liftM or (mapM (occurs a) [t1,t2])
occurs a TInt = return False
occurs a TBool = return False
occurs a TProc = return False
occurs a TEvent = return False
occurs a TEventable = return False

-- | Unifys all types to a single type. The first type is  used as the 
-- expected Type in error messages.
unifyAll :: [Type] -> TypeCheckMonad Type
unifyAll [] = freshTypeVar
unifyAll [t] = return t
unifyAll (t1:ts) = do
    t2 <- unifyAll ts
    unify t1 t2

-- | Takes a constraint and a type and returns True iff the type satisfies the
-- constraint, or can be made to satsify the constraint by appropriate type
-- substitutions, in which case the type substitutions are performed.
unifyConstraint :: Constraint -> Type -> TypeCheckMonad Bool
unifyConstraint c (TVar v) = do
    res <- readTypeRef v
    case res of
        Left (tva, cs)  -> 
            if c `elem` cs then return True else do
                fv <- freshTypeVarWithConstraints (nub (c:cs))
                applySubstitution v fv
                return True
        Right t         -> unifyConstraint c t
unifyConstraint c TInt = return True
unifyConstraint Eq TBool = return True -- Bools are not orderable P524
unifyConstraint c (TSeq t) = unifyConstraint c t
unifyConstraint c (TDot t1 t2) = liftM and (mapM (unifyConstraint c) [t1,t2])
unifyConstraint c (TSet t) = return True -- All set elements must support comparison
unifyConstraint c (TTuple ts) = liftM and (mapM (unifyConstraint c) ts)
unifyConstraint Eq TEvent = return True -- Events comparable only
unifyConstraint Eq TEventable = return True -- ditto
unifyConstraint Eq (TDotable a b) = -- channels and datatypes are only dotable things
    liftM and (mapM (unifyConstraint Eq) [a,b])
unifyConstraint Eq (TDatatype n) = return True
    -- User data types are not orderable P524
unifyConstraint c t = 
    raiseMessageAsError $ constraintUnificationErrorMessage c t


-- | Takes a type and converts TDot t1 t2 to [t1, t2].
typeToDotList :: Type -> TypeCheckMonad [Type]
typeToDotList t = compress t >>= \ t ->
    case t of
        TDot t1 t2 -> do
            (t:ts1) <- typeToDotList t1
            ts2 <- typeToDotList t2
            return (t:ts1++ts2)
        _   -> return [t]

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
    -- IMPORTANT: The expected type is b
    t <- unify b (foldr1 TDot (a:as))
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

-- ASSUMPTION: argt is not a TDot, or a TDotable. 

-- Otherwise, if the head of one of the lists is dotable then we proceed
-- as follows
combineTypeLists ((a0@(TDotable argt rt)):a:as) (b:bs)
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
        t:ts <- evalTypeList (b:bs)
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
    | (isSimple b || (isVar b && bs /= [])) = do
        unify argt b
        combineTypeLists (rt:bs) (a:as)
    | isVar b = do
        let (args, urt) = reduceDotable (TDotable argt rt)
        t:ts <- evalTypeList (a:as)
        t1 <- unify urt t
        combineTypeLists (args++ts) [b]
        return (t1:ts)
    | isDotable b = do
        let (argsB, rtB) = reduceDotable b
        unify argt rtB
        combineTypeLists (foldr TDotable rt argsB : bs) (a:as)

-- TODO: explain why we can't do the unification (it may be because of
-- a type error, but may well be because of an unsupported type list).
combineTypeLists as bs = raiseUnificationError True




-- | The main type unification algorithm. This adds values to the unification 
-- stack in order to ensure error messages are helpful.
unify :: Type -> Type -> TypeCheckMonad Type
unify texp tact = do
    addUnificationPair (texp, tact) (unifyNoStk texp tact)

-- | Unifies the types but doesn't add a pair to the stack.
unifyNoStk :: Type -> Type -> TypeCheckMonad Type
unifyNoStk (TVar t1) (TVar t2) | t1 == t2 = 
    return (TVar t1)
unifyNoStk (TVar t1) (TVar t2) = do
    res1 <- readTypeRef t1
    res2 <- readTypeRef t2
    case (res1, res2) of
        (Left (tv1, cs1), Left (tv2,cs2)) -> do
            fv <- freshTypeVarWithConstraints (nub (cs1 ++ cs2))
            applySubstitution t1 fv
            applySubstitution t2 fv
            return fv
        (Left _, Right t)       -> unify (TVar t1) t
        (Right t, Left _)       -> unify t (TVar t2)
        (Right t1, Right t2)    -> unify t1 t2
unifyNoStk (TVar a) b = do
    res <- readTypeRef a
    case res of
        Left (tva, cs)  -> do
            res <- liftM and (mapM (\ c -> unifyConstraint c b) cs)
            if res then applySubstitution a b
            else raiseUnificationError False
        Right t         -> unify t b
unifyNoStk a (TVar b) = do
    res <- readTypeRef b
    case res of
        Left (tvb, cs)  -> do
            res <- liftM and (mapM (\ c -> unifyConstraint c a) cs)
            if res then applySubstitution b a
            else raiseUnificationError False
        Right t         -> unify a t


-- Type Atoms
unifyNoStk TInt TInt = return TInt
unifyNoStk TBool TBool = return TBool
unifyNoStk TProc TProc = return TProc
unifyNoStk TEvent TEvent = return TEvent
unifyNoStk TEventable TEventable = return TEventable
unifyNoStk TEvent TEventable = return TEvent
unifyNoStk TEventable TEvent = return TEvent
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
    -- The return type of the combined dotable must be the unified version
    -- of the return types
    rt <- unify rtA rtB

    case (rtA, rtB) of
        (TEventable, TEventable) -> panic "TC: double eventable"
        (TEventable, _) -> do
            -- Firstly, evaluate each type list to reduce it; this means
            -- that it will not have any terms like TDotable TInt ..., TInt..
            as <- evalTypeList argsA
            bs <- evalTypeList argsB
            -- As the left argument is eventable we compute what arguments
            -- would be required to make it into a TEventable (by computing
            -- the ultimate return types of each element).
            let as' = map (snd . reduceDotable . toNormalForm) as
            -- These must be equal to the argument types that are required to
            -- reach rtB, hence we unify.
            zipWithM unify as' bs
            -- The most general type will have the arguments of bs, rather
            -- than the arguments of as (bs provide more information).
            return $ TDotable (foldl1 TDot bs) rt
        (_, TEventable) -> do
            as <- evalTypeList argsA
            bs <- evalTypeList argsB
            let bs' = map (snd . reduceDotable . toNormalForm) bs
            zipWithM unify as bs'
            return $ TDotable (foldl1 TDot as) rt
        (_, _) -> do
            -- If neither is a TEventable then the args must be the same.
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

-- TDot + TEvent/TEventable/TDatatype/TDotable

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

unifyNoStk (TDot t1 t2) TEventable = do
    b <- symmetricUnificationAllowed
    if not b then raiseUnificationError False else return ()
    tl <- unify (TDotable t2 TEventable) t1
    return $ TDot tl t2

unifyNoStk TEventable (TDot t1 t2) = do
    tl <- unify (TDotable t2 TEventable) t1
    return $ TDot tl t2

unifyNoStk (TDot t1 t2) (TDotable argt rt) = do
    b <- symmetricUnificationAllowed
    if not b then raiseUnificationError False else return ()
    unify t1 (TDotable t2 (TDotable argt rt))
    return $ TDotable argt rt

unifyNoStk (TDotable argt rt) (TDot t1 t2) = do
    unify (TDotable t2 (TDotable argt rt)) t1
    return $ TDotable argt rt

unifyNoStk (TDotable argt rt) TEventable = do
    unify TEventable rt
    return $ TDotable argt rt
unifyNoStk TEventable (TDotable argt rt) = do
    unify TEventable rt
    return $ TDotable argt rt

unifyNoStk t1 t2 = raiseUnificationError False

-- | Raises a unification error. If the passed flag is True then 
-- any dots are not evaluated in the error. This is to avoid infinite loops that
-- can occur, for example, whilst unifiying:
-- [TDotable TInt (TDatatype (Name "A")),TBool]
-- [TDotable TInt (TDatatype (Name "A")),TBool]
raiseUnificationError :: Bool -> TypeCheckMonad a
raiseUnificationError isDotError = do
    b <- getInError
    if b then throwException $ UserError else setInError True $ do
    ts <- getUnificationStack
    cts <- mapM (\ (t1, t2) -> do
        t1 <- compress t1
        t2 <- compress t2
        -- Try and tidy any dot lists
        (t1, t2) <- tryAndRecover (do
            t1 <- evaluateDots t1
            t2 <- evaluateDots t2
            return (t1, t2)) (return (t1,t2))
        return (t1, t2)) ts
    raiseMessageAsError $ unificationErrorMessage cts
        
-- Returns the type that we substitute for
-- NB: in a quantified type we do not apply the substitution to any 
-- quantified variables
applySubstitution :: TypeVarRef -> Type -> TypeCheckMonad Type
applySubstitution (tvref @ (TypeVarRef tv _ _)) typ = do
    t' <- compress typ
    b <- occurs tv typ
    (b, t) <- if b then do
            t <- evaluateDots t'
            b <- occurs tv t
            return (b,t)
        else return (b, typ)
    errorIfFalse (not b)
        (infiniteUnificationMessage (TVar tvref) t')
    writeTypeRef tvref t
    return typ

-- | Applies a subtitution directly to the type. This is used in
-- type instantiation where we create a fresh type for each universal 
-- variable
substituteType :: (TypeVar, Type) -> Type -> TypeCheckMonad Type
substituteType (tv, t) (b @ (TVar (a @ (TypeVarRef tv' cs ioref)))) = do
    res <- readTypeRef a
    case res of
        Left tva -> if tv == tv' then return t else return b
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
substituteType sub TEventable = return TEventable

-- | Takes a type and attempts to simplify all TDots inside
-- by combining TDotable t1 t2 and arguments.
evaluateDots :: Type -> TypeCheckMonad Type
evaluateDots (TVar t) = do
    res <- readTypeRef t
    case res of
        Left (tv, cs) -> return $ TVar t
        Right t       -> evaluateDots t
evaluateDots (TSet t) = evaluateDots t >>= return . TSet
evaluateDots (TSeq t) = evaluateDots t >>= return . TSeq
evaluateDots (TTuple ts) = mapM evaluateDots ts >>= return . TTuple
evaluateDots t = do
    ts <- typeToDotList t
    ts <- mapM (\t -> compress t >>= return . toNormalForm) ts
    ts <- evalTypeList ts
    return $ foldr1 TDot ts

-- Assumption, argument of TDotable is always simple
-- and that result of TDotable is either another TDotable or
-- simple.
evalTypeList :: [Type] -> TypeCheckMonad [Type]
evalTypeList (t:[]) = return [t]
evalTypeList ((TDot t1 t2):ts) = evalTypeList (t1:t2:ts)
evalTypeList (TDotable argt rt : arg : args)
    | isVar arg && args == [] = do
        let (args, urt) = reduceDotable (TDotable argt rt)
        -- Implement longest match rule
        unify arg (foldr1 TDot args)
        return [urt]
    | not (isDotable arg) = do
        -- Implement shortest match rule (if isVar ag)
        t <- unify argt arg
        evalTypeList (rt:args)
    | isDotable arg = do
        let (argsA, rtA) = reduceDotable arg
        t <- unify argt rtA
        evalTypeList (foldr TDotable rt argsA : args)
-- If the first argument isn't a dotable we ignore it.
evalTypeList (t:ts) = do
    ts <- evalTypeList ts
    return $ t:ts
