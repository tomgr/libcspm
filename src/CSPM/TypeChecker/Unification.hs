{-# LANGUAGE DoAndIfThenElse #-}
module CSPM.TypeChecker.Unification (
	generaliseGroup, instantiate, unify, unifyAll, evaluateDots,
	typeToDotList,
) where

import Control.Monad
import List (nub, (\\), intersect, group, sort)
import Prelude

import CSPM.DataStructures.Names
import CSPM.DataStructures.Types
import CSPM.TypeChecker.Environment
import CSPM.TypeChecker.Exceptions
import CSPM.TypeChecker.Monad
import Util.Monad

-- | Return the free type variables (and their constraints) for all 'TypeVar's 
-- that occur in 'Type'.
freeTypeVars :: Type -> TypeCheckMonad [(TypeVar, [Constraint])]
freeTypeVars = liftM nub . freeTypeVars'	
freeTypeVars' :: Type -> TypeCheckMonad [(TypeVar, [Constraint])]
freeTypeVars' (TVar tv) = do
	typ <- readTypeRef tv
	case typ of
		Left (tv, cs)	-> return [(tv, cs)]
		Right t			-> freeTypeVars' t
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
freeTypeVars' TPrebound = return []

-- | Generalise the types of the declarations. The parameter 'names' gives the 
-- names that were bound by all the declarations that we are interested in. This
-- is done because we convert a type T into forall vs T where 
-- 		vs = fvts (T) - fvts(Env)
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
			[t | (n, (ForAll _ t)) <- flatten env, not (n `elem` names)]
	mapM (\ nts -> mapM (\ (n,t) -> do
		-- The free vars in this type
		deffvs <- freeTypeVars t
		-- All the free variables that were actually bound by this declaration 
		-- (rather than some other declaration in the environment).
		let unboundVars = 
			filter (\ (fv, cs) -> not (fv `elem` map fst envfvs)) deffvs
		let ts = ForAll unboundVars t
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
		Right t		-> occurs a t
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
		Left (tva, cs)	-> 
			if c `elem` cs then return True else do
				fv <- freshTypeVarWithConstraints (nub (c:cs))
				applySubstitution v fv
				return True
		Right t			-> unifyConstraint c t
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

-- | The main type unification algorithm. This adds values to the unification 
-- stack in order to ensure error messages are helpful.
unify :: Type -> Type -> TypeCheckMonad Type
unify texp tact = addUnificationPair (texp, tact) (unifyNoStk texp tact)

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
		(Left _, Right t)		-> unify (TVar t1) t
		(Right t, Left _)		-> unify t (TVar t2)
		(Right t1, Right t2)	-> unify t1 t2
unifyNoStk (TVar a) b = do
	res <- readTypeRef a
	case res of
		Left (tva, cs)	-> do
			res <- liftM and (mapM (\ c -> unifyConstraint c b) cs)
			if res then applySubstitution a b
			else raiseUnificationError False
		Right t			-> unify t b
unifyNoStk a (TVar b) = do
	res <- readTypeRef b
	case res of
		Left (tvb, cs)	-> do
			res <- liftM and (mapM (\ c -> unifyConstraint c a) cs)
			if res then applySubstitution b a
			else raiseUnificationError False
		Right t			-> unify a t


-- Type Atoms
unifyNoStk TInt TInt = return TInt
unifyNoStk TBool TBool = return TBool
unifyNoStk TProc TProc = return TProc
unifyNoStk TEvent TEvent = return TEvent
unifyNoStk TEventable TEventable = return TEventable
unifyNoStk TEvent TEventable = return TEventable
unifyNoStk TEventable TEvent = return TEventable
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
unifyNoStk (TDotable t1 t2) (TDotable t1' t2') = do
	a <- compress t2
	b <- compress t2'
	case (a,b) of
		(TDotable argt rt, _) ->
			unify (TDotable (TDot t1 argt) rt) (TDotable t1' t2')
		(_, TDotable argt rt) ->
			unify (TDotable t1 t2) (TDotable (TDot t1' argt) rt)
		(TEventable, _) 	-> do
			c <- evalTypeToDotList t1
			d <- evalTypeToDotList t1'
			-- NB. zipWithM ignores the possibility one option
			-- is longer than the other which is what we need
			argt <- zipWithM unify c d
			rt <- unify a b
			return $ TDotable (foldr1 TDot argt) rt
		(_, TEventable) 	-> do
			c <- evalTypeToDotList t1
			d <- evalTypeToDotList t1'
			argt <- zipWithM unify c d
			rt <- unify a b
			return $ TDotable (foldr1 TDot argt) rt
		_					-> do
			a <- unify t1 t1'
			b <- unify t2 t2'
			return $ TDotable a b
	where
		evalTypeToDotList :: Type -> TypeCheckMonad [Type]
		evalTypeToDotList t = compress t >>= \ t ->
				case t of
					TDot t1 t2 -> do
						(t:ts1) <- evalTypeToDotList t1
						ts2 <- evalTypeToDotList t2
						evalTypeList t (ts1++ts2)
					_	-> return [t]

unifyNoStk (TDot t1 t2) (TDot t1' t2') = do
	-- Assumption, argument of TDotable is always simple
	-- and that result of TDotable is either another TDotable or
	-- simple.
	
	-- Also, we assume that if a type list is of the form 
	-- [TDotable argt rt, arg] then arg must contribute to argt (though 
	-- obviously argt could itself by a TDotable). In reality, this means
	-- that we prohibit definitions such as:
	--   datatype A = B.{0..1}
	-- f(x) = B.x
	-- Intuitively this can be thought of as prohibiting dots usage
	-- as a functional programming construct.
	a0 <- typeToDotList (TDot t1 t2)
	b0 <- typeToDotList (TDot t1' t2')
	let a = map toNormalForm a0
	let b = map toNormalForm b0
	ts <- combine a b
	return $ foldl1 TDot ts
	where
		isVar :: Type -> Bool
		isVar (TVar _) = True
		isVar _ = False

		isDotable :: Type -> Bool
		isDotable (TDotable _ _) = True
		isDotable _ = False

		isSimple a = not (isDotable a) && not (isVar a)
		
		-- | We convert all TDotable (TDot t1 t2) to 
		-- TDotable t1 (TDotable t2...). Thus every argument of a TDotable
		-- is not a TDot.
		toNormalForm :: Type -> Type
		toNormalForm (TDotable (TDot t1 t2) rt) = 
			TDotable t1 (toNormalForm (TDotable t2 rt))
		toNormalForm x = x

		-- | Takes a 'TDotable' and returns a tuple consisting of:
		-- the arguments that it takes and the ultimate return type. Note
		-- that due to the way that TDotables are introduced the return type
		-- is guaranteed to be simple.
		reduceDotable :: Type -> ([Type], Type)
		reduceDotable (TDotable argt rt) = 
			let (args, urt) = reduceDotable rt in (argt:args, urt)
		reduceDotable x = ([], x)
		
		-- | Takes two type lists and unifies them into one type list.
		combine :: [Type] -> [Type] -> TypeCheckMonad [Type]
		combine [] [] = return []
		
		-- If either of the front components is a TDot, compute the dot list.
		combine ((TDot a1 a2):as) bs = do
			ts <- typeToDotList (TDot a1 a2)
			combine (ts++as) bs
		combine as ((TDot b1 b2):bs) = do
			ts <- typeToDotList (TDot b1 b2)
			combine as (ts++bs)

		-- If one type list has just one component left then this must be equal
		-- to the dotted type of the other.
		combine (a:as) [b] = do
			t <- unify (foldr1 TDot (a:as)) b
			return [t]
		combine [a] (b:bs) = do
			t <- unify a (foldr1 TDot (b:bs))
			return [t]

		-- Hence, as /= [], bs /= []

		-- Otherwise, if both arguments are simple, and not equal to TDot,
		-- then we can just unify them.
		-- Note, if the first item in the list is a var, and b is not dotable 
		-- then, providing there is another item after the var, (by the shortest
		-- match rule) we unify the var and b.
		combine (a:as) (b:bs) | not (isDotable a) && not (isDotable b) = do
			t <- unify a b
			ts <- combine as bs
			return (t:ts)
		
		-- ASSUMPTION: argt is not a TDot, or a TDotable. 
		
		-- Otherwise, if the head of one of the lists is dotable then we proceed
		-- as follows
		combine ((a0@(TDotable argt rt)):a:as) (b:bs)
			| (isSimple a || (isVar a && as /= [])) = do
				-- By assumption a is not a TDot and so either, a is simple and
				-- hence we unify argt and a or, it is a var. Then, providing 
				-- as /= [] we can use the shortest match rule to justify 
				-- matching with just with one component.
				unify argt a
				combine (rt:as) (b:bs)
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
				-- use the evaluateDots function.
				t:ts <- evaluateDots (foldl1 TDot (b:bs)) >>= typeToDotList
				-- The first type in this list must be equal to urt
				t1 <- unify urt t
				-- We want to set the var a to all the args, plus any extension
				-- from bs
				combine (args++ts) [a]
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
				combine (foldr TDotable rt argsA : as) (b:bs)

		-- Symmetric case of above
		combine (a:as) ((TDotable argt rt):b:bs)
			| (isSimple b || (isVar b && bs /= [])) = do
				unify argt b
				combine (rt:bs) (a:as)
			| isVar b = do
				let (args, urt) = reduceDotable (TDotable argt rt)
				t:ts <- evaluateDots (foldl1 TDot (a:as)) >>= typeToDotList
				t1 <- unify urt t
				combine (args++ts) [b]
				return (t1:ts)
			| isDotable b = do
				let (argsB, rtB) = reduceDotable b
				unify argt rtB
				combine (foldr TDotable rt argsB : bs) (a:as)
		
		-- TODO: explain why we can't do the unification (it may be because of
		-- a type error, but may well be because of an unsupported type list).
		combine as bs = raiseUnificationError True

-- TDot + TEvent/TEventable/TDatatype/TDotable
unifyNoStk (TDot t1 t2) (TDatatype n) = do
	unify (TDotable t2 (TDatatype n)) t1
	return $ TDatatype n
unifyNoStk (TDatatype n) (TDot t1 t2) = do
	unify (TDotable t2 (TDatatype n)) t1
	return $ TDatatype n

unifyNoStk (TDot t1 t2) TEvent = do
	unify (TDotable t2 TEvent) t1
	return TEvent
unifyNoStk TEvent (TDot t1 t2) = do
	unify (TDotable t2 TEvent) t1
	return TEvent

unifyNoStk (TDot t1 t2) TEventable = do
	unify (TDotable t2 TEventable) t1
	return TEventable
unifyNoStk TEventable (TDot t1 t2) = do
	unify (TDotable t2 TEventable) t1
	return TEventable

unifyNoStk (TDot t1 t2) (TDotable argt rt) = do
	unify (TDotable t2 (TDotable argt rt)) t1
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
	ts <- getUnificationStack
	cts <- mapM (\ (t1, t2) -> do
		t1 <- compress t1
		t2 <- compress t2
		if isDotError then return (t1, t2) else do
		t1 <- evaluateDots t1
		t2 <- evaluateDots t2
		return (t1, t2)) ts
	raiseMessageAsError $ unificationErrorMessage cts
		
-- Returns the type that we substitute for
-- NB: in a quantified type we do not apply the substitution to any 
-- quantified variables
applySubstitution :: TypeVarRef -> Type -> TypeCheckMonad Type
applySubstitution (tvref @ (TypeVarRef tv _ _)) typ = do
	t' <- compress typ
	errorIfFalseM (liftM not (occurs tv typ)) 
		(infiniteUnificationMessage (TVar tvref) t')
	writeTypeRef tvref typ
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
		Right t			-> evaluateDots t
evaluateDots (TSet t) = evaluateDots t >>= return . TSet
evaluateDots (TSeq t) = evaluateDots t >>= return . TSeq
evaluateDots (TTuple ts) = mapM evaluateDots ts >>= return . TTuple
evaluateDots t = do
	t:ts <- typeToDotList t
	ts <- mapM evaluateDots ts
	ts <- evalTypeList t ts
	return $ foldr1 TDot ts

-- | Takes a type and converts TDot t1 t2 to [t1, t2].
typeToDotList :: Type -> TypeCheckMonad [Type]
typeToDotList t = compress t >>= \ t ->
	case t of
		TDot t1 t2 -> do
			(t:ts1) <- typeToDotList t1
			ts2 <- typeToDotList t2
			return (t:ts1++ts2)
		_	-> return [t]

-- | Takes a list of types, the left most type and simplifies the list
-- by combining TDotables with arguments appropriately. It returns the resulting
-- type list.
evalTypeList :: Type -> [Type] -> TypeCheckMonad [Type]
evalTypeList t [] = return [t]
-- This implements the 'Longest Match Rule' (i.e. if C.x is a pattern with C a 
-- datatype then x will match all components, rather than just the first).
evalTypeList (TDotable argt1 (TDotable argt2 rt)) ((var @ (TVar _)):[]) =
	evalTypeList (TDotable (TDot argt1 argt2) rt) (var:[])
evalTypeList (TDotable argt rt) (arg:args) = do
	(arg:args) <- evalTypeList arg args
	unify argt arg
	evalTypeList rt args
evalTypeList t (arg:args) = do
	args' <- evalTypeList arg args
	return (t:args')
