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

-- *************************************************************************
-- Unification + Substitution
-- *************************************************************************
-- Name is a workaround for the problem as follows:
--	we convert a type T into forall vs T where vs = fvts (T) - fvts(Env)
--	where Env does not contain the function whose type we are generalizing
--  (this is because when we type a declaration we are really typing a 
--  lambda function).
generaliseGroup :: [Name] -> [TypeCheckMonad [(Name, Type)]] -> 
					TypeCheckMonad [[(Name, TypeScheme)]]
generaliseGroup names tsm = do
	ts <- sequence tsm
	env <- getEnvironment
	envfvs <- liftM nub 
		(concatMapM freeTypeVars 
			[t | (n, (ForAll _ t)) <- flatten env, not (n `elem` names)])
	mapM (\ nts -> mapM (\ (n,t) -> do
							deffvs <- freeTypeVars t
							let unboundVars = 
								filter (\ (fv, cs) -> 
									not (fv `elem` map fst envfvs)) deffvs
							let t' = ForAll unboundVars t
							setType n t'
							return $ (n, t')) nts) ts

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

unifyAll :: [Type] -> TypeCheckMonad Type
unifyAll [] = freshTypeVar
unifyAll [t] = return t
unifyAll (t1:ts) = do
	t2 <- unifyAll ts
	unify t1 t2

-- Takes a constraint and a type and returns True iff the type satisfies the
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

-- | The main type unification algorithm
unify :: Type -> Type -> TypeCheckMonad Type
unify texp tact = addUnificationPair (texp, tact) (unifyNoStk texp tact)

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
			else do
				t1' <- compress (TVar a)
				t2' <- compress b
				raiseMessageAsError $ unificationErrorMessage [(t1', t2')]
		Right t			-> unify t b
unifyNoStk t (TVar b) = unifyNoStk (TVar b) t

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
	-- Recall that (a.b).c == c.(a.b)
	-- i.e. could be the case that t1 = a.b, t2 = c, t1' = a, t2' = b.c
	-- and unification should succeed
	a <- typeToDotList (TDot t1 t2)
	b <- typeToDotList (TDot t1' t2')
	combine a b
	where
		dotableToDotted (TDotable arg1 (TDotable arg2 rt)) =
			dotableToDotted (TDotable (TDot arg1 arg2) rt)
		dotableToDotted x = x
		
		combine ((TDotable argt1' rt1'):(var1 @ (TVar _)):[]) 
				((TDotable argt2' rt2'):(var2 @ (TVar _)):[]) = do
			let (TDotable argt1 rt1) = dotableToDotted (TDotable argt1' rt1')
			let (TDotable argt2 rt2) = dotableToDotted (TDotable argt2' rt2')
			rt <- unify rt1 rt2
			unify argt1 var1
			unify argt2 var2
			return rt
		combine ((TDotable argt1 rt1):(var @ (TVar _)):[]) 
				((TDotable argt2 rt2):b:bs) = do
			unify b argt2
			rts <- typeToDotList rt2
			combine ((TDotable argt1 rt1):var:[]) (rts++bs)
		combine ((TDotable argt rt):(var @ (TVar _)):[]) (b:bs) = do
			-- Eqn: rt+extension of var = foldr1 TDot (b:bs)
			rts <- typeToDotList rt
			if length rts < length (b:bs) then do
				fv <- freshTypeVar
				unify var (TDot argt fv)
				combine [rt, fv] (b:bs)
			else do
				unify var argt
				unify rt (foldr1 TDot (b:bs))
		combine as ((TDotable argt rt):(var @ (TVar _)):[]) = 
			combine ((TDotable argt rt):var:[]) as
		combine ((TDotable argt rt):arg:as) bs = do
			unify argt arg
			combine (rt:as) bs
		combine as ((TDotable argt rt):arg:args) = 
			combine ((TDotable argt rt):arg:args) as
		combine [a] [b] = unify a b
		combine (a:as) [b] = unify (foldr1 TDot (a:as)) b
		combine [a] (b:bs) = combine (b:bs) [a]
		combine (a:as) (b:bs) = do
			t1 <- unify a b
			t2 <- combine as bs
			return $ TDot t1 t2

-- TDot + TEvent/TEventable/TDatatype/TDotable
unifyNoStk (TDot t1 t2) (TDatatype n) = do
	unify t1 (TDotable t2 (TDatatype n))
	return $ TDatatype n
unifyNoStk (TDatatype n) (TDot t1 t2) = unifyNoStk (TDot t1 t2) (TDatatype n)
unifyNoStk (TDot t1 t2) TEvent = do
	unify t1 (TDotable t2 TEvent)
	return TEvent
unifyNoStk TEvent (TDot t1 t2) = unifyNoStk (TDot t1 t2) TEvent
unifyNoStk (TDot t1 t2) TEventable = do
	unify t1 (TDotable t2 TEventable)
	return TEventable
unifyNoStk TEventable (TDot t1 t2) = unifyNoStk (TDot t1 t2) TEventable
unifyNoStk (TDot t1 t2) (TDotable argt rt) = do
	unify t1 (TDotable t2 (TDotable argt rt))
	return $ TDotable argt rt
unifyNoStk (TDotable argt rt) (TDot t1 t2) = 
	unifyNoStk (TDot t1 t2) (TDotable argt rt)

unifyNoStk (TDotable argt rt) TEventable = do
	unify rt TEventable
	return $ TDotable argt rt
unifyNoStk TEventable (TDotable argt rt) = 
	unifyNoStk (TDotable argt rt) TEventable

unifyNoStk t1 t2 =  do
	ts <- getUnificationStack
	cts <- mapM (\ (t1, t2) -> do
		t1' <- evaluateDots t1 >>= compress
		t2' <- evaluateDots t2 >>= compress
		return (t1', t2')) ts
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


-- Takes a type and attempts to simplify all TDots inside
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
	(t:ts) <- typeToDotList t
	ts' <- mapM evaluateDots ts
	ts'' <- evalTypeList t ts'
	ts''' <- mapM compress ts''
	return $ foldr1 TDot ts''

-- | Takes a type and converts TDot t1 t2 to [t1, t2]
typeToDotList :: Type -> TypeCheckMonad [Type]
typeToDotList t = compress t >>= \ t ->
	case t of
		TDot t1 t2 -> do
			(t:ts1) <- typeToDotList t1
			ts2 <- typeToDotList t2
			return (t:ts1++ts2)
		_	-> return [t]

-- | Takes a list of types, the left most type and simplifies the list
-- by combining TDotables with arguments appropriately.
evalTypeList :: Type -> [Type] -> TypeCheckMonad [Type]
evalTypeList t [] = return [t]
evalTypeList (TDotable argt1 (TDotable argt2 rt)) ((var @ (TVar _)):[]) =
	evalTypeList (TDotable (TDot argt1 argt2) rt) (var:[])
evalTypeList (TDotable argt rt) (arg:args) = do
	(arg:args) <- evalTypeList arg args
	unify argt arg
	evalTypeList rt args
evalTypeList t (arg:args) = do
	args' <- evalTypeList arg args
	return (t:args')
