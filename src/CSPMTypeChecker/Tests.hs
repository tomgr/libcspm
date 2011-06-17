{-# LANGUAGE QuasiQuotes #-}
module CSPMTypeChecker.TCTests (testHarness, runtests) where

import Prelude

import CSPMTypeChecker.TCCommon
import CSPMTypeChecker.TCBuiltInFunctions
import CSPMTypeChecker.TCDependencies
import CSPMTypeChecker.TCModule
import CSPMTypeChecker.TCMonad
import CSPMDataStructures
import CSPMParser
import Util

{- Things I don't allow for my typechecker:
	General use of dots
	Pattern matching on channels, e.g.:
		channel p
		channel q
		gen(p) =
		gen(q) =
	Functions that have a union type such as:
		gen(true) = false
		gen(1) = 0
	See cspmGavin
-}

testHarness :: String -> IO ()
testHarness	exp = (runTyger $
	do
		(modules @ [Annotated _ _ (GlobalModule ds)]) <- stringParser exp []
		runTypeChecker (
			do
				injectBuiltInFunctions
				-- We add an extra scope layer here to allow the 
				-- built in functions to be overloaded.
				local [] (
					do
						typeCheck modules
						names <- concatMapM namesBoundByDecl ds
						mapM_ (\n -> 
							do
								t <- getType n
								let (Name s) = n
								debugOutput (s++" :: "
									++show (prettyPrintTypeScheme t))
												) names)
				))
	>>= (\ res -> case res of
			Left err	-> putStrLn (show err)
			Right _		-> return ())


tests = [cspmChannels, cspmFinSeq, cspmListComp,cspmSetEnum,
		cspmComplexFunc, cspmFinSeq', cspmMutualRecursion, cspmSetTest,
		cspmDataTypes, cspmFlatMap, cspmOrdTest, cspmTakeWhile,
		cspmEqTest, cspmFoldl, cspmPartialFunctions, cspmZip,
		cspmFoldr, cspmRecursivePattern,
		cspmEqualityTest, cspmGavin,   cspmRemdups,
		cspmFilter, cspmInfiniteUnification,  cspmScanr,
		cspmTree, cspmBillFunctionalProgramming, cspmListLength]

runtests = mapM_ testHarness tests

-- *************************************************************************
-- Tests
-- *************************************************************************

cspmZip = [$multilineLiteral| 
	zip(<>, _) = <>
	zip(_, <>) = <>
	zip(<x>^xs, <y>^ys) = <(x,y)>^zip(xs, ys)
|]


cspmFlatMap = [$multilineLiteral| 
	flatmap(f,<>) = <>
	flatmap(f,<x>^xs) = f(x)^flatmap(f,xs)
|]

cspmRemdups = [$multilineLiteral|
	remdups(x) =
		let 
			iter(<>,X) = <>
			iter(<x>^xs,X) =
				if member(x,X) then iter(xs,X)
				else <x>^iter(xs,union(X,{x}))
		within
			iter(x, {})
|]

cspmFoldr = [$multilineLiteral|
	foldr(f, e, <>) = e
	foldr(f, e, <x>^xs) = f(x, foldr(f, e, xs))
|]

cspmFoldl = [$multilineLiteral|
	foldl(f, e, <>) = e
	foldl(f, e, <x>^xs) = foldl(f, f(e, x), xs)
|]

cspmScanr = [$multilineLiteral|
	scanr(f, q0, <>) =  <q0>
	scanr(f, q0, <x>^xs) = 
		let
			(qs @@ <q>^_) = scanr(f, q0, xs)
		within
			f(x, q)^qs
|]

cspmTakeWhile = [$multilineLiteral|
	takeWhile(p, <>) = <>
	takeWhile(p, <x>^xs) =
		if p(x) then <x>^takeWhile(p, xs)
		else <>
|]

cspmFinSeq = [$multilineLiteral|
	FinSeq(t, length) = 
		let
			Gen(0) = {<>}
			Gen(n) = {<x>^xs, xs | x <-t, xs <- Gen(n-1)}
		within
			Gen(length)
|]

cspmFinSeq' = [$multilineLiteral|
	FinSeq'(t, length) = 
		let
			Gen(0) = <<>>
			Gen(n) = concat(< <<x>^xs,xs> | xs <- Gen(n-1), x <- t>)
		within
			Gen(length)
|]

cspmChannels = [$multilineLiteral|
	channel test : {0..1}.{{0}}
	
	P(x) = test.x	-- :: Channel [t]
	Q(x,y) = x.y	-- This is hard to type, the type:
					--  forall a, b : (a, b) -> (a.b)
					-- is not restrictive enough. Really we want a type
					-- TDotable b c where TDotable b c means that
					-- x.b :: c . Consider how this interacts with the rest
					-- E.g. TChannel (t1:ts) == TDotable t1 (TChannel ts)
					-- The type of this would then be:
					--  forall b, c : (TDotable b c, b) -> c
					--	P(x) :: Int -> TDotable (TSet Int) (TChannel [])
					--  Q(x,y) :: forall a b : (TDotable a b, a) -> b
					--  R'() :: TDotable (TSet Int) (TChannel [])
					--  R() :: TChannel []
					--	R''() :: TChannel []
	
	R'() = Q(test,0)
	R() = Q(test.0,{0})	-- = test.0.{0}
	R''() = R'().{0}
	
	S() = test.0.{0}
|]


cspmDataTypes = [$multilineLiteral|
	datatype T = 
		A.{0..1}
	
	f(A.0) = 0
	
	g(x) = x.0
	
	r() = g(A)

	datatype T2 = A2.Int | B2.Int.{0..1} | C2.{0..1}
	
	gen(A2.0) = 1
	gen(B2.0.0) = 1
	gen(C2.0) = 1
	
	somePat = union(T2, {A2.0})
|]

cspmComplexFunc = [$multilineLiteral|
	channel p : {0..5}.{0..5}
	channel q : {0..5}

	put(p) = <p.i.j | i<-<0..5>, j<-<0..5>>
--	put(q) = <q.i | i<-<0..5>>
|]

cspmListComp = [$multilineLiteral|
	channel p : {0..1}
	
	P() = <p.i | i <- <0..100>>
|]

cspmFilter = [$multilineLiteral|
	filter(f, <>) = <>
	filter(f, <x>^xs) = 
		if f(x) then <x>^filter(f,xs) else filter(f,xs)
|]

cspmEqualityTest = [$multilineLiteral|
	P(T) = <i | i <- T, j <- T, i != j>
	Q() = P(<1..2>)
|]

cspmOrdTest = [$multilineLiteral|
	P(T) = <i | i <- T, j <- T, i < j>
	Q() = P(<0,1>)
|]

cspmSetTest = [$multilineLiteral|
	P(T) = {i | i <- T}
	
	Q(xs) = {xs}
|]

cspmEqTest = [$multilineLiteral|
	applySeq(f, x) = 
		let
			extract(<x>) = x
		within
			extract(<a | (x', a) <- f, x == x'>)
|]

cspmPartialFunctions = [$multilineLiteral|	
	functionDomain(f) = {x | (x,_) <- f}
	functionDomainSeq(f) = <x | (x,_) <- f>
	functionImage(f) = {x | (_,x) <- f}
	functionImageSeq(f) = <x | (_,x) <- f>
	identityFunction(domain) = {(x,x) | x <- domain}
	identityFunctionSeq(domain) = <(x,x) | x <- domain>
	invert(f) = {(a,b) | (b,a) <- f}
	invertSeq(f) = <(a,b) | (b,a) <- f>

	apply(f, x) = 
		let
			extract({x}) = x
		within
			extract({a | (x', a) <- f, x == x'})

	applySeq(f, x) = 
		let extract(<x>) = x
		within
			extract(<a | (x', a) <- f, x == x'>)

	composeFunctions(fs1, fs2) = {(a, apply(fs1, b)) | (a, b) <- fs2}
	composeFunctionsSeq(fs1, fs2) = <(a,applySeq(fs1,b)) | (a,b) <- fs2>
		

	mapOverSet(f, X) =
		{apply(f, x) | x <- X}
	mapOverSeq(f, <>) = <>
	mapOverSeq(f, <x>^xs) = <applySeq(f,x)>^mapOverSeq(f,xs)

	seqDiff(xs, ys) = <x | x <- xs, not elem(x,ys)>
	seqInter(xs, ys) = <x | x <- xs, elem(x, ys)>
	seqUnion(xs, ys) = remdups(xs^ys)

	remdups(x) =
		let 
			iter(<>,X) = <>
			iter(<x>^xs,X) =
				if member(x,X) then iter(xs,X)
				else <x>^iter(xs,union(X,{x}))
		within
			iter(x, {})
|]

cspmSetEnum = [$multilineLiteral|
	channel p : {0..1}.{0..1}
	
	P(x) = {| p.x |}
	
	Q(chan,x) = chan.x.0
|]

cspmRecursivePattern = [$multilineLiteral|
	x = <0>^x
|]

cspmGavin =[$multilineLiteral|
	-- Haskell bails on this too interestingly
	-- (or rather infers the same type)
	fst((x,y)) = x
	m() = fst(f(true))
	f(x) = (x,m())
|]

cspmMutualRecursion = [$multilineLiteral|
	even(a) = (<a>^odd(a))
	odd(a) = (<a>^even(a))
	
	someOtherFunc(a) = <>
|]

cspmInfiniteUnification = [$multilineLiteral|
	someFunc(a) = someFunc(<a>)
|]

cspmTypeExps = [$multilineLiteral|
	channel funnyChannel : (Int, Bool)

	datatype funnytype = SomeType.(Int, Bool)
|]

cspmBadType = [$multilineLiteral|
	someFunc(x) = x+1
	
	test(y) = 
		let
			f = someFunc(<0>)
		within
			f
|]

cspmTree = [$multilineLiteral|
	datatype IntTree = Leaf.Int | Node.IntTree.IntTree
	
	flatten(Leaf.n) = <n>
	flatten(Node.t1.t2) = flatten(t1)^flatten(t2)
|]


cspmPolymorphicPatterns = [$multilineLiteral|
	id(a) = a
	(somePat1, somePat2) = (id, id)
	
	(someRecursivePattern, b) = 
		(someRecursivePattern^b,someRecursivePattern^b)
|]

cspmCurying = [$multilineLiteral|
	curry(x)(y) = curry(0)(1)
|]

cspmListLength = [$multilineLiteral|
	test=#s 
|]

cspmBillFunctionalProgramming = [$multilineLiteral|
	take(n)(xs) = 
		if n==0 then <> else <head(xs)>^take(n-1)(tail(xs))

	cnth(i,xs) = if i==0 then head(xs) 
						else cnth(i-1,tail(xs))
			
	fst((x,y)) = x
	snd((x,y)) = y

	-- The following function can be useful for partitioning a process list
	-- into roughly equal-sized pieces for structured compression

	groupsof(n)(xs) = let xl=#xs within
					  if xl==0 then <> else
					  if xl<=n or n==0 then <xs>
					  else let 
							 m=if (xl/n)*n==xl then n else (n+1)
						within
						<take(m)(xs)>^groupsof(n)(drop(m)(xs))

	drop(n)(xs) = if n==0 then xs else drop(n-1)(tail(xs))
|]

-- TODO: test for process data types

cspmListEnums = [$multilineLiteral|
	someSeq = <0..>
	someSet = {1..}
	
	someSeq1 = <0..12>
	someSet1 = {1..12}
	
	channel a, b, c 
	someEnumSet = {| a,b,c |}
	someEnumSet1 = {| x | x <- {a,b,c} |}
|]

-- Proper type checker cannot handle this
cspmFails = [$multilineLiteral|
	datatype B = A

	channel a : {0}.{0}.{0}
	channel b : B

	f(x,y) = {| x.y |}
	
	x = f(a,0)

	y = f(b,A)
|]

