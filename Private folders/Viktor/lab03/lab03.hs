module Lab03 where
 
import Data.List
import System.Random
import Lecture3
import Testing

-- doesnt apply at least for one item (value)
contradiction :: Form -> Bool
contradiction f =  any (\ v -> not (evl v f)) (allVals f)

-- must be always true for all elements (values)
-- tautology (Impl (Prop 1) (Impl (Prop 2) (Prop 1)))
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- do cartesian product of all values of firt and second form
allVals2 :: Form -> Form -> [Valuation]
allVals2 p q = [pv++qv | pv <- allVals p, qv <- allVals q]

-- in a sense of logic, we can rewrite impl into Dsjunction as:
-- p -> q == not q || p
entails :: Form -> Form -> Bool
entails p q = all (\ v -> not (evl v q) || (evl v p)) (allVals2 p q)

-- p <=> q == p -> q && q -> p
-- the entails function is used
--equiv :: Form -> Form -> Bool
--equiv p q =  (entails p q) && (entails q p) 

equiv :: Form -> Form -> Bool
equiv f g = all (\v -> evl v f == evl v g) (allVals2 f g)

{--
-- REPORT
estimation time 15min, real time 30 min
--}

------------------ EX 02 ------------------

assertEqual :: (String , [Form]) -> Bool
assertEqual (s, xs) = parse s == xs

assertEqualString :: (String , String) -> Bool
assertEqualString (s, f) = show (parse s) == f

testParse :: [Test]
testParse = [Test "testing parse" assertEqual 
			[
				("", []),
				("*(1 +(2 -3))", [Cnj [Prop 1, Dsj [Prop 2, Neg (Prop 3)]]])
			]

		]++
		[Test "testing parse 2" assertEqualString
			[("*(1 2)", "[*(1 2)]"), ("  *(1 2)", "[*(1 2)]"), ("*(1   2)", "[*(1 2)]"), ("*(1 2)   ", "[*(1 2)]"), ("*(1 -2)", "[*(1 -2)]"), ("*(*1 -2)", "[]")]
		]++
		[ Test "plain input" assertEqualString
			[("*(1)","[*(1)]")]

		]++
		[Test "testing parse shoud be wrong" assertEqualString
			[("(1)","[(1)]"), ("1<=>1", "[1<=>1]"),("1==>1", "[1==>1]")]

		]

{--
-- REPORT
not really get a point of the exercise, but there are few mistakes with parsing:
It doesnt take single plain formula = (1); does take equiv and impl without brackets
Basically two ways of expressing formula are used - prefix and infix.
--}


------------------ EX 03 ----------------------
cnf :: Form -> Form
cnf f = deleteDup $ normal $ makeCnf $ nnf $ arrowfree f

makeCnf :: Form -> Form
makeCnf f@(Prop a) = f
makeCnf f@(Neg (Prop a)) = f
makeCnf (Cnj fs) = Cnj (map makeCnf fs)
makeCnf f@(Dsj []) = f
makeCnf (Dsj [f]) = makeCnf f
-- take one formula from the list and the rest as one formula and then distribute them
makeCnf (Dsj (f:fs)) = distributeDsj (makeCnf f) (makeCnf (Dsj fs)) 

-- applied distributite rule
distributeDsj :: Form -> Form -> Form
distributeDsj (Cnj []) _ = Cnj []
distributeDsj _ (Cnj []) = Cnj []
distributeDsj (Cnj [f]) g = distributeDsj f g
distributeDsj f (Cnj [g]) = distributeDsj f g
distributeDsj (Cnj (f:fs)) g = Cnj ([distributeDsj f g]++[distributeDsj (Cnj fs) g]) 
distributeDsj f (Cnj (g:gs))  = Cnj ([distributeDsj f g]++[distributeDsj f (Cnj gs)])
distributeDsj f g = Dsj [f, g]


-- function normal* normalize list of formulas - deletes brackets which are not needed
normal :: Form -> Form
normal f@(Prop a) = f
normal f@(Neg (Prop a)) = f
normal (Cnj fs) = Cnj (normalCnj fs)
normal (Dsj fs) = Dsj (normalDsj fs)
normal f = f

normalDsj :: [Form] -> [Form]
normalDsj [] = []
normalDsj (f@(Dsj fs):xs) = normalDsj (fs++xs)
--normalDsj (f@(Cnj fs):xs) = (normal f):(normalCnj xs)
--normalDsj (f@(Prop a):xs) = (normal f):(normalCnj xs)
--normalDsj (f@(Neg (Prop a)):xs) = (normal f):(normalCnj xs)
normalDsj (x:xs) = (normal x):(normalCnj xs)

normalCnj :: [Form] -> [Form]
normalCnj [] = []
normalCnj (f@(Cnj fs):xs) = normalCnj (fs++xs)
--normalCnj (f@(Dsj fs):xs) = (normal f):(normalCnj xs)
--normalCnj (f@(Prop a):xs) = (normal f):(normalCnj xs)
--normalCnj (f@(Neg (Prop a)):xs) = (normal f):(normalCnj xs)
normalCnj (x:xs) = (normal x):(normalCnj xs)

-- suppose to delete duplicates and redundands
deleteDup :: Form -> Form
deleteDup (Cnj fs) = (Cnj (map deleteDup fs))
deleteDup (Dsj fs) = (Dsj (nub fs))
deleteDup f = f
 {----}

-- maximum 2 forms, not used
deMorgan :: Form -> Form
deMorgan (Prop p)= (Prop p)
deMorgan (Neg (Neg (Prop p))) = (Prop p)
deMorgan (Neg f) = (Neg f)
deMorgan (Dsj [p,q]) = Cnj [Neg p, Neg q]
deMorgan (Cnj [p,q]) = Dsj [Neg p, Neg q]

-- TESTING
a = Prop 1
b = Prop 2
c = Prop 3 


myForm1 = Neg a
myForm2 = Cnj [a,b]
myForm3 = Dsj [a,b]
myForm4 = Dsj [Cnj [a,Cnj [b,c]], a]
myForm4Correct = Cnj []
myForm5 = Cnj [a,Cnj[b,c]]
myForm6 = Dsj [a,Cnj [a,Cnj [b,c]]]
myForm6Correct = Cnj [Dsj [a,a], Cnj [Dsj [a,b], Dsj [a,c]]]
myForm7 = Dsj [a, Cnj [a,b,c]]
myForm7Correct = Cnj [Dsj [a,a], Dsj [a,b], Dsj [a, c]]


assertEqualForm :: (Form ,Form) -> Bool
assertEqualForm (f,g) = cnf f == g

assertTrue :: (Bool) -> Bool
assertTrue b = b == True

test03 :: [Test]
test03 = [Test "testing cnj" assertEqualForm [
			(Prop 1, Prop 1),
			(myForm1, myForm1),
			(myForm2, myForm2),
			(myForm3, myForm3),
			(myForm5, myForm5),
			(myForm6, myForm6Correct),
			(myForm7, myForm7Correct)
			]
		]

-- returns True if generated and genereted cnf form are equivalent
testCnfGen :: IO Bool
testCnfGen = do
		orifForm <- genForm
		let cnfForm = cnf orifForm
		let res = equiv orifForm cnfForm
		return res
	

----------------- EX 04 ---------------------
setOfProp :: [Form]
setOfProp = [Prop 1, Prop 2, Prop 3, Prop 4, Prop 5]

numberOfOperations = 4
{----}
-- function generates formula
genForm :: IO Form
genForm = genForm' 1	

-- helping function with the argument of depth - deeper we are in the recursion, higher is the chance to generate simple Formula (literal),
-- which takes us out from recursion
genForm' :: Int -> IO Form
genForm' depth = do
		b <- genInt depth
		if (condition b depth) then do
			p <- genForm' (depth+1)
			q <- genForm' (depth+1)
			result <- makeForm p q
			return result
		else do
			l <- genLiteral
			return l

condition :: Int -> Int-> Bool
condition n d = if (n == 0) then True else False
--condition i d = if (((d `mod` 2) - i) < 0) then True else False

-- wrapper for ex03 function cnf
makeFormCnf :: IO Form
makeFormCnf = do
			f <- genForm
			let result = cnf f
			return result

-- randomly makes Formula depending on randomly picked operation
makeForm :: Form -> Form -> IO Form
makeForm p q = do
		kind <- genInt numberOfOperations
		if (kind == 0) then return (Cnj [p,q])
		else if (kind == 1) then return (Dsj [p,q])
		else if (kind == 2) then return (Impl p q)
		else return (Equiv p q)

-- function generates Literal, result might be negative
genLiteral :: IO Form
genLiteral = do
		ix <- genInt ((length setOfProp)-1)
		let f =  setOfProp!!ix
		result <- (negLiteral f)
		return result

-- randomly negates formula
negLiteral :: Form -> IO Form
negLiteral f = do
		b <- genInt 1
		if b == 0 then return f else return (Neg f)

-- random number generator
genInt :: Int -> IO Int
genInt n = do
    rs <- randomRIO (0, n)
    return rs




