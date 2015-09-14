import Test.QuickCheck
import Testing
import Lecture3
import System.Random
import Ex03

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
		if b == 0 then do
			p <- genForm' (depth+1)
			q <- genForm' (depth+1)
			result <- makeForm p q
			return result
		else do
			l <- genLiteral
			return l


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


-- TESTING
{-- WILL BE DELETED
(+(4 -1)<=>5)
Equiv (Dsj [Prop 4, Neg $ Prop 1]) (Prop 5)
+(*(+(4 -1) 5) *(*(-4 1) -5))  // arrowfree and nnf
Dsj [Cnj [Dsj [Prop 4, Neg $ Prop 1], Prop 5], Cnj [Cnj [Neg $ Prop 4, Prop 1], Neg $ Prop 5]]
--}

