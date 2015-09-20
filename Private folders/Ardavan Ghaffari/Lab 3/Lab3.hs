module Lab3 where

import Data.List
import System.Random
import Lecture3
import Testing

--Exercise1

--time spent: 3 hours mostly because at first i was not able to implement the "equiv" and "entails" functions.
--but after a while i realised that two propositional formulas f1 and f2 are equivalent if f1 <==> f2 is a tautoloty,hence my
--concise implementation for "equiv". the same goes for "entails"	

contradiction :: Form -> Bool
contradiction f = all (\v -> not(evl v f) ) (allVals f)

testContradiction :: (Form, Bool) -> Bool
testContradiction (f,e) = contradiction f == e

contradictionTests :: [Test]
contradictionTests = [ Test "contradiction tests" testContradiction
    [
      (Cnj [Prop 1, Neg (Prop 1)], True),
      (Cnj [Dsj [Prop 1, Prop 2], Cnj [Neg (Prop 1), Neg (Prop 2)]], True),
      (Cnj [Prop 1, Prop 2], False)
    ]
  ]


tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

testTautology :: (Form, Bool) -> Bool
testTautology (f,e) = tautology f == e

tautologyTests :: [Test]
tautologyTests = [ Test "tautology tests" testTautology
    [
      (Dsj [Prop 1, Neg (Prop 1)], True),
      (Cnj [Dsj [Prop 1, Neg (Prop 1) ], Dsj [Prop 2, Neg (Prop 2)] ], True),
      (Cnj [Prop 1, Prop 2], False)
    ]
  ]

entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

testEntails :: (Form, Form, Bool) -> Bool
testEntails (f,g,e) = entails f g == e

entailsTests :: [Test]
entailsTests = [ Test "entails tests" testEntails
    [
      (Prop 1, Prop 1, True),
      (Cnj [Prop 1, Prop 2], (Impl (Prop 1) (Prop 2)), True),
      ((Impl (Prop 1) (Prop 2)), Cnj [Prop 1, Prop 2], False),
      (Prop 1, Neg (Prop 1), False),
      (Prop 1, Impl (Prop 2) (Prop 1), True)
    ]
  ]

equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

testEquiv :: (Form, Form, Bool) -> Bool
testEquiv (f1,f2,b) = equiv f1 f2 == b

equivTests :: [Test]
equivTests = [Test "equivTests" testEquiv
				[
					(p,p,True),
					(p,q,False),
					(Impl p q,Dsj[Neg p,q],True),
					(Neg(Dsj[p,q]),Cnj[Neg p,Neg q],True)
				]
			]

----------------------------------------------------------------------------------------------------------

--Exercise2

--time spent: 1 hour

--test report: as suggested by the hint in the lecture, i decided to use the show function for formulas 

testParse :: (String,String) -> Bool
testParse (i,o) = show(parse i) == o

parseTests :: [Test]
parseTests = [Test "testing parse" testParse
				[
					("*(1 +(2 -3))","[*(1 +(2 -3))]"),
					("*(1 +(2 -3)",[]),
					("*(1 +(2 -3)))))","[*(1 +(2 -3))]"),
					("*(1 +(==>2 -3))",[])	
				]
			]

------------------------------------------------------------------------------------------------------------	

--Exercise3

--time spent: 2 hours

--test report: our group decided to skip the method proposed in the lecture and instead implement it as proposed
--in question number 13 in the workshop. first we determine in which valuations the formula evaluates to False.then 
--we negate all the propositional letters in that valuation and then form a disjunction of them. at the end a conjunction
--of all the produced disjunctions gives us the CNF format.	

convertToCNF :: Form -> Form
convertToCNF f = Cnj (map (\v -> Dsj (cnfHelper v)) (filter (\v -> evl v f == False) (allVals f)))
 where
	cnfHelper [] = []
	cnfHelper (v:vs) = if snd v then Neg (Prop (fst v)) : cnfHelper vs else Prop (fst v) : cnfHelper vs

------------------------------------------------------------------------------------------------------------

--Exercise4

--time spent: a lot, i think about 8 hours because at first i started to read QuickCheck so that 
--it can generate propositional formulas for me but unfortunately i was unable to understand it. Therefore
--i had to generate these formulas myself with the help of a generator function. implementing that function was 
--time consuming for me as well. but at the end i managed to do it.

--test report: in order to test "converToCNF" function you can run "test_convertToCNF" and it will run 100
--tests each time. due to lack of time i only managed to define only one testable property named "prop_IsEquivalent"
--which basically checks weather the original randomly generated formula is equivalent to it cnf counterpart or not.

test_convertToCNF :: IO ()
test_convertToCNF = testR 1 100 convertToCNF prop_IsEquivalent 							

testR :: Int -> Int -> (Form -> Form) -> (Form -> Form -> Bool) -> IO ()
testR k n f p = if k == n then print(show n ++ " tests passed successfully!")
				else do
					d <- getRandomInt 3
					rndForm <- genRndForm d
					let cnfEquivalent = convertToCNF rndForm
					if cnfEquivalent == Cnj[] then 
						do print( show rndForm ++ " is a tautology and therefore it's CNF is a tautology as well")
						   testR (k+1) n f p
					else if p (rndForm) (convertToCNF rndForm) then
						do print(show rndForm ++ " and " ++ show cnfEquivalent ++ " are equivalent!")
						   testR (k+1) n f p
						else error("test failed on: " ++ show rndForm) 						

propList = [Prop 1, Prop 2, Prop 3, Prop 4]

-- this function generates propositional formulas automatically
genRndForm :: Int -> IO Form
genRndForm 0 = genRndProp
genRndForm n = do
	m <- getRandomInt 3
	form1 <- genRndForm (n-1)
	form2 <- genRndForm (n-1)
 	if m == 0 then return (Cnj[form1, form2])
		else if m == 1 then return (Dsj[form1, form2])
			else if m == 2 then return (Impl form1 form2)
				else return (Equiv form1 form2)
			  

genRndProp :: IO Form
genRndProp = do
	n <- getRandomInt((length propList) - 1)
	let rndProp = propList !! n
	m <- getRandomInt 1
	if m == 0 then return rndProp else return (Neg(rndProp))

-- Testable property
prop_IsEquivalent :: Form -> Form -> Bool
prop_IsEquivalent f1 f2 = equiv f1 f2	

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))
			  
			  