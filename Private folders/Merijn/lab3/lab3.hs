module Lab3 where 

import Data.List
import System.Random
import Lecture3
import Testing
import Test.QuickCheck
import Control.Monad

-- START logical functions

contradiction :: Form -> Bool
contradiction f = not (any (\v -> evl v f) (allVals f))

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

allVals2 :: Form -> Form -> [Valuation]
allVals2 f g = nub [sort (helper (vf ++ vg)) | vf <- allVals f, vg <- allVals g] where 
  helper ((a,b):cs) = (a,b) : filter (\(x,y) -> x /= a) cs

entails :: Form -> Form -> Bool
entails f g = all (\v -> not (evl v f) || evl v g) (allVals2 f g)

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
equiv f g = all (\v -> evl v f == evl v g) (allVals2 f g)

testEquiv :: (Form, Form, Bool) -> Bool
testEquiv (f,g,e) = equiv f g == e

equivTests :: [Test]
equivTests = [ Test "equiv tests" testEquiv
    [
      (Prop 1, Prop 1, True),
      (Impl (Prop 1) (Prop 2), Dsj [Neg (Prop 1), Prop 2], True),
      (Prop 1, Prop 2, False),
      (Cnj [Prop 1, Prop 2], Dsj [Prop 1, Prop 2], False)
    ]
  ]


logicalTests = concat [
    contradictionTests,
    tautologyTests,
    entailsTests,
    equivTests
  ]


-- END logical functions
-- Time spent: 1.5h, tested using `runTests logicalTests`

-- START test parse

testParse :: (String, String) -> Bool
testParse (s,e) = show (parse s) == e

parseTests :: [Test]
parseTests = [ Test "parse tests" testParse
    [
      ("*(1 +(2 -3))", "[*(1 +(2 -3))]"),
      ("*(1 +(2 -3)", "[]"),
      ("*(1 +(2 -3))))", "[*(1 +(2 -3))]")
    ]
  ]

-- END test parse 
-- Time spent: 30m, tested using `runTests parseTests`

-- START convert to CNF

toCnf :: Form -> Form
toCnf f = Cnj [ Dsj [if v then Neg (Prop n) else Prop n | (n,v) <- vs] | vs <- allVals f, evl vs f == False]

-- END convert to CNF 
-- Time spent: 3h, of which 2,5h trying a method without truth table conversion, which turned out to be way too hard...

-- START test toCnf

instance Arbitrary Form where
  arbitrary = sized arbForm

arbForm :: Int -> Gen Form
arbForm 0 = arbProp
arbForm n = oneof [
    arbProp, 
    liftM Neg subform, 
    do 
      a <- subform
      b <- subform
      return $ Cnj [a,b],
    do 
      a <- subform
      b <- subform
      return $ Dsj [a,b],
    liftM2 Impl subform subform,
    liftM2 Equiv subform subform
  ] where 
  subform = arbForm (n `div` 2)

-- generator for non-negative properties below 5
arbProp :: Gen Form
arbProp = do
  a <- resize 4 arbitrary
  return $ Prop (abs a)

isCnf :: Form -> Bool 
isCnf (Cnj fs) = all isCnfClause fs
isCnf f = isCnfClause f

isCnfClause :: Form -> Bool
isCnfClause (Dsj fs) = all isCnfLiteral fs
isCnfClause f = isCnfLiteral f

isCnfLiteral :: Form -> Bool
isCnfLiteral (Prop _) = True
isCnfLiteral (Neg (Prop _)) = True
isCnfLiteral _ = False

-- Test property - Forms returned by tpCnf should be logically equivalent 
prop_ToCnfIsEquiv :: Form -> Bool
prop_ToCnfIsEquiv f = equiv f (toCnf f)

-- Test property - Forms returned by toCnf should be in Cnf form
prop_ToCnfIsCnf :: Form -> Bool
prop_ToCnfIsCnf f = isCnf (toCnf f)

-- END test toCnf 
-- Time spent: 2h, tested using quickCheck for properties `prop_ToCnfIsEquiv` and `prop_ToCnfIsCnf`, which i think are the 2 important properties(?)

-- START bonus - resolutions style theorem proving

type Clause = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Cnj fs) = map cnfClause2cls fs
cnf2cls f = [cnfClause2cls f]

cnfClause2cls :: Form -> Clause
cnfClause2cls (Dsj fs) = map cnfNeg2Cls fs
cnfClause2cls f = [cnfNeg2Cls f]

cnfNeg2Cls :: Form -> Int 
cnfNeg2Cls (Neg f) = - (cnfLiteral2Cls f)
cnfNeg2Cls f = cnfLiteral2Cls f

cnfLiteral2Cls :: Form -> Int
cnfLiteral2Cls (Prop p) 
  | p == 0 = error "properties cannot have name 0"
  | otherwise = p
cnfLiteral2Cls _ = error "not cnf"

-- dual of evl, but for Clauses
evlClauses :: Valuation -> Clauses -> Bool
evlClauses v cs = all (evlClause v) cs

evlClause :: Valuation -> Clause -> Bool
evlClause v c = any (evlLiteral v) c

evlLiteral :: Valuation -> Int -> Bool
evlLiteral [] l = error ("no info for " ++ show l)
evlLiteral ((i,b):xs) l
  | abs l == i = if l > 0 then b else not b
  | otherwise = evlLiteral xs l

containsProp0 :: Form -> Bool
containsProp0 (Prop p) = p == 0
containsProp0 (Neg f) = containsProp0 f
containsProp0 (Cnj fs) = any containsProp0 fs
containsProp0 (Dsj fs) = any containsProp0 fs
containsProp0 (Impl f g) = containsProp0 f || containsProp0 g
containsProp0 (Equiv f g) = containsProp0 f || containsProp0 g

-- Test property - CNF forms returned by cnf2cls should be logically equivalent to original
prop_Cnf2ClsFromCnfIsEquiv f = not (containsProp0 f) ==> all (\v -> evl v f == evlClauses v (cnf2cls (toCnf f))) (allVals f)

-- END bonus 
-- Time spent: 2 h, tested using quickCheck for property `prop_Cnf2ClsFromCnfIsEquiv`