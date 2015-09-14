module Lab3 where 

import Data.List
import System.Random
import Lecture3
import Testing
import Test.QuickCheck
import Test.QuickCheck.Gen
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
allVals2 f g = nub [sort (nub (vf ++ vg)) | vf <- allVals f, vg <- allVals g]

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
    equivTests,
    parseTests
  ]


-- END test logical functions
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
arbForm 0 = liftM Prop arbitrary
arbForm n = oneof [
    liftM Prop arbitrary, 
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
  subform = arbForm (n `div` 10)

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

testToCnfIsEquiv :: IO ()
testToCnfIsEquiv = verboseCheck (\x -> equiv x (toCnf x))

testToCnfIsCnf :: IO ()
testToCnfIsCnf = verboseCheck (\x -> isCnf (toCnf x))

-- END test toCnf 
-- Time spent: 2h, tested using `testToCnfIsEquiv` and `testToCnfIsCnf`, which i think are the 2 important properties(?)