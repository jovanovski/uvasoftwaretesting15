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

-- END test logical functions
--
-- time spent: 1.5h

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
--
-- time spent: 30m

toCnf :: Form -> Form
toCnf f = Cnj [ Dsj [if v then Neg (Prop n) else Prop n | (n,v) <- vs] | vs <- allVals f, evl vs f == False]

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

allTests = concat [
    contradictionTests,
    tautologyTests,
    entailsTests,
    equivTests,
    parseTests
  ]
