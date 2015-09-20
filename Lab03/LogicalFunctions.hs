module LogicalFunctions where 

import Data.List
import Lecture3
import Testing

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
allVals2 f g = nub [sort (removeDuplicateVals (vf ++ vg)) | vf <- allVals f, vg <- allVals g] where 
  removeDuplicateVals [] = []
  removeDuplicateVals ((a,b):cs) = (a,b) : removeDuplicateVals (filter (\(x,y) -> x /= a) cs)

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
