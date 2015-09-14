module Ex1 where

import Lecture3
import Data.List

myForm1 = Cnj [p, Neg p]

myForm2 = Cnj [p, p]

myForm3a = Impl p q
myForm3b = Dsj [(Neg p), q]

myForm4a = Impl p q
myForm4b = Impl (Neg q) (Neg p)

allVals2 :: Form -> Form -> [Valuation]
allVals2 f1 f2 = nub [sort(nub(vf++vg)) | vf <- allVals f1, vg <- allVals f2]

contradiction :: Form -> Bool
contradiction f = not (any (\ v -> evl v f) (allVals f))

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f1 f2 = all (\ v -> not (evl v f1) || evl v f2) (allVals2 f1 f2)

equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\ v -> evl v f1 == evl v f2) (allVals2 f1 f2)

-- | time spent: 20 min
-- | checked via prefedined forms (myForm1, myForm2...)