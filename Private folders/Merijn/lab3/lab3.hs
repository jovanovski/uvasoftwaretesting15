module Lab3 where 

import Data.List
import System.Random
import Lecture3
import Test.QuickCheck

contradiction :: Form -> Bool
contradiction f = not (any (\v -> evl v f) (allVals f))

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

allVals2 :: Form -> Form -> [Valuation]
allVals2 f g = nub ((allVals f) ++ (allVals g))

entails :: Form -> Form -> Bool
entails f g = all (\v -> not (evl v f) || evl v g) (allVals2 f g)

equiv :: Form -> Form -> Bool
equiv f g = all (\v -> evl v f == evl v g) (allVals2 f g)

