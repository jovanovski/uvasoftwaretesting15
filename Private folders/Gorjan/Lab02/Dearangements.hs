module Dearr where

import Data.List
import Testing

isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) = if x/=y then isDerangement xs ys else False


-- fucntion extracted from the Rosetta Code (http://rosettacode.org/wiki/Permutations/Derangements#Haskell), presumed to be functional
derangements :: [Integer] -> [[Integer]]
derangements xs = filter (and . zipWith (/=) xs) $ permutations xs

--- prechecks

checkInput :: Integer -> Bool
checkInput n = n>=0

---

deran :: Integer -> [[Integer]]
deran x = if checkInput x then filter (\y -> isDerangement y [0..(x-1)]) (permutations [0..(x-1)]) else error "Bad Input (<0)"

--- TESTING

testFunctions :: (Integer, Bool) -> Bool
testFunctions (x, b) = (deran x == derangements [0..(x-1)])==b

derangementsTests :: [Test]
derangementsTests = [ Test "testing derangements " testFunctions
             [(2, True), (3, True), (4, True), (5, True), (6, True), (7, True), (8, True)]
           ]

--time needed : 15 minutes