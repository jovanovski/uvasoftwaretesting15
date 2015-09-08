module Dearr where
import Data.List

isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) = if x/=y then isDerangement xs ys else False


-- fucntion extracted from the Rosetta Code (http://rosettacode.org/wiki/Permutations/Derangements#Haskell), presumed to be functional
derangements :: [Integer] -> [[Integer]]
derangements xs = filter (and . zipWith (/=) xs) $ permutations xs

--- precondition tests

checkInput :: Integer -> Bool
checkInput n = n>=0

---

deran :: Integer -> [[Integer]]
deran x = if checkInput x then filter (\y -> isDerangement y [0..(x-1)]) (permutations [0..(x-1)]) else error "Bad Input (<0)"

testFunctions :: Integer -> Bool
testFunctions x = deran x == derangements [0..(x-1)] 

runTests :: Integer -> Integer -> Integer -> String
runTests 0 p f = "Passed " ++ (show p) ++ " tests, failed " ++ (show f) ++ " tests"
runTests n p f = if testFunctions n then runTests (n-1) (p+1) f else runTests (n-1) p (f+1)

--time needed : 15 minutes