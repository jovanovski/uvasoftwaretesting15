module Dearr where
import Data.List

isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) = if x/=y then isDerangement xs ys
								else False
isDerangement2 :: [Integer] -> [Integer] -> Bool
isDerangement2 [] [] = True
isDerangement2 (x:xs) (y:ys) = True --TODO

---

checkLength :: [Integer] -> [Integer] -> Bool
checkLength [] [] = True
checkLength xs ys = length xs == length ys

checkElems :: [Integer] -> [Integer] -> Bool
checkElems [] [] = True
checkElems xs ys = sort xs == sort ys

---

deran :: Integer -> [[Integer]]
deran x = filter (\y -> isDerangement y [0..(x-1)]) (permutations [0..(x-1)])



--time needed : 5 minutes
--TODO tests