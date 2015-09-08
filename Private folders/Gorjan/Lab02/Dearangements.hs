module Dearr where
import Data.List

isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) = if x/=y then isDerangement xs ys
								else False

deran :: Integer -> [[Integer]]
deran x = filter (\y -> isDerangement y [0..(x-1)]) (permutations [0..(x-1)])

--time needed : 5 minutes
--TODO tests