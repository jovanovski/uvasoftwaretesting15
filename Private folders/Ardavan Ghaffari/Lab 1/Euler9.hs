module Euler9 where

isInt :: Float -> Bool
isInt x = x == fromInteger (round x)

hasSqrt :: Float -> Bool
hasSqrt x = isInt (sqrt x)


doPytha :: [Float] -> [Float] -> [Integer]
doPytha [] [] = []
doPytha (x:xs) [] = doPytha xs [1..1000]
doPytha [] (y:ys) = []
doPytha (x:xs) (y:ys) = if hasSqrt (x^2 + y^2)  && (x + y + sqrt(x^2 + y^2))==1000 then [truncate(x),truncate(y)]
						else doPytha (x:xs) ys
