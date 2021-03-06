module Euler9 where

isInt :: Float -> Bool
isInt x = x == fromInteger (round x)

hasSqrt :: Float -> Bool
hasSqrt x = isInt (sqrt x)

-- call this function to start the program
doPytha :: [Float] -> [Float] -> (Integer, Integer, Integer)
doPytha [] [] = (0,0,0)
doPytha (x:xs) [] = doPytha xs [1..1000]
doPytha [] (y:ys) = (0,0,0)
doPytha (x:xs) (y:ys) = if hasSqrt (x^2 + y^2)  && (x + y + sqrt(x^2 + y^2))==1000 then (truncate(x),truncate(y), truncate(sqrt(x^2 + y^2)))
						else doPytha (x:xs) ys

doPytha2 :: [Float] -> [Float] -> [(Integer, Integer, Integer)]
doPytha2 [] [] = []
doPytha2 (x:xs) [] = doPytha2 xs [1..1000]
doPytha2 [] (y:ys) = []
doPytha2 (x:xs) (y:ys) = if hasSqrt (x^2 + y^2) then (truncate(x),truncate(y), truncate(sqrt(x^2 + y^2))) : doPytha2 (x:xs) ys
						else doPytha2 (x:xs) ys
