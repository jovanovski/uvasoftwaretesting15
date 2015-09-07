module Euler9 where

specialPythagoreanTriplet :: [(Integer,Integer,Integer)]
specialPythagoreanTriplet = take 1 (filter (\(a,b,c) -> a^2 + b^2 == c^2 && a + b + c == 1000) [ (a,b,c) | c <- [1..1000],b <- [1..c], a <- [1..b], a<b, b<c ])
computeEuler9 = head $ map (\(a,b,c) -> a*b*c) specialPythagoreanTriplet