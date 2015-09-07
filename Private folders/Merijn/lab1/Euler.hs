module Euler where

import Data.List

tripleswithsum :: Integer -> [(Integer, Integer, Integer)]
tripleswithsum n = [(a, b, (n - a - b)) | a <- [1..(n `div` 3)-1], b <- [(a+1)..((n-a) `div` 2)]]

tripleswithsumandpy :: Integer -> [(Integer, Integer, Integer)]
tripleswithsumandpy n = [(a,b,c) | (a,b,c) <- tripleswithsum n, a^2 + b^2 == c^2]

sol9 :: Integer
sol9 = 
  let (a,b,c) = head (tripleswithsumandpy 1000)
  in a*b*c

-- Continuing to sieve after x^2 > n takes too much time and is not necessary because we only use primes up to n.
-- After x^2 > n all remaining numbers between x and n must be prime, otherwise they would have already cancelled 
-- out by an earlier sieve step since they would've been a multiple of a number smaller than x. 
primesbelow :: Integer -> [Integer]
primesbelow n = sievebelow [2..] where 
  sievebelow (x:xs) = if x^2 > n then x:takeWhile (< n) xs else x:(sievebelow [y|y <- xs, y `mod` x /= 0])

sol10 :: Integer
sol10 = sum (primesbelow 2000000)

primesbetween :: Integer -> Integer -> [Integer] 
primesbetween l u = filter (>l) (primesbelow u)

isintperm :: Integer -> Integer -> Bool
isintperm x y = sort (show x) == sort (show y)

findseqtriples :: [Integer] -> [(Integer, Integer, Integer)]
findseqtriples [] = []
findseqtriples (x:xs) = helper xs ++ findseqtriples xs where 
  helper [] = []
  helper (y:ys) = 
    let z = (y + (y - x))
    in if elem z ys then [(x,y,z)] ++ helper ys else helper ys

findseqpermtriples :: [Integer] -> [(Integer, Integer, Integer)]
findseqpermtriples n = [(x,y,z) | (x,y,z) <- findseqtriples n, isintperm x y && isintperm y z]

sol49 :: Integer
sol49 = 
  let (a,b,c) = (findseqpermtriples (primesbetween 999 10000)) !! 1
  in read (show a ++ show b ++ show c)