module Euler where

-- Continuing to sieve after x^2 > n takes too much time and is not necessary because we only use primes up to n.
-- After x^2 > n all remaining numbers between x and n must be prime, otherwise they would have already cancelled 
-- out by an earlier sieve step since they would've been a multiple of a number smaller than x. 
primesbelow :: Integer -> [Integer]
primesbelow n = sievebelow [2..] where 
  sievebelow (x:xs) = if x^2 > n then x:takeWhile (< n) xs else x:(sievebelow [y|y <- xs, y `mod` x /= 0])

sol10 :: Integer
sol10 = sum (primesbelow 2000000)


tripleswithsum :: Integer -> [(Integer, Integer, Integer)]
tripleswithsum n = [(a, b, (n - a - b)) | a <- [1..(n `div` 3)-1], b <- [(a+1)..((n-a) `div` 2)]]

sol9 :: [(Integer, Integer, Integer)]
sol9 = [(a,b,c) | (a,b,c) <- tripleswithsum 1000, a^2 + b^2 == c^2]