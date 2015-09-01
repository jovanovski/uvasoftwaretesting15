module Euler where

-- Continuing to sieve after x^2 > n takes too much time and is not necessary because we only use primes up to n.
-- After x^2 > n all remaining numbers between x and n must be prime, otherwise they would have already cancelled 
-- out by an earlier sieve step since they would've been a multiple of a number smaller than x. 
primesbelow :: Integer -> [Integer]
primesbelow n = sievebelow [2..] where 
  sievebelow (x:xs) = if x^2 > n then x:takeWhile (< n) xs else x:(sievebelow [y|y <- xs, y `mod` x /= 0])

sol10 :: Integer
sol10 = sum (primesbelow 2000000)

