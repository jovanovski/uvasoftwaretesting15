module Euler10 where

isPrime :: Integer -> Bool
isPrime x = length(filter (\y -> (rem x y == 0 && y^2<x)) [2..x]) == 0



sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
	where
		mark :: [Integer] -> Integer -> Integer -> [Integer]
		mark [] k m = []
		mark (y:ys) k m | k == m = 0 : (mark ys 1 m)
						| otherwise = y : (mark ys (k+1) m)


primes = sum(sieve [2..2000000])


