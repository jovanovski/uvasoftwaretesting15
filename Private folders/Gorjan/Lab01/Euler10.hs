module Euler10 where

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
	where
		mark :: [Integer] -> Integer -> Integer -> [Integer]
		mark [] k m = []
		mark (y:ys) k m | k == m = 0 : (mark ys 1 m)
						| otherwise = y : (mark ys (k+1) m)

primesSum ::Integer -> Integer
primesSum x = sum(sieve [2..x])