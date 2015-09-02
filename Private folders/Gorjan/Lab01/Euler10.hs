module Euler10 where

sieve :: Integer -> [Integer] -> [Integer]
sieve n [] = []
sieve n (0 : xs) = sieve n xs
sieve n (x : xs) = if x^2 > n then x:(filter (>0) xs) else x : sieve n (mark xs 1 x)
	where
		mark :: [Integer] -> Integer -> Integer -> [Integer]
		mark [] k m = []
		mark (y:ys) k m | k == m = 0 : (mark ys 1 m)
						| otherwise = y : (mark ys (k+1) m)

primesSum ::Integer -> Integer
primesSum x = sum(sieve x [2..x])
