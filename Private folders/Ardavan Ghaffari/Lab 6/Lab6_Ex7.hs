module Lab6_Ex7 where

import Lecture6

mersennePrimes :: [Integer] -> IO ([Integer])
mersennePrimes (x:xs) = do
	value <- primeMR 3 x
	y <- mersennePrimes xs
	if value then return (x:y) else return (y)

run :: IO ([Integer])
run = do
	mp <- mersennePrimes (map (\p -> 2^p -1) primes)
	return mp

run' = map (\p -> 2^p -1) primes	