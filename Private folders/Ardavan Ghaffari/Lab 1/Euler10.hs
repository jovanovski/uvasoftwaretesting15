module Euler10 where

isPrime :: Integer -> Bool
isPrime n = all (\p -> rem n p /= 0) ps where ps = takeWhile (\x -> x^2 <= n) primes

primes :: [Integer]
primes = 2 : filter isPrime [3..2000000]

computeEuler10 = sum(primes)