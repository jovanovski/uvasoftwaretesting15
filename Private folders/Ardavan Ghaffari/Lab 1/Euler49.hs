module Euler49 where

import Data.List	

isPrime :: Integer -> Bool
isPrime n = all (\m -> rem n m /= 0) ms	where ms = takeWhile (\p -> p^2 <= n) primes

primes :: [Integer]
primes = 2 : filter isPrime [3..9999]

digits :: Integer -> [Integer]   								
digits 0 = []
digits x = digits(div x 10) ++ [mod x 10]

computeEuler49 :: [(Integer,Integer,Integer)]
computeEuler49 = [ (a,b,c) | a <- filter (\p -> p > 1000 && p <= 3330) primes,
 								b <- filter (\p -> p > 3330 && p <= 6661) primes,
  								c <- filter (\p -> p > 6661 && p < 9999) primes,
   								b - a == 3330, c - b == 3330, sort(digits a) == sort(digits b), sort(digits b) == sort(digits c) ]