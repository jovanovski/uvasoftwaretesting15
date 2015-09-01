module Euler10 where

import Data.List
import System.Environment

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
	where
		mark :: [Integer] -> Integer -> Integer -> [Integer]
		mark [] k m = []
		mark (y:ys) k m | k == m = 0 : (mark ys 1 m)
						| otherwise = y : (mark ys (k+1) m)

primes :: Integer -> Integer -> [Integer]
primes x y = if x>=2 && y>=2 then sieve [x..y]
				else if x==2 && y==2 then [2]
				else []

getDigits :: Integer -> [Integer]
getDigits x = if x>0 then (x `mod` 10) : getDigits (x `div` 10) else []

pairs :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> (Integer, Integer, Integer)
pairs (x:xs) (y:ys) (z:zs) (p:ps) = if x/= 1487 && x/= y && y/=z && x/=z && sort (getDigits x) == sort (getDigits y) && sort (getDigits y) == sort (getDigits z) && z-y == y-x then (x,y,z)
								else if ys/=[] && zs==[] then pairs (x:xs) ys (getLargerThan x (p:ps)) (p:ps)
								else if xs/=[] && ys==[] && zs==[] then pairs xs (getLargerThan x (p:ps)) (getLargerThan x (p:ps)) (p:ps)
								else if xs==[] && ys==[] && zs==[] then (0,0,0)
								else pairs (x:xs) (y:ys) zs (p:ps)

findPairs :: [Integer] -> (Integer, Integer, Integer)
findPairs (x:y:zs) = pairs (x:y:zs) (y:zs) zs (x:y:zs)

callPairs :: (Integer, Integer, Integer)
callPairs = findPairs (getPrimes 999)

getLargerThan :: Integer -> [Integer] -> [Integer]
getLargerThan x ys = filter (\y -> y >= x) ys

getPrimes :: Integer -> [Integer]
getPrimes x = filter (\y -> y>x) (primes 2 9999)

