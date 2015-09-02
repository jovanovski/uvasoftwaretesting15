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

optimizedSieve :: Integer -> [Integer] -> [Integer]
optimizedSieve n (x:xs) = if x^2 > n then x:(filter (<n) xs) else x : (optimizedSieve n (filter (\y -> y `mod` x /= 0) xs))

primes :: Integer -> Integer -> [Integer]
primes x y = if x>=2 && y>=2 then optimizedSieve y [x..y]
				else if x==2 && y==2 then [2]
				else []

getDigits :: Integer -> [Integer]
getDigits x = if x>0 then (x `mod` 10) : getDigits (x `div` 10) else []

pairs :: [Integer] -> [Integer] -> [Integer] -> Integer
pairs (x:xs) (y:ys) (p:ps) = if x/= 1487 && x<y && sort (getDigits x) == sort (getDigits y) && (y + y - x) > y && (contains (y + y - x) (p:ps)) && sort (getDigits (y+y-x)) == sort (getDigits y) then (x*100000000)+(y*10000)+(y+y-x)
								else if xs/=[] && ys==[] then pairs xs (getLargerThan x (p:ps)) (p:ps)
								else if xs==[] && ys==[] then 0
								else pairs (x:xs) ys (p:ps)

contains :: Integer -> [Integer] -> Bool
contains n [] = False
contains n (x:xs) = if x==n then True else contains n xs

findPairs :: [Integer] -> Integer
findPairs (x:zs) = pairs (x:zs) zs (x:zs)

getLargerThan :: Integer -> [Integer] -> [Integer]
getLargerThan x ys = filter (\y -> y >= x) ys

getPrimes :: Integer -> [Integer]
getPrimes x = filter (\y -> y>x) (primes 2 9999)

callPairs :: Integer
callPairs = findPairs (getPrimes 999)
