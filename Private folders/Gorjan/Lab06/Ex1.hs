module Ex1 where

import Test.QuickCheck
import Data.Time
import Lecture6
import System.Random
import Data.Bits
import Data.Time

-- Ex1

powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

prop_test :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
prop_test (Positive x) (Positive y) (Positive n) = expM x y n == powm x y n 1

-- end Ex1

-- Ex2

test1 = powm 2988348162058574136915891421498819466320163312926952423791023078876139 2351399303373464486466122544523690094744975233415544072992656881240319 (10 ^ 40) 1
test2 = expM 2988348162058574136915891421498819466320163312926952423791023078876139 2351399303373464486466122544523690094744975233415544072992656881240319 (10 ^ 40)

-- end Ex2

--tryFind :: Integer -> Bool
--tryFind 1 = False
--tryFind n = all (\ a -> exM a (n-1) n == 1) [1..n-1]

--Ex3

composites :: [Integer]
composites = 4 : filter (not.isPrime) [5..]

-- end Ex3

-- Ex4: n = 4; the least is 4 that still fools primeF with k=3 on rare occasions

prop_tryfind :: Int -> IO Integer
prop_tryfind n = do
	a1 <- prime_test_F (composites!!n)
	a2 <- prime_test_F (composites!!n)
	a3 <- prime_test_F (composites!!n)
	if a1==a2 && a2==a3 then prop_tryfind (n+1) else return (composites!!n)

-- end Ex4

-- Ex5

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
       k <- [2..], 
       isPrime (6*k+1), 
       isPrime (12*k+1), 
       isPrime (18*k+1) ]

prime_tests_F_all :: [Integer] -> IO Integer
prime_tests_F_all (x:xs) = do
	tes <- prime_test_F x 
	if tes then return x
	else do 
		a <- prime_tests_F_all xs
		return a

run_Fprime :: [Integer] -> IO Integer
run_Fprime xs = do
	start <- getCurrentTime
	res <- prime_tests_F_all xs
	end <- getCurrentTime 
	let diff = diffUTCTime end start
	print ("Execution time: " ++ (show diff))
	return res

-- end Ex5
 
-- Ex6

primeMR' :: Int -> Integer -> IO Bool
primeMR' _ 2 = return True
primeMR' 0 _ = return True
primeMR' k n = let 
 (r,s) = decomp (n-1) 
 f = \ x -> takeWhile (/= 1) 
     (map (\ j -> powm x (2^j*s) n 1)  [0..r])
	in 
	 do 
	  a <- randomRIO (1, n-1) :: IO Integer
	  if powm a (n-1) n 1 /= 1 
	    then return False 
	    else 
	      if powm a s n 1 /= 1 && last (f a) /= (n-1) 
	        then return False
	        else primeMR' (k-1) n

primeMR_all :: [Integer] -> IO Integer
primeMR_all (x:xs) = do
	tes <- primeMR' 1 x 
	if tes then return x
	else do 
		a <- primeMR_all xs
		return a

run_primeMr :: [Integer] -> IO Integer
run_primeMr xs = do
	start <- getCurrentTime
	res <- primeMR_all xs
	end <- getCurrentTime 
	let diff = diffUTCTime end start
	print ("Execution time: " ++ (show diff))
	return res

-- end Ex6

-- Ex7

mrPrimes :: [Integer] -> Integer -> [Integer] -> IO [Integer]
mrPrimes (x:xs) lim ys = do
	tes <- primeMR' 1 (2^x - 1) 
	if tes then do
		let nys = x:ys
		if lim == 1 then return nys
		else do
			a <- mrPrimes xs (lim-1) nys
			return a
	else do 
		a <- mrPrimes xs lim ys
		return a


run_getMrNumbers :: Integer -> IO [Integer]
run_getMrNumbers limit = do
	start <- getCurrentTime
	res <- mrPrimes primes limit []
	end <- getCurrentTime 
	let diff = diffUTCTime end start
	print ("Execution time: " ++ (show diff))
	return res

-- end Ex7

-- Ex8

rsaTest :: IO String
rsaTest = do
	let primes1 = filter (>10000) (takeWhile (\x -> x<99999) primes)
	let primes2 = filter (>100000) (takeWhile (\x -> x<999999) primes)
	rnd1 <- randomRIO (0, length primes1)
	rnd2 <- randomRIO (0, length primes2)
	let g = primes1!!rnd1
	let p = primes2!!rnd2
	let secretA = m4
	let secretB = m5
	let partA = exM g secretA p
	let partB = exM g secretB p
	let key = exM partB secretA p
	let message = 2^17 -- limit on encoding
	let encoded = encode p key message
	print (show encoded)
	let decoded = decode p key encoded
	print (show decoded)
	return "Done"

encode' :: Integer -> Integer -> Integer -> Integer
encode' p k m = let 
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
 	in 
   	exM m e p

decode' :: Integer -> Integer -> Integer -> Integer
decode' p k m = let 
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
   d  = invM e p' 
 	in 
   	exM m d p