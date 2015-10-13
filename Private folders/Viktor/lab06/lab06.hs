module Lab6 where
 
import Data.List
import System.Random
import Lecture6
import Data.Time
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad


{-- 
exM2 :: Integer -> Integer -> Integer -> Integer -> Integer
exM2 x 0 _ m = x `mod` m
exM2 x 1 _ m = x `mod` m
exM2 x y e m = if (e' <= y) then exM2 x' y e' m
							else ((x*(exM2 x r 1 m)) `mod` m)
	where
		e'= e*2
		x'= ((x*x) `mod` m)
		r = y-e'
--}

exM2 :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
--exM2 x y m e r = r
exM2 x 1 m e r = r
exM2 x y m e r = if (e < y) then (exM2 x' y m e' r)
						  else (exM2 x' y m er (x `mod` y))
				where
					e' = e*2
					x'= x*x `mod` 2
					er = y - e 


powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

exM' = powm

{-- 
expm :: Integer → Integer → Integer → Integer
expm m b k =
	let
		ex a k s 	| k == 0 = s
					| k `mod` 2 == 0 = ((ex (a*a `mod` m)) (k `div` 2)) s
					| otherwise = ((ex (a*a `mod` m)) (k ‘div‘ 2)) (s*a `mod` m)
	in ex b k 1
--}


testEx :: Integer -> Integer -> Integer ->  IO Bool
testEx x y m = do
   start <- getCurrentTime
   let r1 = expM x y m
   print (("Result of expM: ")++(show r1))
   stop <- getCurrentTime
   print $ diffUTCTime stop start
   start <- getCurrentTime
   --let r2 = exM2 x y m 1 1
   let r2 = powm x y m 1
   print (("Result of exM: ")++(show r2))
   stop <- getCurrentTime
   print $ diffUTCTime stop start
   let result = r2 == r1
   return result

-- ex02
prop_exp :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
--prop_exp (Positive x) (Positive y) (Positive m) = (expM x y m) == (exM2 x y m 1 1)
prop_exp (Positive x) (Positive y) (Positive m) = (expM x y m) == (powm x y m 1)



--ex 03
composites :: [Integer]
composites = 4 : filter (not.isPrime) [5..]	


--ex04
{-- --} 
testFermat :: IO String
testFermat = do
		r1 <- testFermat' 1 composites
		print ("The first composite number doesnt hold prime_tests_F 1: "++(show r1))
		r2 <- testFermat' 2 composites
		print ("The first composite number doesnt hold prime_tests_F 2: "++(show r2))
		r3 <- testFermat' 3 composites
		print ("The first composite number doesnt hold prime_tests_F 3: "++(show r3))
		r4 <- testFermat' 100000 composites
		print ("The first composite number doesnt hold prime_tests_F 100000: "++(show r4))
		return "Finish"

testFermat' :: Int -> [Integer] ->IO Integer
testFermat' k (x:xs) = do
			b <- prime_tests_F k x
			if b then testFermat' k xs 
							else return x


--ex05
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

testFermat2 :: [Integer] ->IO Integer
testFermat2 (x:xs) = do
			b <- prime_test_F x
			if not b then testFermat2 xs 
							else return x


testCarmichael :: IO String
testCarmichael = do
		r1 <- testFermat2 carmichael
		print ("The first composite number doesnt hold prime_tests_F for 1: "++(show r1))
		return "Finish"

-- ex06
testCarMR :: [Integer] ->IO Integer
testCarMR (x:xs) = do
			b <- primeMR' 1 x
			if not b then testCarMR xs 
							else return x



testCarmichaelMR :: IO String
testCarmichaelMR = do
		r1 <- testCarMR carmichael
		print ("The first prime number from carmichael set: "++(show r1))
		return "Finish"


primeMR' :: Int -> Integer -> IO Bool
primeMR' _ 2 = return True
primeMR' 0 _ = return True
primeMR' k n = let 
   (r,s) = decomp (n-1) 
   f = \ x -> takeWhile (/= 1) 
       (map (\ j -> exM' x (2^j*s) n 1)  [0..r])
  in 
   do 
    a <- randomRIO (1, n-1) :: IO Integer
    if exM' a (n-1) n 1 /= 1 
      then return False 
      else 
        if exM' a s n 1 /= 1 && last (f a) /= (n-1) 
          then return False
          else primeMR' (k-1) n

-- ex06.2
isMersenne :: Integer -> IO Bool
isMersenne p = do
		b <- primeMR' 1 ((2^p)-1)
		if b then return True
			 else return False

--primesMersenne :: [Integer]
--primesMersenne = do

--	return [x | x <- primes, isMersenne x]

 
primesMersenne :: [Integer] ->IO [Integer]
primesMersenne (p:ps) = do
			b <- isMersenne p
			if b then do
					print (p)
					ps' <- primesMersenne ps
					return (p:ps') 
				else do
					ps' <- primesMersenne ps
					return ps'
{----}
-- ex07

onlyPrimesMR :: [Integer] -> IO [Integer] 
onlyPrimesMR (p:ps) = do
			b <- primeMR' 1 p
			if b then do
					print (p)
					ps' <- primesMersenne ps
					return (p:ps') 
				else do
					ps' <- primesMersenne ps
					return ps'
{----}


findPrimePair :: Int -> Int -> IO (Integer, Integer)
findPrimePair k n = do
	primesInRange <- filterM (\ x -> primeMR' 1 x) [2^k..2^n]
	let len = length primesInRange
	if (len <= 0) then return (1,1) --raise Error
				else do
					ix1 <- randomRIO (0, len-1)
					ix2 <- randomRIO (0, len-1)
					let p1 = primesInRange!!ix1
					let p2 = primesInRange!!ix2
					return (p1, p2)


a = m3
b = m4

type Triple = (Integer, Integer, Integer)


genKey :: Integer -> Integer -> IO Triple
genKey a b = do
	(p,q) <- findPrimePair 8 9
	let k1 = q^a `mod` p
	let k2 = q^b `mod` p
	let key = k1^b `mod` p
	let keyCheck = k2^a `mod` p
	let same = key == keyCheck
	if (same) then return (p,q,key)
			  else error "Failed"

{-- 
testCrypt :: Integer -> IO Bool
testCrypt m = do
	(p,q,key) <- genKey a b
	let c = encode p key m
	let d = decode p key c
	let res = m == d
	print ("orig: " ++ (show m) ++ ", coded: "++ (show c) ++ ", decoded: "++(show d) )
	return res
--}

testRSA :: Integer -> IO Bool
testRSA m = do
	(p,q) <- findPrimePair 14 15
	--print ((show p) ++ ", "++(show q))
	let public = rsa_public p q
	let private = rsa_private p q
	let c = rsa_encode public m
	let d = rsa_decode private c
	let res = m == d
	print ("orig: " ++ (show m) ++ ", coded: "++ (show c) ++ ", decoded: "++(show d) )
	return res

testRSAWithPrimes :: (Integer, Integer) -> Integer -> IO Bool
testRSAWithPrimes (p, q) m = do
	let public = rsa_public p q
	let private = rsa_private p q
	let c = rsa_encode public m
	let d = rsa_decode private c
	let res = m == d
	print ("orig: " ++ (show m) ++ ", coded: "++ (show c) ++ ", decoded: "++(show d) )
	return res

-- quickCheck prop_rsa not passing
prop_rsa :: Positive Integer -> Property
prop_rsa (Positive m) = monadicIO $ do
  factors <- run (testRSA m)
  assert (factors == True)

-- frozen every time
prop_rsa2 :: Positive Integer -> Property
prop_rsa2 (Positive m) = monadicIO $ do
  factors <- run (testRSAWithPrimes (10903,11527) m)
  assert (factors == True)
