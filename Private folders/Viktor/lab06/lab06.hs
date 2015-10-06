module Lab6 where
 
import Data.List
import System.Random
import Lecture6
import Data.Time
import Test.QuickCheck


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
testFermat :: Int -> IO String
testFermat k = do
		r1 <- testFermat' 1 composites
		--print "The first composite number doesnt hold prime_tests_F for 1: "++(show r1)
		r2 <- testFermat' 2 composites
		--print "The first composite number doesnt hold prime_tests_F for 2: "++(show r2)
		r3 <- testFermat' 3 composites
		--print "The first composite number doesnt hold prime_tests_F fr 3: "++(show r3)
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


