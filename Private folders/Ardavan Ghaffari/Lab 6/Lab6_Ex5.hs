module Lab6_Ex5 where

import Lecture6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

fermatTest :: Int -> [Integer] -> IO ()
fermatTest k (x:xs) = do
				value <- prime_tests_F k x
				if value then print ("the carmichael number that fools fermat's check when k=" ++ show k ++ " is " ++ show x)
					else fermatTest k xs

-- what we found during testing fermat's algorithm with carmichael numbers is that suprisingly for some of the 
-- numbers in the carmichael list, the algorithm returns false meaning that it classifies the number as a composite.
-- but we know that carmichael numbers are composite numbers that have the property of: a^(n-1) = 1 (mod n) so we expected
-- that for none of the carmicheal numbers the fermat test should return false. in other words whenever we increase k in the test
-- above, the first few numbers in the list are skiped. but we finaly realized what is the reason behind this. as stated in the
-- wikipedia page for carmicheal numbers: a carmicheal number is a composite number n which satisfies b^(n-1) = 1 (mod n) for all
-- Integers 1<a<n which are relatively prime to n. at first we did not pay attention to the second part of the property. the second part
-- is saying that if the selected b is not relatively prime to n then the first part of the property will not be satisfied meaning a^(n-1) /=1 (mod n)
-- so as we increase k, the number of times that a carmicheal number gets tested increases and there is more chance of finding a number between 1 and that
-- carmicheal number which is not relatively prime to it.