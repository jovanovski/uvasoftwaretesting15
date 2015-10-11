module Lab6_Ex4 where

import Lecture6
import Lab6_Ex3

fermatTest :: Int -> [Integer] -> IO ()
fermatTest k (x:xs) = do
				value <- prime_tests_F k x
				if value then print ("the least composite number that can fool fermat's check while k=" ++ show k ++ " is " ++ show x)
					else fermatTest k xs

-- the least composite number that can fool fermat's check while k=1 is 4																
-- the least composite number that can fool fermat's check while k=1 is 12
-- the least composite number that can fool fermat's check while k=2 is 15
-- the least composite number that can fool fermat's check while k=2 is 9
-- the least composite number that can fool fermat's check while k=3 is 561
-- the least composite number that can fool fermat's check while k=3 is 2465
-- the least composite number that can fool fermat's check while k=4 is 1105
-- the least composite number that can fool fermat's check while k=4 is 561
-- the least composite number that can fool fermat's check while k=5 is 1105
-- the least composite number that can fool fermat's check while k=5 is 1729
-- the least composite number that can fool fermat's check while k=6 is 1729
-- the least composite number that can fool fermat's check while k=7 is 2465
-- the least composite number that can fool fermat's check while k=8 is 2821
-- the least composite number that can fool fermat's check while k=9 is 8911
-- the least composite number that can fool fermat's check while k=10 is 10585
-- the least composite number that can fool fermat's check while k=11 is 46657
-- the least composite number that can fool fermat's check while k=12 is 29341
-- the least composite number that can fool fermat's check while k=13 is 52633
-- the least composite number that can fool fermat's check while k=14 is 252601
-- the least composite number that can fool fermat's check while k=15 is 252601
-- as we increase k the chances of fooling the fermat's primality test becomes less and less.

