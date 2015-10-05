module Lab6Ex4 where

import Lab6Ex3
import Lab6Ex1
import Lecture6
import System.Random

prime_tests_F2 :: Int -> Integer -> IO Bool
prime_tests_F2 k n = do
  as <- sequence $ fmap (\_-> randomRIO (1,n-1)) [1..k]
  return (all (\ a -> exM2 a (n-1) n == 1) as)

testFermat :: Int -> [Integer] -> IO (Integer)
testFermat k (x:xs) = do
  b <- prime_tests_F2 k x
  if b
  then return x
  else do
    r <- testFermat k xs
    return r

{-
Tested using ghci and 'testFermat k composites'.

Numbers that fooled the tests:
k=1 = 4
k=2 = 65
k=3 = 91
k=4 = 703
k=5 = 4
k=10 = ... none

As we increase k it becomes unlikely that the test returns true for a composite
number, but any k can in theory return true for 4, is it did in the case of k=5.
-}

-- Time taken: 30m
