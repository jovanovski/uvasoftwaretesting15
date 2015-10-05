module Lab6Ex4 where

import Lab6Ex3
import Lecture6
import Data.List

testFermat :: Int -> IO (Integer)
testFermat k = helper composites where
  helper (x:xs) = do
    b <- prime_tests_F k x
    if b then return x else do
      r <- helper xs
      return r

{-
Tested using ghci, numbers that fooled the tests:
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
