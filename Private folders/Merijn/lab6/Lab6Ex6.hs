module Lab6Ex6 where

import Lab6Ex1
import Lab6Ex3
import Lab6Ex5
import Lecture6
import System.Random

primeMR2 :: Int -> Integer -> IO Bool
primeMR2 _ 2 = return True
primeMR2 0 _ = return True
primeMR2 k n =
  let
    (r,s) = decomp (n-1)
    f = \ x -> takeWhile (/= 1) (map (\ j -> exM2 x (2^j*s) n)  [0..r])
  in do
    a <- randomRIO (1, n-1) :: IO Integer
    if exM2 a (n-1) n /= 1
    then return False
    else
      if exM2 a s n /= 1 && last (f a) /= (n-1)
        then return False
        else primeMR2 (k-1) n

testPrimeMR2 :: Int -> [Integer]-> IO Integer
testPrimeMR2 k (x:xs) = do
  b <- primeMR2 k x
  if b
  then return x
  else testPrimeMR2 k xs

{-

Tested using 'testPrimeMR2 k carmichael'

The test is less likely to return true for composites than the fermat test, but
still returns true for the carmichael numbers sometimes.

-}

-- Time taken: 30m
