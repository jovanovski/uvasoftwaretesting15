module Lab6Ex7 where

import Lecture6
import Lab6Ex6

mersenne :: Int -> IO ()
mersenne k = helper primes where
  helper (p:ps) =
    let c = 2^p-1
    in do
      b <- primeMR2 k c
      if b
      then do
        print (p,c)
        helper ps
      else helper ps

{-
Finds mersenne primes correctly for p in {2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279}
These are all mersenne primes according to wikipedia.

Tested using 'mersenne k'
-}

-- Time taken 1h
