module Lab6Ex5 where

import Lecture6
import Lab6Ex4

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | k <- [2..], isPrime (6*k+1), isPrime (12*k+1), isPrime (18*k+1) ]

{-
These numbers fool the fermat test very quickly, but not all the time?? Not
exactly sure why...

Tested using 'testFermat k carmichael'
-}

-- Time taken 30m
