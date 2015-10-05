module Lab6Ex1 where

import Lecture6
import Test.QuickCheck

exM2 :: Integer -> Integer -> Integer -> Integer
exM2 _ _ 0 = error "Cannot exponentiate modulo 0"
exM2 _ 0 m = 1 `mod` m
exM2 x y m
  | y < 0     = error "Cannot do negative exponentiation"
  | otherwise = sq 1 x where
    sq c r =
      let c2 = c * 2
      in if c2 < y then sq c2 (r^2 `mod` m) else (r * exM2 x (y-c) m) `mod` m

prop_exM2IsCorrect :: Integer -> Positive Integer -> NonZero Integer -> Bool
prop_exM2IsCorrect x (Positive y) (NonZero m) = exM2 x y m == x^y `mod` m

-- tested using quickcheck property prop_exM2IsCorrect
-- I think this result is better than the expM function as that one uses `rem` instead of `mod`?

-- Time taken: 1h
