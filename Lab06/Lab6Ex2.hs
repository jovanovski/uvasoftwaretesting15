module Lab6Ex2 where

import Lab6Ex1
import Test.QuickCheck
import Lecture6

-- Only checking this for positive numbers, as the `rem` implementation differs
-- from the `mod` implementation when doing stuff with negative numbers. But we
-- are not using this for negative numbers anyway.
prop_exM2expMisEquiv :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
prop_exM2expMisEquiv (Positive x) (Positive y) (Positive m) = exM2 x y m == expM x y m

{--
Checked using ghci to time execution and print memory usage:

expM 7 123456789 44
(9.59 s, 123.077.304 b)

exM2 7 123456789 44
(0.12 s, 0 b)

expM 7 12345678901234567890 44
*out of memory*

exM2 7 12345678901234567890 44
(0.02 s, 0 b)

--}

-- Time taken: 30m
