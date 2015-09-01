module Euler where

primesbelow :: Integer -> [Integer]
primesbelow n = sievebelow [2..] where 
  sievebelow (x:xs) = if x^2 > n then x:takeWhile (< n) xs else x:(sievebelow [y|y <- xs, y `mod` x /= 0])

sol10 :: Integer
sol10 = sum (primesbelow 2000000)