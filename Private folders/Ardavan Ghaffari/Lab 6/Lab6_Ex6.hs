module Lab6_Ex6 where

import Lecture6
import Lab6_Ex5

test_primeMR :: Int -> Int -> [Integer] -> IO ()
test_primeMR n k (x:xs) = if n == 0 then print ("finished testing carmichael numbers")
							else do
							value <- primeMR k x
							if value then print ("Miller-Rabin primality test says " ++ show x ++ " which is a carmichael number is prime")
								else print ("Miller-Rabin primality test says " ++ show x ++ " which is a carmichael number is composite")
							test_primeMR (n-1) k xs

-- when k is set to 1 or 2, there are some carmichael numbers that can fool the Miller-rabin primality test
-- but when i set k to 3, the miller-rabin test is always working correctly and makes no mistakes in clasifying composite numbers.
-- overall the Miller-Rabing primality test is stronger than fermat's test.							