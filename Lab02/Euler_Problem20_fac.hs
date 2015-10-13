import Data.Char
fact :: Integer -> Integer
fact 0 = 1
fact n = (fact (n-1))*n

-- works only with numbers 0-9
sumOfDigits :: String -> Int
sumOfDigits "" = 0
sumOfDigits (x:xs) = sumOfDigits xs + ((ord x) - 48)

problem20 :: Int
problem20 = sumOfDigits $ show (fact 100)
	
