{-# OPTIONS_GHC -Wall #-}
module HW01 where

import Data.Char

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = toInteger (digitToInt (last (show n)))

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = if n >= 10 then read (take ((length (show n))-1) (show n)) else 0

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n = if n <= 0 then [] else map (toInteger . digitToInt) (reverse (show n))

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x:(2*y):(doubleEveryOther zs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits n = toInteger (sum (map (sum . (map digitToInt) . show) n))

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = rem (sumDigits (doubleEveryOther (toRevDigits n))) 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
