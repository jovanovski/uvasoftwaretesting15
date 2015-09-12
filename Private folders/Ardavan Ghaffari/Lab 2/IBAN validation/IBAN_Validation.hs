module IBAN_Validation where

import Data.Char
import Data.List
import System.Random

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs) = if isSpace x then removeSpaces xs else x : removeSpaces xs

move4Chars :: String -> String
move4Chars xs = drop 4 xs ++ take 4 xs

convertLetterToNum :: String -> String
convertLetterToNum [] = []
convertLetterToNum (x:xs) = if isDigit x then x : convertLetterToNum xs else show((ord x) - 55) ++ convertLetterToNum xs

iban :: String -> Bool
iban x = read(convertLetterToNum(move4Chars(removeSpaces x))) `mod` 97 == 1