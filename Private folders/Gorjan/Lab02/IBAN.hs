module IBAN where

import Data.Char

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs) = if x==' ' then removeSpaces xs
						else x:removeSpaces xs

tableConvert :: String -> String
tableConvert [] = ""
tableConvert (x:xs) = show ((ord x)-55) ++ tableConvert xs

convertToDigits :: String -> String
convertToDigits [] = []
convertToDigits (x:xs) = if isNumber x then x : convertToDigits xs
							else tableConvert (charToString x) ++ convertToDigits xs

rearrange :: String -> String
rearrange (x:y:z:q:zs) = zs ++ charToString x ++ charToString y ++ charToString z ++ charToString q
-- rearrange (x:y:z:q:zs) = zs ++ tableConvert((charToString x) ++ (charToString y)) ++ (charToString z) ++ (charToString q) 

charToString :: Char -> String
charToString = (:[])

stringToInt :: String -> Integer -> Integer
stringToInt [] _ = 0
stringToInt (x:xs) n = (toInteger(ord x) * n) + stringToInt xs n*10 

doCheck :: String -> Bool
doCheck x = read (convertToDigits(rearrange(removeSpaces x))) `mod` 97 == 1

trueIbans = ["AT61 1904 3002 3457 3201", "FR14 2004 1010 0505 0001 3M02 606", "GL56 0444 9876 5432 10", "MT84 MALT 0110 0001 2345 MTLC AST0 01S"]
falseIbans = ["AT61 1904 3002 34578 3201", "FR14 2004 1010 0505 0001 3M02 6060", "G56 0444 9876 5432 10", "MT84 MALT 0110 0001 2345 MTLC AST 01S"]

checkIbans :: [String] -> Bool
checkIbans [] = True
checkIbans (x:xs) = if doCheck x then checkIbans xs else False

--time needed 30 min