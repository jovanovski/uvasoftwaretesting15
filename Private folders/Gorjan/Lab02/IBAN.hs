module IBAN where

import Data.Char
import Testing

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

charToString :: Char -> String
charToString = (:[])

stringToInt :: String -> Integer -> Integer
stringToInt [] _ = 0
stringToInt (x:xs) n = (toInteger(ord x) * n) + stringToInt xs n*10 

----- prechecks

checkAN :: String -> Bool
checkAN [] = True
checkAN (x:xs) = if isAlphaNum x then checkAN xs else False

checkLenght :: String -> Bool
checkLenght [] = False
checkLenght xs = if length(xs)>= 15 && length(xs)<=31 then True else False

checkStart :: String -> Bool
checkStart [] = False
checkStart (x:y:zs) = if isAlpha x && isAlpha y then True else False

checkAllUpper :: String -> Bool
checkAllUpper [] = True
checkAllUpper (x:xs) = if (isAlpha x && (ord x) >= 65 && (ord x) <= 90) || isNumber x then checkAllUpper xs else False

isOKInput :: String -> Bool
isOKInput [] = False
isOKInput x = if checkAllUpper x && checkStart x && checkAN x && checkLenght x then True else False


--- TESTING

doCheck :: String -> Bool
doCheck x = if (isOKInput (removeSpaces x)) && (read (convertToDigits(rearrange (removeSpaces x))) `mod` 97 == 1) then True else False

testFunctions :: (String, Bool) -> Bool
testFunctions (x, b) = doCheck x == b

ibanTests :: [Test]
ibanTests = [ Test "testing ibans " testFunctions
             [("AL47 2121 1009 0000 0002 3569 8741", True),
				("AD12 0001 2030 2003 5910 0100", True),
				("AT61 1904 3002 3457 3201", True),
				("AZ21 NABZ 0000 0000 1370 1000 1944", True),
				("BH67 BMAG 0000 1299 1234 56", True),
				("BE62 5100 0754 7061", True),
				("BA39 1290 0794 0102 8494", True),
				("BG80 BNBG 9661 1020 3456 78", True),
				("HR12 1001 0051 8630 0016 0", True),
				("CY17 0020 0128 0000 0012 0052 7600", True),
				("CZ65 0800 0000 1920 0014 5399", True),
				("DK50 0040 0440 1162 43", True),
				("EE38 2200 2210 2014 5685", True),
				("FO97 5432 0388 8999 44", True),
				("FI21 1234 5600 0007 85", True),
				("FR14 2004 1010 0505 0001 3M02 606", True),
				("GE29 NB00 0000 0101 9049 17", True),
				("DE89 3704 0044 0532 0130 00", True),
				("GI75 NWBK 0000 0000 7099 453", True),
				("GR16 0110 1250 0000 0001 2300 695", True),
				("GL56 0444 9876 5432 10", True),
				("HU42 1177 3016 1111 1018 0000 0000", True),
				("IS14 0159 2600 7654 5510 7303 39", True),
				("IE29 AIBK 9311 5212 3456 78", True),
				("IL62 0108 0000 0009 9999 999", True),
				("IT40 S054 2811 1010 0000 0123 456", True),
				("JO94 CBJO 0010 0000 0000 0131 0003 02", True),
				("KW81 CBKU 0000 0000 0000 1234 5601 01", True),
				("LV80 BANK 0000 4351 9500 1", True),
				("LB62 0999 0000 0001 0019 0122 9114", True),
				("LI21 0881 0000 2324 013A A", True),
				("LT12 1000 0111 0100 1000", True),
				("LU28 0019 4006 4475 0000", True),
				("MK072 5012 0000 0589 84", True),
				("MT84 MALT 0110 0001 2345 MTLC AST0 01S", True),
				("MU17 BOMM 0101 1010 3030 0200 000M UR", True),
				("MD24 AG00 0225 1000 1310 4168", True),
				("MC93 2005 2222 1001 1223 3M44 555", True),
				("ME25 5050 0001 2345 6789 51", True),
				("NL39 RABO 0300 0652 64", True),
				("NO93 8601 1117 947", True),
				("PK36 SCBL 0000 0011 2345 6702", True),
				("PL60 1020 1026 0000 0422 7020 1111", True),
				("PT50 0002 0123 1234 5678 9015 4", True),
				("QA58 DOHB 0000 1234 5678 90AB CDEF G", True),
				("RO49 AAAA 1B31 0075 9384 0000", True),
				("SM86 U032 2509 8000 0000 0270 100", True),
				("SA03 8000 0000 6080 1016 7519", True),
				("RS35 2600 0560 1001 6113 79", True),
				("SK31 1200 0000 1987 4263 7541", True),
				("SI56 1910 0000 0123 438", True),
				("ES80 2310 0001 1800 0001 2345", True),
				("SE35 5000 0000 0549 1000 0003", True),
				("CH93 0076 2011 6238 5295 7", True),
				("TN59 1000 6035 1835 9847 8831", True),
				("TR33 0006 1005 1978 6457 8413 26", True),
				("AE07 0331 2345 6789 0123 456", True),
				("GB29 RBOS 6016 1331 9268 19", False),
				("A07 0331 2345 6789 0123 456", False),
				("AAE07 0331 2345 6789 0123 456", False),
				("AE07 0331 2345 6789 0123 4564 4458 4557", False),
				("AE07 033", False)]
				           ]


--time needed 2h