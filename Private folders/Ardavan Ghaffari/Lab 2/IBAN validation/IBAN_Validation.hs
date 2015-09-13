module IBAN_Validation where

import Data.Char
import Data.List
import System.Random
import Testing

--Concise test report: I have defined four precondition properties for the iban function. These properties check the input to be within some constraints before
--handing it over to iban function. So for example if the input does not conform to standard length, one of these properties (preP1) detects
--it and returns False. In such cases the iban function never gets called. I was not able to randomly generate iban numbers therefore i couldn't 
--automate the test process.	

iban :: String -> Bool
iban x = read(convertLetterToNum(move4Chars x)) `mod` 97 == 1

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs) = if isSpace x then removeSpaces xs else x : removeSpaces xs

move4Chars :: String -> String
move4Chars xs = drop 4 xs ++ take 4 xs

convertLetterToNum :: String -> String
convertLetterToNum [] = []
convertLetterToNum (x:xs) = if isDigit x then x : convertLetterToNum xs else show((ord x) - 55) ++ convertLetterToNum xs

----------------------------------------------------------------------------------------------------------------------------

--Precondition Properties:

--iban length should be less than 35 characters
preP1 :: String -> Bool
preP1 s = if length s < 35 then True else False

--iban should only contain alphanumeric characters
preP2 :: String -> Bool
preP2 [] = True
preP2 (s:ss) = isAlphaNum s && preP2 ss

--alphabetic characters should all be uppercase
preP3 :: String -> Bool
preP3 s = and(map isUpper (filter isAlpha s))

--the first two characters in iban should always be alphabetic characters
preP4 :: String -> Bool
preP4 (a:b:ss) = isAlpha a && isAlpha b

--Precondition Wrapper
preW :: (String -> Bool) ->
		 (String -> Bool) ->
		  (String -> Bool) ->
		   (String -> Bool) ->
		    (String -> Bool) ->
		      String -> Bool
preW p1 p2 p3 p4 f s = if p1 s && p2 s && p3 s && p4 s then f s else False

check :: String -> Bool
check = preW preP1 preP2 preP3 preP4 iban

----------------------------------------------------------------------------------------------------------------------------

testIban :: (String,Bool) -> Bool
testIban (n,d) = check(removeSpaces n) == d

manualIbanTests :: [Test]	      
manualIbanTests = [Test "I am manually testing the iban function" testIban
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
					("AE07 0331 2345 6789 0123 4564 4458 4557", False)]
				]
