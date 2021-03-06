import Data.List
import Data.Char
import Testing

-- Ex 04
minLength = 1250
maxLength = 32

iban :: String -> Bool
iban s =  step4 $ step3 $ step23 $ step1 $ isIbanForm $ (delSpaces s)

step1 :: String -> String
step1 (a:b:c:d:xs) = xs++a:b:c:d:[]
step1 (xs) = xs


step23 :: String -> String
step23 "" = ""
step23 (x:xs) = (show (convertToInt x))++(step23 xs)

convertToInt:: Char -> Int
convertToInt c  | o < 0 = o + 17	
				| otherwise = o + 10
					where o = ord c - 65

step3 :: String -> Integer
step3 s = read s

step4 :: Integer -> Bool
step4 n = n `mod` 97 == 1

delSpaces :: String -> String
delSpaces "" = ""
delSpaces (' ':xs) = delSpaces xs
delSpaces (x:xs) = x:(delSpaces xs)

inRange :: Integer -> Integer -> Integer -> Bool
inRange l h val | l <= val && val < h = True
				| otherwise = False

inRangeInt :: Int -> Int -> Int -> Bool
inRangeInt l h val | l <= val && val < h = True
				| otherwise = False

isIbanForm :: String -> String
isIbanForm s = if (hasCountryLett s) then (isIbanForm' s 0) else "" 

hasCountryLett :: String -> Bool
hasCountryLett "" = False
hasCountryLett (x:(y:xs)) = (isUpperCase x) && (isUpperCase y)
hasCountryLett (xs) = False


isLowerCase :: Char -> Bool
isLowerCase c = inRangeInt 92 123 (ord c)

isUpperCase :: Char -> Bool
isUpperCase c = inRangeInt 65 92 (ord c)

isIbanForm' :: String -> Integer -> String
isIbanForm' "" len = if inRange minLength maxLength len then "" else ("The lenght of given string is not in a valid range.")
isIbanForm' (x:xs) len	| isAlphaNum x && not(isLowerCase x) = x:(isIbanForm' xs (len+1))
						| otherwise = error (x:" is not acceptable IBAN character.")


--TESTING
assertBool :: (String, Bool) -> Bool
assertBool (s, b) = iban s == b

test' :: IO String
test' = do
		let t = test
		let res = "res"
		print("finish")
		return res


test :: [Test]
test = [ Test "testing ibans " assertBool
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
				("AE07 0331 2345 6789 0123 4564 4458 4557", False)
				] 
		    ]

{--
-- estimation time 1,5h, real time: 2h
--}