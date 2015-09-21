module Ex4 where

import Data.List
import System.Random
import Testing
import Lecture3
import Ex1
import Ex3

aset = ["1", "2", "3", "4"]
bset = ["*", "+", "==>", "<=>"]
depthlimit = 10	 

-- | Call this function to test CNF
ioCnfTest :: IO Bool
ioCnfTest = do
	generatedForm <- generateForm
	print ("Testing generated form: " ++ generatedForm)
	let gencnf = cnf generatedForm
	let checkcnf = isCnf gencnf
	print ("Checking if is CNF...: " ++ (show checkcnf))
	if (show gencnf) == "*()" then 
		print ("Generation CNF:" ++ (show gencnf) ++ " - this is a tautology!") 
		else
		print ("Generating CNF: " ++ (show gencnf))
	let parsed = parse generatedForm
	print ("Checking if CNF and parsed original are equivilent...")
	-- | Testable property: generated cnf is equivilant to parsed formula
	let output = equiv gencnf (parsed!!0)
	return output

-- | Testable property
isCnf :: Form -> Bool
isCnf (Cnj xs) = all isDsj xs
isCnf _ = False

isDsj :: Form -> Bool
isDsj (Dsj xs) = all isLiteral xs
isDsj (Prop _) = True
isDsj (Neg (Prop _)) = True
isDsj _ = False

isLiteral :: Form -> Bool
isLiteral (Prop _) = True
isLiteral (Neg (Prop _)) = True
isLiteral _ = False


-- | Call this function to test PARSE
testParse :: IO String
testParse = do
	form <- generateForm
	res <- ioParse form
	if res then return ("OK, parsed: " ++ form) else return ("Failed parse on " ++ form)

ioParse :: String -> IO Bool
ioParse xs = do
	let parsed = parse xs
	print (show parsed)
	if parsed /= [] && show(parsed!!0) == xs then return True else return False

generateForm :: IO String
generateForm = genForm 0

genForm :: Integer -> IO String
genForm dep = do
	left <- genFrmOrLit dep
	right <- genFrmOrLit dep
	opnum <- genNumber 3
	if opnum <=1 then do
		let op = bset!!opnum
		return (op++"("++left++" "++right++")")
	else do
		let op = bset!!opnum
		return ("("++left++""++op++""++right++")")

genFrmOrLit :: Integer -> IO String
genFrmOrLit dep = do
	num <- genNumber 10
	if dep == depthlimit || num <= 4 then do
		lit <- genLiteral
		return lit
	else do
		frm <- genForm (dep+1)
		return frm

genLiteral :: IO String
genLiteral = do
	num <- genNumber (length aset)
	let letter = aset!!num
	sign <- genNumber 1
	if sign==1 then return ("-"++letter) else return letter

genNumber :: Int -> IO Int
genNumber n = do
    rs <- randomRIO (0, n-1)
    return rs

-- | Time spent: 2h