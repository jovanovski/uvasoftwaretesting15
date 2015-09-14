module Ex4 where

import Data.List
import System.Random
import Testing
import Lecture3

aset = ["1", "2", "3", "4"]
bset = ["*", "+", "==>", "<=>"]

testParse :: IO String
testParse = do
	form <- genForm
	res <- ioParse form
	if res then return ("Parsed: " ++ form) else return ("Failed parse on " ++ form)

ioParse :: String -> IO Bool
ioParse xs = do
	let parsed = parse xs
	if parsed /= [] then return True else return False

genForm :: IO String
genForm = do
	left <- genFrmOrLit
	right <- genFrmOrLit
	opnum <- genNumber 3
	if opnum <=1 then do
		let op = bset!!opnum
		return (op++"("++left++" "++right++")")
	else do
		let op = bset!!opnum
		return ("("++left++" "++op++" "++right++")")

genFrmOrLit :: IO String
genFrmOrLit = do
	num <- genNumber 10
	if num <= 6 then do
		lit <- genLiteral
		return lit
	else do
		frm <- genForm
		return frm

genLiteral :: IO String
genLiteral = do
	num <- genNumber 3
	let letter = aset!!num
	sign <- genNumber 1
	if sign==1 then return ("-"++letter) else return letter

genNumber :: Int -> IO Int
genNumber n = do
    rs <- randomRIO (0, n)
    return rs