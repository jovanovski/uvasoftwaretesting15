module Permutations where

import Data.List
import System.Random
import Testing

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = if x `elem` ys then
									isPermutation xs (delete x ys)
								else
									False

isPermutation2 :: Eq a => [a] -> [a] -> Bool
isPermutation2 [] [] = True
isPermutation2 [] _ = False
isPermutation2 _ [] = False
isPermutation2 xs ys = xs `elem` (permutations ys) 


--- prechecks

checkLength :: [Integer] -> [Integer] -> Bool
checkLength [] [] = True
checkLength xs ys = length xs == length ys

--- TESTING

runMyTest :: IO String
runMyTest = do
	rn <- randomRIO (2,8)
	print ("Random selected list length: " ++ (show rn))
	rnElem <- randomRIO (0, (rn-1))
	rnd <- generateRndList rn 
	print ("Random generated list: " ++ (show rnd))
	let perms = permutations rnd
	print ("Random selected permutation: " ++ show (perms!!rnElem))
	let result = isPermutation rnd (perms!!rnElem)
	let result2 = isPermutation2 rnd (perms!!rnElem)
	let final = result == result2
	let output = "Do they match: " ++ (show final)
	return output

generateRndList :: Int -> IO [Int]
generateRndList 0 = return []
generateRndList n = do
    rs <- randomRIO (-100, 100)
    perms <- generateRndList (n-1)
    return (rs:perms)

--time needed 2h 
--couldn't use Test syntaxt because of IO bool return type
--testreport:
--*Permutations> runMyTest
--"Random selected list length: 8"
--"Random generated list: [-90,-4,54,-100,20,-39,72,-84]"
--"Random selected permutation: [54,-100,-4,-90,20,-39,72,-84]"
--"Do they match: True"
--*Permutations> runMyTest
--"Random selected list length: 4"
--"Random generated list: [17,-41,-40,74]"
--"Random selected permutation: [-41,17,-40,74]"
--"Do they match: True"
--*Permutations> runMyTest
--"Random selected list length: 4"
--"Random generated list: [17,-41,-40,74]"
--"Random selected permutation: [-41,17,-40,74]"
--"Do they match: True"