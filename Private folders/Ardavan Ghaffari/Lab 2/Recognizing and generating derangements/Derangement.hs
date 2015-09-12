module Derangement where 

import Data.List
import System.Random

isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys = if isPermutation xs ys then all(\n -> (ys !! n) /= n+1) [0..(length ys-1)] else False

-- My own definition of generating derangements:
deran :: [Int] -> [[Int]]
deran xs = [ ys | ys <- perms xs, isDerangement xs ys]

-- function below generates all the derangements of a given list of Integers. One of our group members found this function online.
-- During the test process, we assume that this function works fine.
derangements :: [Int] -> [[Int]]
derangements xs = filter (and . zipWith (/=) xs) $ permutations xs

-----------------------------------------------------------------------------------------------------------------

testIsDerangement :: ([Int],[Int],Bool) -> Bool
testIsDerangement (n,d,b) = isDerangement n d == b

--manualDerangementTest :: [Test]
--manualDerangementTest = [Test "I am testing the isDerangement function manually" testIsDerangement]

-----------------------------------------------------------------------------------------------------------------

automateDerangementTest :: Int -> Int -> IO ()
automateDerangementTest k n = if k == n then print ( show n ++ " isDerangement tests passed")
								else do
									xs <- genIntList
									let ds = derangements xs
									randomNum <- getRandomInt (length ds - 1)
									let ys = ds !! randomNum
									if isDerangement xs ys then
										do print ("test passed for " ++ show xs ++ " and" ++ show ys)
										   automateDerangementTest (k+1) n
									else error ("test failed for " ++ show xs ++ " and " ++ show ys)

-----------------------------------------------------------------------------------------------------------------

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = elem x ys && isPermutation xs (delete x ys)

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))									

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 7
  getIntL k n
 
getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <- getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs) 

