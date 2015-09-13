module Derangement where 

import Data.List
import System.Random
import Testing

--concice test report: We came to understand that it is not useful to test the isDerangement function with the help of deran function.
-- Because it's basically using the isDerangement function itself. Therefore we had to find another way for generating derangements other than 
--our own deran function. By looking at the Internet one of our group members stumbled upon the derangements function which can be
--viewed below. Now that we have this function, we can use it to test our own definition of isDerangement function. I have managed to automate the
--test process using the techniques from the lecture. In every iteration of our test function, a list of integers is being generated
--randomly(let's call it xs). by calling the derangements function all the possible derangements of xs is populated and assigned to a 
--varialbe called ds. one of the derangements is randomly selected and assigned to a variable called ys. At the end we determine if ys
--is a derangement of xs or not. We have inspected the derangements function for accuracy and we are pretty sure that it works fine.
--Therefore we expect that all the tests get passed.

isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys = isPermutation xs ys && isDerangementHelper xs ys

isDerangementHelper :: [Int] -> [Int] -> Bool
isDerangementHelper [] [] = True
isDerangementHelper [] _ = False
isDerangementHelper _ [] = False
isDerangementHelper (x:xs) (y:ys) = x /= y && isDerangementHelper xs ys

-- My own definition of generating derangements:
deran :: [Int] -> [[Int]]
deran xs = [ ys | ys <- perms xs, isDerangementHelper xs ys]

-- function below generates all the derangements of a given list of Integers. One of our group members found this function online.
-- During the test process, we assume that this function works fine.
derangements :: [Int] -> [[Int]]
derangements xs = filter (and . zipWith (/=) xs) $ permutations xs

-----------------------------------------------------------------------------------------------------------------

testIsDerangement :: ([Int],[Int],Bool) -> Bool
testIsDerangement (n,d,b) = isDerangement n d == b

manualDerangementTest :: [Test]
manualDerangementTest = [Test "I am testing the isDerangement function manually" testIsDerangement
                          [([], [], True),
                          ([1], [1], False),
                          ([1,2,3], [2,3,1], True),
                          ([1,2,3], [3,2,1], False),
                          ([1,2], [1,2,2], False),
                          ([1,2,2], [1,2], False),
                          ([2,2,4,4], [4,4,2,2], True),
                          ([], [1,2], False),
                          ([1,2], [], False)]
                        ]

-----------------------------------------------------------------------------------------------------------------

automateDerangementTest :: Int -> Int -> IO ()
automateDerangementTest k n = if k == n then print ( show n ++ " isDerangement tests passed")
                								else do
                  									xs <- genIntList
                  									let ds = derangements xs
                  									randomNum <- getRandomInt (length ds - 1)
                  									let ys = ds !! randomNum
                  									if isDerangement xs ys then
                  										do print ("test passed for " ++ show xs ++ " and " ++ show ys)
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

