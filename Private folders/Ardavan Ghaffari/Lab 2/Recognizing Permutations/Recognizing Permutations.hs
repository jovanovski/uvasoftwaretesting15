module RecognizingPermutations where

import Data.List
import System.Random
import Testing

-- Concise test report: I have defined a precondition property for the isPermutation function called preP which
-- is executed before the isPermutation function and simply checks weather the two arguments passed to isPermutation function
-- are of the same length or not. if the arguments don't have the same length then the isPermutation function never gets called.
-- On the other hand if they have the same length then it is the isPermutation's responsibility to determine if they are permutations
-- of one and other. I have tried to automate the test process with the help of predefined "permutations" function in haskell. By calling
-- "automatePermutationTest 1 100" one hundred tests can be executed. In each iteration a random list is generated (let's call this list xs).
-- "permutatoins" function generates all the possible permutations of xs. Next we select one of these permutations randomly. Since we are pretty 
-- sure that the predefined "permutations" function in haskell does not make a mistake in generating permutations of a given list, then we can expect
-- that our tests get passed every single time.


-- The function itself:
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = elem x ys && isPermutation xs (delete x ys)	

-- Precondition property:
preP :: Eq a => [a] -> [a] -> Bool
preP xs ys = length xs == length ys

-- Precondition wrapper:
preW :: ([a]->[a]->Bool) -> ([a]->[a]->Bool) -> [a] -> [a] -> Bool
preW p f xs ys = if p xs ys then f xs ys
					else False

check :: Eq a => [a] -> [a] -> Bool
check = preW preP isPermutation

-----------------------------------------------------------------------------------------------------------------------  							

testIsPermutation :: Eq a => ([a],[a],Bool) -> Bool
testIsPermutation (n,d,b) = check n d == b

manualPermutationTests :: [Test]
manualPermutationTests = [Test "I am testing the isPermutation function manually" testIsPermutation
							[([1,2,3,4,5],[3,5,2,1,4],True),
							([1,2],[2,1],True),
							([1,2,3],[2,3],False),
							([65,39,105],[105,65,39],True),
							([65,39,105,4,5,98],[105,65,39],False),
							([],[3],False)]
						 ]

----------------------------------------------------------------------------------------------------------------------

automatePermutationTest :: Int -> Int -> IO ()
automatePermutationTest k n = if k == n then print ( show n ++ " isPermutation tests passed")
								else do
									xs <- genIntList
									let ps = permutations xs
									randomNum <- getRandomInt (length ps - 1)
									let ys = ps !! randomNum
									if check xs ys then
										do print ("test passed for " ++ show xs ++ " and " ++ show ys)
							   			   automatePermutationTest (k+1) n
									else error ("test failed for " ++ show xs ++ " and " ++ show ys)

-----------------------------------------------------------------------------------------------------------------------

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

									