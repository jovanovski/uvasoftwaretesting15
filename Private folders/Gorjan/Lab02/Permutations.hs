module Permutations where

import Data.List
import System.Random

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = if x `elem` ys then
									isPermutation xs (delete x ys)
								else
									False

getRandomInt :: Integer -> IO Integer
getRandomInt n = getStdRandom (randomR (0,n))

genIntList :: IO [Integer]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Integer -> Integer -> IO [Integer]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   xs <- getIntL k (n-1)
   is <- isPermutation [1,2] [1,2]
   return (x:xs)

--time needed 1h 
--TODO tests