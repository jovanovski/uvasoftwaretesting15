module Lab4 where

import Lab2
import SetOrd
import System.Random
import Test.QuickCheck
import Control.Monad
import Data.List

-- START random set generator

-- n = number of test cases to generate
genIntSets :: Int -> IO ([Set Int])
genIntSets 0 = return []
genIntSets n = do
  l <- genIntList 20 20
  r <- genIntSets (n-1)  
  return $ (list2set l) : r

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where 
  arbitrary = liftM list2set arbitrary

-- END random set generator 
-- Time taken 30m, tested using `genIntSet 50 50` and `sample $ (arbitrary :: Gen (Set Int))`

-- START set operations

-- union already implemented in SetOrd
prop_UnionContainsAllElements :: Set Int -> Set Int -> Bool
prop_UnionContainsAllElements f@(Set fs) g@(Set gs) = 
  let u = unionSet f g
  in (all (\x -> inSet x u) fs) && (all (\x -> inSet x u) gs)

intersectionSet :: Ord a => Set a -> Set a -> Set a
intersectionSet (Set xs) (Set ys) = list2set $ xs `intersect` ys

differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet (Set xs) (Set ys) = list2set $ xs \\ ys

