module Lab4Ex2 where

import Lab2
import SetOrd
import System.Random
import Test.QuickCheck
import Control.Monad

-- START random set generator

-- n = number of test cases to generate
genIntSet :: IO (Set Int)
genIntSet = do
  l <- genIntList 15 15
  return $ list2set l

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = liftM list2set arbitrary

-- END random set generator
-- Time taken 30m, tested using `genIntSet` and `sample $ (arbitrary :: Gen (Set Int))`
