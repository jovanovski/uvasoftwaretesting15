module Lab4Ex3 where

import SetOrd
import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad

instance Foldable Set where
  foldr f s (Set l) = foldr f s l 

intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet _ (Set []) = Set []
intersectSet (Set []) _ = Set []
intersectSet (Set (x:xs)) y = if inSet x y then insertSet x (intersectSet (Set xs) y) else intersectSet (Set xs) y


unionSet2 :: (Ord a) => Set a -> Set a -> Set a
unionSet2 _ (Set []) = Set []
unionSet2 (Set []) y = y
unionSet2 (Set (x:xs)) y = insertSet x (unionSet2 (Set xs) y)


diffSet :: (Ord a) => Set a -> Set a -> Set a
diffSet _ (Set []) = Set []
diffSet (Set []) _ = Set []
diffSet (Set (x:xs)) y = if inSet x y then diffSet (Set xs) y else insertSet x (diffSet (Set xs) y)

---TESTING

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where 
  arbitrary = liftM list2set arbitrary

prop_unionSet2 :: Set Integer -> Set Integer -> Bool
prop_unionSet2 x z = all (\y -> inSet y x || inSet y z) (unionSet2 x z)

prop_intersectSet :: Set Integer -> Set Integer -> Bool
prop_intersectSet x z = all (\y -> inSet y x && inSet y z) (intersectSet x z)

prop_diffSet :: Set Integer -> Set Integer -> Bool
prop_diffSet x z = all (\y -> inSet y x && not (inSet y z)) (diffSet x z)

---

--time spent: 30 min