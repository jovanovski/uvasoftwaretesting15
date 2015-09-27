module Lab4Ex3 where

import SetOrd
import Test.QuickCheck
import Data.List
import Lab4Ex2

-- START set operations

-- unionSet already implemented in SetOrd

testSetProp2 :: (Set Int -> Set Int -> Bool) -> Int -> IO()
testSetProp2 _ 0 = print "All passed"
testSetProp2 p n = do
  f <- genIntSet
  g <- genIntSet
  let s = (show f) ++ " and " ++ (show g)
  if p f g then do
    print $ "Pass on " ++ s
    testSetProp2 p (n-1)
  else error $ "Failed on " ++ s

instance Foldable (Set) where
  foldr f s (Set l) = foldr f s l

prop_UnionHasAllElements :: Set Int -> Set Int -> Bool
prop_UnionHasAllElements f g =
  let u = unionSet f g
  in (all (\x -> inSet x u) f) && (all (\x -> inSet x u) g)

prop_UnionHasOnlyElements :: Set Int -> Set Int -> Bool
prop_UnionHasOnlyElements f g =
  let u = unionSet f g
  in all (\x -> (x `elem` f) || (x `elem` g)) u

-- test using own set generator
testUnionSet :: Int -> IO ()
testUnionSet n = testSetProp2 (\f g -> (prop_UnionHasOnlyElements f g) && (prop_UnionHasAllElements f g)) n

intersectionSet :: Ord a => Set a -> Set a -> Set a
intersectionSet (Set xs) (Set ys) = list2set $ xs `intersect` ys

prop_IntersectionElementsInBoth :: Set Int -> Set Int -> Bool
prop_IntersectionElementsInBoth f g =
  let i = intersectionSet f g
  in all (\x -> (inSet x f) && (inSet x g)) i

prop_IntersectionHasAllElements :: Set Int -> Set Int -> Bool
prop_IntersectionHasAllElements f g =
  let i = intersectionSet f g
  in all (\x -> if x `elem` g then inSet x i else True) f

testIntersectionSet :: Int -> IO ()
testIntersectionSet n = testSetProp2 (\f g -> (prop_IntersectionElementsInBoth f g) && (prop_IntersectionHasAllElements f g)) n

differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet (Set xs) (Set ys) = list2set $ xs \\ ys

prop_DifferenceElementsNotInOther :: Set Int -> Set Int -> Bool
prop_DifferenceElementsNotInOther f g =
  let d = differenceSet f g
  in all (\x -> not (inSet x g)) d

prop_DifferenceHasAllElements :: Set Int -> Set Int -> Bool
prop_DifferenceHasAllElements f g =
  let d = differenceSet f g
  in all (\x -> if not (x `elem` g) then inSet x d else True) f

testDifferenceSet :: Int -> IO ()
testDifferenceSet n = testSetProp2 (\f g -> (prop_DifferenceElementsNotInOther f g) && (prop_DifferenceHasAllElements f g)) n

-- END set operations
-- Time taken 1.5h.
-- Tested with own generator and `testUnionSet`, `testIntersectionSet` & `testDifferenceSet`
-- Tested with QuickCheck using properties `prop_UnionHasAllElements`, `prop_UnionHasOnlyElements`, `prop_IntersectionElementsInBoth`, `prop_IntersectionHasAllElements`, `prop_DifferenceElementsNotInOther` & `prop_DifferenceHasAllElements`
