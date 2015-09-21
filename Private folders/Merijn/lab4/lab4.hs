module Lab4 where

import Lab2
import SetOrd
import System.Random
import Test.QuickCheck
import Control.Monad
import Data.List
-- import Data.Function -- using fix function definition from lecture
import Lecture4

-- START random set generator

-- n = number of test cases to generate
genIntSet :: IO (Set Int)
genIntSet = do
  l <- genIntList 15 15  
  return $ list2set l

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where 
  arbitrary = do
    a <- arbitrary
    return $ list2set a

-- END random set generator 
-- Time taken 30m, tested using `genIntSet 50 50` and `sample $ (arbitrary :: Gen (Set Int))`

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

-- START symmetric closure

type Rel a = [(a, a)]

symClos :: Ord a => Rel a -> Rel a
symClos rs = sort $ nub $ symClos' rs

symClos' :: Ord a => Rel a -> Rel a
symClos' [] = []
symClos' ((a,b):rs) = (a,b):(b,a):(symClos' rs)

-- END symClos
-- Time taken 15m

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos rs = fix (\f rs -> let cs = sort $ nub (rs ++ (rs @@ rs)) in if rs == cs then rs else f cs) rs

-- END trClos,
-- Time taken 1h

-- START Testing symClos & trClos

prop_SymClosIsSymmetric :: Rel Int -> Bool
prop_SymClosIsSymmetric r = let c = symClos r in all (\(a,b) -> (b,a) `elem` c) c

prop_SymClosContainsOriginal :: Rel Int -> Bool
prop_SymClosContainsOriginal r = let c = symClos r in all (\x -> x `elem` c) r

prop_TrClosIsTransitive :: Rel Int -> Bool
prop_TrClosIsTransitive r = let c = trClos r in all (\v -> v `elem` c) [((x,u)) | (x,y) <- c, (z,u) <- c, y == z]

prop_TrClosContainsOriginal :: Rel Int -> Bool
prop_TrClosContainsOriginal r = let c = trClos r in all (\x -> x `elem` c) r

-- END Testing symClos & trClos
-- Time taken 20m - tested uing quickcheck

-- START sym tr clos is tr sym clos 

prop_SymTrClosIsTrSymClos :: Rel Int -> Bool
prop_SymTrClosIsTrSymClos r = let stc = symClos $ trClos r; tsc = trClos $ symClos r in stc == tsc

{-
They are NOT the same - the transitive closure of a symmetric closure is also reflexive, while a symmetric 
closure of a transitive closure does not need to be so (because of isolated points). An example would be: 

  [(1,0)]

When first taking the transitive closure and then the symmetric closure, this would lead to:

  [(1,0)] -> [(1,0)] -> [(0,1),(1,0)]

While first taking the symmetric closure and then the transitive closure leads to:

  [(1,0)] -> [(0,1),(1,0)] -> [(0,0),(0,1),(1,0),(1,1)]
-}

-- END sym tr clos is tr sym clos
-- Time taken 30m 

-- START Bonus - show Function

arbI :: Gen Expr
arbI = liftM I (resize 5 arbitrary)

arbV :: [String] -> Gen Expr
arbV vs = liftM V (elements vs)

arbExpr :: Int -> [String] -> Gen Expr
arbExpr 0 [] = arbI
arbExpr 0 vs = oneof [arbI, arbV vs]
arbExpr n vs = 
  case n of 
    0 -> case vs of 
      [] -> arbI
      vs -> oneof [arbI, arbV vs]
    n -> case vs of 
      [] -> oneof [
          arbI,
          liftM2 Add subExpr subExpr,
          liftM2 Subtr subExpr subExpr,
          liftM2 Mult subExpr subExpr
        ]
      vs ->  oneof [
          arbI,
          arbV vs,
          liftM2 Add subExpr subExpr,
          liftM2 Subtr subExpr subExpr,
          liftM2 Mult subExpr subExpr
        ]
  where 
    subExpr = arbExpr (n `div` 2) vs

arbCondition :: Int -> [String] -> Gen Condition
arbCondition n vs = case n of 
  0 -> case vs of 
    [] -> oneof [
        liftM2 Eq subExpr subExpr,
        liftM2 Lt subExpr subExpr,
        liftM2 Gt subExpr subExpr
      ]
    vs -> oneof [
        liftM Prp (elements vs),
        liftM2 Eq subExpr subExpr,
        liftM2 Lt subExpr subExpr,
        liftM2 Gt subExpr subExpr
      ]
  n -> case vs of
    [] -> oneof [
        liftM2 Eq subExpr subExpr,
        liftM2 Lt subExpr subExpr,
        liftM2 Gt subExpr subExpr,
        liftM Ng subCondition,
        liftM Cj (listOf subCondition),
        liftM Dj (listOf subCondition)
      ]
    vs -> oneof [
        liftM Prp (elements vs),
        liftM2 Eq subExpr subExpr,
        liftM2 Lt subExpr subExpr,
        liftM2 Gt subExpr subExpr,
        liftM Ng subCondition,
        liftM Cj (listOf subCondition),
        liftM Dj (listOf subCondition)
      ] 
  where 
    subExpr = arbExpr (n `div` 2) vs
    subCondition = arbCondition (n `div` 2) vs

--arbStatement :: Int -> Gen Statement
--arbStatement n = case n of 
--  0 -> oneof [
--      abrAss,
--      liftM3 Cond arbitrary subStatement subStatement,
--      liftM Seq (listOf subStatement),
--      liftM2 While arbitrary subStatement
--    ] 
--  n -> oneof [
--      abrAss,
--      liftM3 Cond arbitrary subStatement subStatement,
--      liftM Seq (listOf subStatement),
--      liftM2 While arbitrary subStatement
--    ] 
--  where 
--    subStatement = arbStatement (n `div` 2)
--    subCondition = arbCondition (n `div` 2)

-- END Bonus - show function