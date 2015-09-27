module Lab4 where

import SetOrd
import System.Random
import Test.QuickCheck
import Data.List

--Exercise #2

rndSetGenerator :: IO (Set Int)
rndSetGenerator = do
	xs <- genIntList
	return (list2set xs)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
	arbitrary = do
		a <- arbitrary
		return (list2set a)

prop_IsOrdered :: Set Int -> Bool
prop_IsOrdered (Set []) = True
prop_IsOrdered (Set (x:xs)) = all (>= x) xs && prop_IsOrdered (Set xs)

prop_IsDuplicateFree :: Set Int -> Bool
prop_IsDuplicateFree (Set []) = True
prop_IsDuplicateFree (Set (x:xs)) = not(elem x xs) && prop_IsDuplicateFree (Set xs)

testSetQC = quickCheckResult (\s -> prop_IsOrdered s && prop_IsDuplicateFree s)

-------------------------------------------------------------------------------------------------------------------

--Exercise #3

--Function Definitions, the definition for union was already provided in SetOrd.hs
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set []) (Set []) = Set []
intersection (Set []) s = Set []
intersection (Set (x:xs)) s = if inSet x s then insertSet x (intersection (Set xs) s)
                                else intersection (Set xs) s

difference :: (Ord a) => Set a -> Set a -> Set a
difference s (Set []) = s
difference s (Set (x:xs)) = difference (deleteSet x s) (Set xs)                                

--Testable Properties
prop_Union1 :: Set Int -> Set Int -> Bool
prop_Union1 s1 s2 = subSet s1 u && subSet s2 u
                    where u = unionSet s1 s2

prop_Union2 :: Set Int -> Set Int -> Bool
prop_Union2 s1 s2 = all (\x -> inSet x s1 || inSet x s2) u
                      where u = unionSet s1 s2

prop_Intersection :: Set Int -> Set Int -> Bool
prop_Intersection s1 s2 = all (\x -> inSet x s1 && inSet x s2) i
                            where i = intersection s1 s2                            

prop_Difference :: Set Int -> Set Int -> Bool
prop_Difference s1 s2 = all (\x -> inSet x s1 && not(inSet x s2)) d
                          where d = difference s1 s2

--Test with QuickCheck
testUnionQC = quickCheckResult (\ s1 s2 -> prop_Union1 s1 s2 && prop_Union2 s1 s2) 
testIntersectionQC = quickCheckResult (\ s1 s2 -> prop_Intersection s1 s2)
testDifferenceQC = quickCheckResult (\ s1 s2 -> prop_Difference s1 s2)

--Test with own generator
testWOG1 :: Int -> Int -> (Set Int -> Set Int -> Bool) -> IO ()
testWOG1 k n p = if k == n then print( show n ++ " tests passed successfully!")
                  else do
                    s1 <- rndSetGenerator
                    s2 <- rndSetGenerator
                    if p s1 s2 then do
                      print ("test passed for " ++ show s1 ++ " and " ++ show s2)
                      testWOG1 (k+1) n p
                    else error ("test failed for " ++ show s1 ++ " and " ++ show s2)

testUnionWOG = testWOG1 1 100 (\ s1 s2 -> prop_Union1 s1 s2 && prop_Union2 s1 s2)
testIntersectionWOG = testWOG1 1 100 prop_Intersection
testDifferenceWOG = testWOG1 1 100 prop_Difference                    

-------------------------------------------------------------------------------------------------------------------

--Exercise #5

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos r = sort(union r r') where r' = [ (x,y) | (y,x) <- r]

-------------------------------------------------------------------------------------------------------------------

--Exercise #6

--fixpoint operation retrieved from the lecture
fp :: Eq a => (a -> a) -> a -> a 
fp f = until (\ x -> x == f x) f

infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r = fp (\x -> sort( (x @@ x) `union` x)) r

-------------------------------------------------------------------------------------------------------------------

--Exercise #7

--Some testable properties for symmetric and transitive closure functions:
prop_IsSymmetry :: (Eq a) => Rel a -> Bool
prop_IsSymmetry s = all (\ (x,y) -> (y,x) `elem` s) s

prop_IsTransitive :: (Eq a) => Rel a -> Bool
prop_IsTransitive s = all (\ (x,y) -> all (\ (w,z) -> y /= w || (x,z) `elem` s) s) s 

--Test with own generator
testWOG2 :: Int -> Int -> (Rel Int -> Rel Int) -> (Rel Int -> Bool) -> IO ()
testWOG2 k n f p = if k == n then print( show n ++ " tests passed successfully!")
                  else do
                    r <- rndRelGenerator 6
                    let r' = f(sort(nub(r)))
                    if p r' then do
                      print ("test passed for " ++ show r ++ " and it's closure" ++ show r')
                      testWOG2 (k+1) n f p
                    else error ("test failed for " ++ show r ++ " and it's closure" ++ show r')

testSymClos = testWOG2 1 100 symClos prop_IsSymmetry
testTrClos = testWOG2 1 100 trClos prop_IsTransitive

--A random relation generator
rndRelGenerator :: Int -> IO (Rel Int)
rndRelGenerator 0 = return []
rndRelGenerator n = do
  a <- getRandomInt 6
  b <- getRandomInt 6
  rs <- rndRelGenerator (n-1)
  return ((a,b):rs)

-------------------------------------------------------------------------------------------------------------------

--Exercise #8

--yes there is a difference between the two. the following counter example proves that the order of applying closure
--to a relation is important:

--for instance let R be [(1,2)]
--then the symmetric closure of the transitive closure of R is : [(1,2),(2,1)]
--and the transitive closure of the symmetric closure of R is : [(1,1),(1,2),(2,1),(2,2)]

--------------------------------------------------------------------------------------------------------------------

--Some Helper Functions:
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 50
  n <- getRandomInt 20
  getIntL k n
 
getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)