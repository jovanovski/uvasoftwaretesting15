module Lab04 

where

import SetOrd
import Data.List
import System.Random
import Test.QuickCheck 

-- ex 02


-- random number generator
{--
genSet :: Int -> IO (Set Int)
genSet 0 = return (Set [])
genSet n = do
    x <- genInt 100
    let xs  = genSet (n-1)
    set <- insertSetIO x xs
    return set
--}
randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- genInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- genInt 20
  n <- genInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  genInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

insertAllSet :: [Int] -> Set Int
insertAllSet [] = Set []
insertAllSet (x:xs) = insertSet x (insertAllSet xs)

 
genSet :: IO (Set Int)
genSet = do
	list <- genIntList
	let set = insertAllSet list
	return set

genInt :: Int -> IO Int
genInt n = do
    rs <- randomRIO (0, n)
    return rs

genSetQC :: Gen (Set Int)
genSetQC = do
 	val <- arbitrary
 	return val 	

-- QuickCheck
 --instance Arbitrary Set Int where
 --   arbitrary = oneof [powerSet [0..5]]

{--
unionSet :: (Ord a) => Set a -> Set a -> Set a 
unionSet (Set [])     set2  =  set2
unionSet (Set (x:xs)) set2  = 
   insertSet x (unionSet (Set xs) set2)
--}

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where 
  arbitrary = do
  	a <-arbitrary
  	return $ list2set a 
 

{-- --}
-- Ex 03
--Union is already defined in SetOrd.hs

-- Intersection
interSet :: (Ord a) => (Set a) -> (Set a) -> (Set a)
interSet (Set []) _ = (Set [])
interSet (Set (x:xs)) set2 | inSet x set2 = (insertSet x (interSet (Set xs) set2))
						   | otherwise = interSet (Set xs) set2 
-- Difference of sets
diffSet :: (Ord a) => (Set a) -> (Set a) -> (Set a)
diffSet (Set []) _ = (Set [])
diffSet (Set (x:xs)) set2 | inSet x set2 = diffSet (Set xs) set2 
						   | otherwise = (insertSet x (diffSet (Set xs) set2))



prop_UnionContainsAllElements :: Set Int -> Set Int -> Bool
prop_UnionContainsAllElements f@(Set fs) g@(Set gs) = 
  let u = unionSet f g
  in (all (\x -> inSet x u) fs) || (all (\x -> inSet x u) gs)
	
-- for all x in Set1 and all x Set2 -> x in Set1 intersection Set2	
prop_Intersection :: Set Int -> Set Int -> Bool
prop_Intersection f@(Set fs) g@(Set gs) = 
  let u = interSet f g
  in (all (\x -> inSet x u) fs) && (all (\x -> inSet x u) gs)


--ex04
type Rel a = [(a,a)] 

extractList :: Set a -> [a]
extractList (Set []) = []
extractList (Set xs) = xs

insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of 
                                 GT -> y : insertList x ys' 
                                 EQ -> ys 
                                 _  -> x : ys 


-- not very efficient version, sometimes might have do redundand computations
symClos :: Ord a => Rel a -> Rel a
symClos r = extractList $ list2set $ symClos' r


symClos' :: Ord a => Rel a -> Rel a
symClos' [] = []
symClos' (x:xs) = y:(x:(symClos' xs))
					where
						(a,b) = x
						y = (b,a)

-- 5 min

-- EX06
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

infixl 1 $$
 
($$) :: a -> (a -> b) -> b
($$) = flip ($)

trClos :: Ord a => Rel a -> Rel a 
trClos r = (r) $$ fp (\r -> tr r)

tr :: Ord a => Rel a -> Rel a 
tr r = extractList $ list2set $ trClos' r

trClos' :: Ord a => Rel a -> Rel a 
trClos' [] = []
--trClos [r] = [r]
trClos' (x:xs) = [x]++([x] @@ xs)++(trClos' xs)

fp :: Eq a => (Rel a -> Rel a) -> Rel a -> Rel a 
fp f = until (\ x -> x == f x) f


-- ex07
{-- 
seeDiff :: IO Bool
seeDiff = do
	let r = genSet
	result <- compareEx07 r
	return result
--}

compareEx07 :: Rel Int -> Bool
compareEx07 r = a == b where
	a = trClos $ symClos r
	b = symClos $ trClos r

genNumber :: Gen Int
genNumber = do
 	val <- arbitrary
 	let val2 = val + 1000000
 	return val

genRel :: Gen (Rel Int)
genRel = do
 	val <- arbitrary
 	return val 	 	

test07 :: Gen (Bool, Rel Int)
test07 = do
 	val <- arbitrary 
-- 	v <- generate val
 	let result = compareEx07 val
 	return (result, val)

-- Examples when it is not the same
--trClos $ symClos [(1,-1)]
--symClos $ trClos [(1,-1)]

{-- 
generate :: Gen Int -> IO Int
generate g =
  do r <- g
     return r
--}
{----}
--verboteCheck
--quickCheck
--trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
-- (1,3)

