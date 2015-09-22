module Lab04 

where

import SetOrd
import Data.List
import Data.Map
import System.Random
import Test.QuickCheck 
import Control.Monad

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where 
  arbitrary = do
  	a <-arbitrary
  	return $ list2set a 

--instance Foldable (Set a) where
 -- foldr f s (Set l) = foldr f s l

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

-- QuickCheck
genSetQC :: Gen (Set Int)
genSetQC = do
 	val <- arbitrary
 	return val 	

-- Ex 03
{--
Union is already defined in SetOrd.hs

unionSet :: (Ord a) => Set a -> Set a -> Set a 
unionSet (Set [])     set2  =  set2
unionSet (Set (x:xs)) set2  = 
   insertSet x (unionSet (Set xs) set2)
--}



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
-- TESTING	
prop_unionSet :: Set Int -> Set Int -> Bool
prop_unionSet s1 s2 = all (\x -> inSet x s1 || inSet x s2) (extractList (unionSet s1 s2))

prop_intersectSet :: Set Int -> Set Int -> Bool
prop_intersectSet s1 s2 = all (\x -> inSet x s1 && inSet x s2) (extractList (interSet s1 s2))

prop_diffSet :: Set Int -> Set Int -> Bool
prop_diffSet s1 s2 = all (\x -> inSet x s1 && not (inSet x s2)) (extractList (diffSet s1 s2))


--MY TESTS
{-- 
testInter :: IO String
testInter = do
	let result = ""

	return result

testInter' :: Int -> [String] -> IO [String]
testInter' 0 acc = acc
testInter' n acc = testInter' (n-1) ((failure):acc)
				where
					failure = if (prop)


testInter :: IO (
testInter =  do 
	let n = 100
	loop $ do 
           s <- genSet
           while (n > 0)
--}

testSets :: IO [String]
testSets = do
	u <- testUnion
	i <- testInter
	d <- testDiff
	let result = u++i++d 
	return result

testInter :: IO [String]
testInter = do
	result <- testN prop_intersectSet 100
	print ("Intersection: "++show (100 - (length result))++" out of 100 tests passed.")
	return result

testUnion :: IO [String]
testUnion = do
	result <- testN prop_unionSet 100
	print ("Union: "++show (100 - (length result))++" out of 100 tests passed.")
	return result

testDiff :: IO [String]
testDiff = do
	result <- testN prop_diffSet 100
	print ("Difference: "++show (100 - (length result))++" out of 100 tests passed.")
	return result
	
testN :: (Set Int -> Set Int -> Bool) -> Int -> IO [String]
testN _ 0 = return []
testN prop n = do
	x <- testSingle prop []
	xs <- testN prop (n-1)
	let result = x++xs
	return result 

testSingle :: (Set Int -> Set Int -> Bool) -> [String] -> IO [String]
testSingle prop acc = do
	s1 <- genSet
	s2 <- genSet
	let result = if (prop s1 s2) then [] else [show (s1) ++ " and "++(show s2)]
	return result


-- REPORT
-- 40 min

--ex05
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
symClos r = nub $ symClos' r


symClos' :: Ord a => Rel a -> Rel a
symClos' [] = []
symClos' (x:xs) = y:(x:(symClos' xs))
					where
						(a,b) = x
						y = (b,a)

prop_symetric :: (Ord a) => Rel a -> Bool
prop_symetric r = and [ (y,x) `elem` r | (x, y) <- r]



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

prop_transitive :: (Ord a) => Rel a -> Bool
prop_transitive r = and [not ((y == y2) && (not ((x,z) `elem` r))) | (x,y) <- r, (y2,z) <- r]

{-- isReversed :: Ord a => (a,a) -> (a,a) -> Bool
isReversed (x1,y1) (y2, x2) | x1 == x2 || y1 == y2 = True
							| otherwise = False 
--}


-- REPORT
-- 50min

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
symetric as the first applied function makes pairs (a,b) and (b,a). Therefore result ater applying trClos must contains reflexive tuples. 
The other way around doesnt imply that there must be reflexive tuples. So they are not equal and it depends on the order of functions
--}


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



