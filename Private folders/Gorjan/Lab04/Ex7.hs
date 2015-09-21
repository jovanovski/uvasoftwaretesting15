module Ex7 where

import System.Random
import Test.QuickCheck
import Ex5
import Ex6

insertRndNumbers :: Integer -> Rel Integer -> IO (Rel Integer)
insertRndNumbers 0 x = return x
insertRndNumbers n x = do
	rn1 <- randomRIO (0, 10)
	rn2 <- randomRIO (0, 10)
	list <- insertRndNumbers (n-1) ((rn1, rn2):x)
	return list

generateRndList :: IO (Rel Integer)
generateRndList = do
    len <- randomRIO (0, 10)
    list <- insertRndNumbers len ([])
    return list

--Manual
testRndListSymmetry :: IO Bool
testRndListSymmetry = do
	x <- generateRndList
	print ("Generated rel: " ++ (show x))
	let sym = symClose x
	print ("Symmetric closure rel: " ++ (show sym))
	--test if all the elements in x have their flips in sym
	let res = all (\(a,b) -> (b,a) `elem` sym) x
	print ("All elements in rnd list have symmetries in closure? " ++ (show res))
	--test that the symClose of x is the symClose (symClose x); there are no more pairs that were missed
	let res2 = symClose x == symClose sym
	print ("Is the symmetric closure of the symmetric closure equal to the symmetric closure of the generated list? " ++ (show res2))
	return (res && res2)


-- Quickcheck symmetric
prop_allIn :: Rel Integer -> Bool
prop_allIn x = all (\y -> y `elem` symClose x) x

prop_hasFliped :: Rel Integer -> Bool
prop_hasFliped x = all (\(a,b) -> (b,a) `elem` symClose x) x 

prop_noMoreSymmetry :: Rel Integer -> Bool
prop_noMoreSymmetry x = symClose x == symClose (symClose x)


-- Quickcheck transitive
prop_noMoreTransitivity :: Rel Integer -> Bool
prop_noMoreTransitivity x = prop_noMoreTransitivity' (trClos x)

prop_noMoreTransitivity' :: Rel Integer -> Bool
prop_noMoreTransitivity' tr = all (\(a,b) -> all (\(c,d) -> b/=c || (a,d) `elem` tr) (tr)) (tr)