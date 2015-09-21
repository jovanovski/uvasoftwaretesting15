module Ex2 where

import SetOrd
import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad


addNumber :: Integer -> Set Integer -> Set Integer
addNumber a b = insertSet a b 

insertRndNumbers :: Integer -> Set Integer -> IO (Set Integer)
insertRndNumbers 0 x = return x
insertRndNumbers n x = do
	rn <- randomRIO (0, 100)
	list <- insertRndNumbers (n-1) (addNumber rn x)
	return list

generateRndList :: IO (Set Integer)
generateRndList = do
    len <- randomRIO (0, 10)
    list <- insertRndNumbers len (Set [])
    return list

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where 
  arbitrary = liftM list2set arbitrary


--Checks order and no duplicates in Set
prop_checkOrder :: Set Integer -> Bool
prop_checkOrder (Set []) = True
prop_checkOrder (Set x) = True
prop_checkOrder (Set (x:y:xs)) = if x<y then prop_checkOrder (Set xs) else False

generateSet :: Gen (Set Integer)
generateSet = do
	a <- arbitrary
	return a

