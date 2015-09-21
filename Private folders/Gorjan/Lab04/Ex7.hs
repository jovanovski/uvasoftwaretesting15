module Ex6 where

import SetOrd
import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad

type Rel a = [(a,a)]

symClose' :: Ord a => Rel a -> Rel a
symClose' [] = []
symClose' ((a,b):xs) = if (b,a) `elem` xs then (a,b) : symClose' xs else (a,b) : (b,a) : symClose' xs

symClose :: Ord a => Rel a -> Rel a
symClose x = nub (symClose' x)

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 
trClos [] = []
trClos xs = if (sort(nub((xs @@ xs) ++ xs)))==xs then xs else trClos (sort(nub((xs @@ xs) ++ xs)))


testProp :: Rel Integer -> Bool
testProp x = trClos ( symClose x ) == symClose ( trClos x)

-- TEST concludes that there is a difference when symmetric closure is applied over a transitive closure, and the other way aroud
-- falsifiable test case Rel [(0,1)]