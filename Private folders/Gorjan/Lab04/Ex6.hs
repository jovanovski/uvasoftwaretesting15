module Ex6 where

import SetOrd
import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 
trClos [] = []
trClos xs = if (sort(nub((xs @@ xs) ++ xs)))==xs then xs else trClos (sort(nub((xs @@ xs) ++ xs)))

-- time spent : 15min