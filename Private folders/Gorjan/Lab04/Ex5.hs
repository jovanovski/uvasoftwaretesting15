module Ex5 where

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

-- time spent : 5min