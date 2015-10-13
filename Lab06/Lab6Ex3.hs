module Lab6Ex3 where

import Lecture6
import Data.List

composites :: [Integer]
composites = filter (\x -> not $ isPrime x) [2..]

-- Time taken 5m
