module Lab6_Ex3

where

import Lecture6

composites :: [Integer]
composites = filter (not.isPrime) [2..]	