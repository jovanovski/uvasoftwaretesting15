module Ex4 where

import Data.List
import System.Random
import Testing

aset = ["p", "q", "r", "s"]
b11set = ["("]
b12set = ["-"]
b13set = [")"]
c1set = ["+", "*"]
c2set = ["==>", "<=>"]

generateRndList :: IO [Int]
generateRndList = do
    rs <- randomRIO (0, 5)
    perms <- generateRndList 
    return (rs:perms)