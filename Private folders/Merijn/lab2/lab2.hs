module Lab2 where 

import Data.List
import Data.Char
import System.Random
import Testing

data Shape = NoTriangle | Equilateral | Rectangular | Isosceles | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
  | x <= 0 || y <= 0 || z <= 0 = NoTriangle
  | x == y && y == z = Equilateral
  | x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || y^2 + z^2 == x^2 = Rectangular
  | x == y || x == z || y == z = Isosceles
  | otherwise = Other

testTriangle :: ((Integer, Integer, Integer), Shape) -> Bool
testTriangle ((a,b,c), t) = triangle a b c == t

triangleTests :: [Test]
triangleTests = [ Test "triangle test" testTriangle
                  [
                    ((0,1,2), NoTriangle),
                    ((1,0,2), NoTriangle),
                    ((1,2,0), NoTriangle),
                    ((-1,1,1), NoTriangle),
                    ((1,-1,1), NoTriangle),
                    ((1,1,-1), NoTriangle),
                    ((3,3,3), Equilateral),
                    ((6,6,6), Equilateral),
                    ((3,4,5), Rectangular),
                    ((4,5,3), Rectangular),
                    ((5,3,4), Rectangular),
                    ((4,4,2), Isosceles),
                    ((4,2,4), Isosceles),
                    ((2,4,4), Isosceles),
                    ((2,2,8), Isosceles),
                    ((1,2,3), Other),
                    ((2,1,3), Other),
                    ((1,2,3), Other)
                  ]
                ]

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = x `elem` ys && isPermutation xs (delete x ys)

getRandomInteger :: Int -> IO Int
getRandomInteger n = getStdRandom (randomR (0,n))

-- arguments n = number of lists, m = max list length, o = max number bound
genLists :: Int -> Int -> Int -> IO [([Int], [Int], Bool)]
genLists 0 _ _ = return []
genLists n m o = do 
  l <- getRandomInteger m
  p <- genListL l o
  q <- genLists (n-1) m o
  r <- (shuffle p)
  return ((p, r, True) : q)

genListL :: Int -> Int -> IO [Int]
genListL 0 _ = return []
genListL n o = do
  x <- getRandomInteger o
  p <- genListL (n-1) o
  return (x:p)
  
shuffle :: Eq a => [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  p <- getRandomInteger (length xs - 1)
  q <- shuffle (delete (xs !! p) xs)
  return ((xs !! p) : q)

changeAtRandom :: Eq a => [a] -> IO [a]
changeAtRandom [] = return []
changeAtRandom ys@(x:xs) = do
  p <- getRandomInteger 1
  q <- getRandomInteger (length xs)
  if p == 0 then 
    return ys
  else
    let (a,b) = splitAt q xs
    in return (a ++ [x] ++ b)
  
testIsPermutation :: IO ()
testIsPermutation = do
  xs <- genLists 100 10 20
  helper xs where
    helper [] = print "All tests passed"
    helper ((a,b,r):ys) = 
      if isPermutation a b == r then 
        do 
          print ("pass on: " ++ show a ++ " is permutation of " ++ show b)
          helper ys
      else
        do 
          print ("failed on: " ++ show a ++ " is a permutation of " ++ show b)

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement x y = (isPermutation x y) && hasElementsInSamePosition x y == False

hasElementsInSamePosition :: Eq a => [a] -> [a] -> Bool
hasElementsInSamePosition [] _ = False
hasElementsInSamePosition _ [] = False
hasElementsInSamePosition (x:xs) (y:ys) = x == y || hasElementsInSamePosition xs ys

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = [a ++ [x] ++ b | ys <- perms xs, z <- [0..(length ys)], let (a,b) = splitAt z ys]

deran :: Eq a => [a] -> [[a]]
deran xs = [y | y <- perms xs, (hasElementsInSamePosition xs y) == False]

iban :: String -> Bool
iban s = 
  let sf = filter isAlphaNum s
  in (read (foldr (++) "" [if isNumber x then x:"" else show (ord (toLower x) - 87) | x <- (drop 4 sf) ++ (take 4 sf)])) `mod` 97 == 1