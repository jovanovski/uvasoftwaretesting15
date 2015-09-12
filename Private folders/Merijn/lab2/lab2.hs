module Lab2 where 

import Data.List
import Data.Char
import System.Random
import Testing

-- START `Recognizing triangles`

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

-- END `Recognizing triangles`
-- 
-- Time taken: 30m

-- START `Recognizing permutations` 

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = x `elem` ys && isPermutation xs (delete x ys)

testIsPermutation :: Eq a => ([a], [a], Bool) -> Bool
testIsPermutation (a,b,r) = isPermutation a b == r 

isPermutationTests :: [Test]
isPermutationTests = [ Test "is permutation test" testIsPermutation
   [
     ([1,2,3], [3,2,1], True),
     ([1,2,2], [2,2,1], True),
     ([1,2], [1,2,2], False),
     ([], [], True),
     ([], [1,2], False),
     ([1,2], [], False)
   ]
  ]

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- m = max length of list, n = max bound of int
genIntList :: Int -> Int -> IO [Int]
genIntList m n = do 
  p <- getRandomInt m
  q <- genIntListL p
  return q where 
    genIntListL 0 = return []
    genIntListL r = do
      t <- getRandomInt n
      ts <- genIntListL (r-1)
      return $ t:ts

-- m = number of lists to generate, n = max length of list, p = max bound of int
genPermTestCases :: Int -> Int -> Int -> IO [([Int], [Int], Bool)]
genPermTestCases 0 _ _ = return []
genPermTestCases m n p = do 
  r <- genIntList n p
  s <- (shuffle r)
  t <- genPermTestCases (m-1) n p
  u <- getRandomInt 1
  if length r == 0 || u == 0 then 
    return $ (r, s, True) : t
  else 
    do 
      v <- getRandomInt ((length s)-1)
      return $ (r, take v s ++ [(s!!v)+1] ++ drop (v+1) s, False) : t

shuffle :: Eq a => [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  p <- getRandomInt (length xs - 1)
  q <- shuffle $ delete (xs !! p) xs
  return $ (xs !! p) : q

autoTestIsPermutation :: IO ()
autoTestIsPermutation = do
  xs <- genPermTestCases 100 10 20
  helper xs where
    helper [] = print "All tests passed"
    helper ((a,b,r):ys) = 
      let msg  = show a ++ " is " ++ (if r then "" else "not ") ++ "a permutation of " ++ show b
      in  if isPermutation a b == r then
            do
              print $ "pass on: " ++ msg
              helper ys
          else
            do 
              print $ "failed on: " ++ msg

-- END `Recognizing permutations`
-- 
-- Automating test process: For correct cases this is easily possible using the shuffle function, but to fully
-- test the function we would also need to create deviations. This can be done using the method shown in
-- the genPermTestCases function, but it only creates deviations with a single modified element and of the same
-- length as the original. To implement automated tests for all possible deviations would be a lot of work.
-- 
-- time taken: 2,5h

-- START `Recognizing and generating derangements`

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] _ = False
isDerangement _ [] = False
isDerangement (x:xs) (y:ys) = x /= y && isDerangement xs ys

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ y:z | y <- xs, z <- perms $ delete y xs]

deran :: Eq a => [a] -> [[a]]
deran xs = [y | y <- perms xs, isDerangement xs y == True]

testIsDerangement :: Eq a => ([a], [a], Bool) -> Bool
testIsDerangement (a,b,r) = isDerangement a b == r

isDerangementTests = [Test "is derangement test" testIsDerangement
    [
      ([], [], True),
      ([1], [1], False),
      ([1,2,3], [2,3,1], True),
      ([1,2,3], [3,2,1], False),
      ([1,2], [1,2,2], False),
      ([1,2,2], [1,2], False),
      ([2,2,4,4], [4,4,2,2], True),
      ([], [1,2], False),
      ([1,2], [], False)
    ]
  ]

derange :: [a] -> IO [a]
derange [] = return [] 
derange [x] = error "no derangement possible"
derange xs = helper xs 0 where 
  helper xs n = 
    let l = length xs
    in 
      if n == l then 
        return $ xs
      else 
        do 
          r <- rsInt n (l-1)
          let p = xs !! n
              q = xs !! r
              ys = splice r 1 [] (splice n 1 [q] xs)
          do 
            sub <- helper ys (n+1)
            return (splice r 0 [p] sub)
            
rsInt :: Int -> Int -> IO Int 
rsInt n m = 
  do 
    r <- getRandomInt m
    if r /= n then 
      return r 
    else
      do 
        sub <- rsInt n m 
        return sub

splice :: Int -> Int -> [a] -> [a] -> [a]
splice p r a l = (take p l) ++ a ++ (drop (p+r) l)

-- END Recognizing and generating derangements`
--
-- Automating test process: It is difficult to automate this test process in a useful way because generating 
-- derangements uses the isDerangement function internally. Of course this could be rewritten to not use the 
-- isDerangement function itself but only it's internal functionality, but this would not change much about 
-- the usefulness. 
-- 
-- time taken: 15m

-- START `Implementing and testing IBAN validation` 

iban :: String -> Bool
iban s = ibanCheckDigits s == 1

ibanCheckDigits :: String -> Integer
ibanCheckDigits s = (convertIbanToInteger $ rearrangeIban $ filterNonAlphaNumeric s) `mod` 97

filterNonAlphaNumeric :: String -> String
filterNonAlphaNumeric = filter isAlphaNum

rearrangeIban :: String -> String
rearrangeIban s = 
  let (f,l) = splitAt 4 s 
  in  l ++ f

convertIbanToInteger :: String -> Integer
convertIbanToInteger s = read $ foldr (++) "" [ if isNumber x then x:"" else show $ (ord $ toLower x) - 87 | x <- s ]

testIban :: (String,Bool) -> Bool
testIban (a,b) = iban a == b

ibanTests = [Test "iban test" testIban 
    [
      ("AL47 2121 1009 0000 0002 3569 8741", True),
      ("AL47 2122 1009 0000 0002 3569 8741", False),
      ("BA39 1290 0794 0102 8494", True),
      ("BA39 1290 0794 0102 8493", False),
      ("BA39 1290 0794 0102 8494", True),
      ("CA39 1290 0794 0102 8494", False),
      ("SK31 1200 0000 1987 4263 7541", True),
      ("SK31 1200 0000 2134 4263 7541", False)
    ]
  ]

-- arguments: n = number of ibans to generate, m = max bban length
genIbanTestCases :: Int -> Int -> IO [(String, Bool)]
genIbanTestCases 0 _ = return []
genIbanTestCases n m = do
  cc <- genAlphanumericList 2
  l <- getRandomInt m
  bban <- genAlphanumericList l
  r <- genIbanTestCases (n-1) m
  let cd = 98 - ibanCheckDigits (cc ++ "00" ++ bban)
      cds = if cd < 10 then "0" ++ show cd else show cd 
  return $ (cc ++ cds ++ bban, True) : r

genAlphanumericList :: Int -> IO String
genAlphanumericList 0 = return ""
genAlphanumericList n = do
  o <- getRandomAlphanumeric 
  p <- genAlphanumericList (n-1)
  return $ o : p

getRandomAlphanumeric :: IO Char
getRandomAlphanumeric = do
  x <- getRandomInt 35
  return $ if x < 10 then intToDigit x else chr (x + 55)

autoTestIban :: IO ()
autoTestIban = do
  xs <- genIbanTestCases 100 10
  helper xs where
    helper [] = print "All tests passed"
    helper ((a,r):ys) = 
      let msg = show a ++ " is " ++ (if r then "" else "not ") ++ "a valid iban"
      in  if iban a == r then
            do
              print $ "pass on: " ++ msg
              helper ys
          else
            do 
              print $ "failed on: " ++ msg

-- END `Implementing and testing IBAN validation` 
--
-- Automating test process: As shown, generating random ibans internally uses the same functionality as the
-- iban check itself. Just like for the derangements, this is not very useful. 
-- 
-- time taken: 1,5h

allTests = concat [
    triangleTests,
    isPermutationTests,
    isDerangementTests,
    ibanTests
  ]