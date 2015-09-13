import Data.List
import System.Random
import Testing
-- Ex 02
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs [] = False
isPermutation [] _ = False
isPermutation (x:xs) (ys) | x `elem` ys = isPermutation xs (delete x ys)
						  | otherwise = False


-- TESTING
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
		   	insrt x [] = [[x]]
 	  		insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)




isPermutation2 :: Eq a => [a] -> [a] -> Bool
isPermutation2 xs ys = isContained ys (perms xs)

getRandomInt :: Int -> Int -> IO Int
getRandomInt l h = getStdRandom (randomR (l,h))

isContained :: Eq a => [a] -> [[a]] -> Bool
isContained _ [] = False
isContained xs (ys:yss)  | xs == ys = True
						 | otherwise = isContained xs yss


-- randomly pick two of given list, return a pair
pickTwo :: [[a]] -> IO ([a],[a])
pickTwo xss = do
		i <- getRandomInt 0 ((length xss)-1)  
		j <- getRandomInt 0 ((length xss)-1) -- might happend i == j
		return (xss!!i,xss!!j)


randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 0 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 0 20
  n <- getRandomInt 0 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt 0 k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)


testIsPerm :: IO String
testIsPerm = do
	list <- genIntList
	(xs,ys) <- pickTwo (perms list)
	print ("Selected lists: "++(show xs)++ " and "++(show ys))
	let r1 = isPermutation xs ys
	print("isPermutation: "++(show r1))
	let r2 = isPermutation2 xs ys
	print("isPermutation2: "++(show r2))
	let r = r1 ==r2
	let res = r == True
	let output = ("Result: "++(show res))
	print (output)
	return output

-- runs testIsPerm 3 times (put there your number)
test = test' 3

test' 0 = return ()
test' n = do
		testIsPerm
		test' (n-1)
{--REPORT
-- estimation time 1h, real time 3h (always more because of the tests)
"Selected lists: [-10,0,-5,15,-2] and [-2,-5,-10,15,0]"
"isPermutation: True"
"isPermutation2: True"
"Result: True"
"Selected lists: [4,16,4,-11,-6,-17,-8,-11] and [-6,4,-17,-11,4,-8,16,-11]"
"isPermutation: True"
"isPermutation2: True"
"Result: True"
"Selected lists: [-6,13,3,-8,-7,11] and [3,11,-8,13,-7,-6]"
"isPermutation: True"
"isPermutation2: True"
"Result: True"
--}
