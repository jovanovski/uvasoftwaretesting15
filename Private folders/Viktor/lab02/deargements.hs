import Data.List
import Testing

-- Ex 03
isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement [] [] = True
isDerangement [] _ = True
isDerangement _ [] = True
isDerangement (x:xs) (y:ys) | x == y = False
							| otherwise  = isDerangement xs ys

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
		   	insrt x [] = [[x]]
 	  		insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)


deran :: Integer -> [[Integer]]
deran 0 =  []
deran n = filter (\y -> isDerangement y [0..(n-1)]) (perms [0..(n-1)])

createList :: Integer -> [Integer]
createList n = [0..n-1]


--TESTING
checkLength :: [Integer] -> [Integer] -> Bool
checkLength xs ys 	| length xs == length ys = True
					| otherwise = False

checkChars :: [Integer] -> [Integer] -> Bool
checkChars xs ys = if sort xs == sort ys then True else False

derangements :: [Integer] -> [[Integer]]
derangements xs = filter (and . zipWith (/=) xs) $ permutations xs

testGen :: Integer -> Bool
testGen n = if sort (deran n) == sort (derangements [0..(n-1)]) then True else False

assertTrue :: Bool -> Bool
assertTrue b = b == True

test = test' 3

test' 0 = return ()
test' n = do
		let res = testGen n
		print("Test for length "++(show n)++(": ")++(show res))
		test' (n-1)

{--
-- It takes parameter n and uses it as a length information for generating derangements
-- estimation time:1,5h , real time: 3h
test
"Test for length 3: True"
"Test for length 2: True"
"Test for length 1: True"
--}
