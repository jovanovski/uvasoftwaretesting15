module Lab5Ex6
where


import Lecture5
-- bonux ex 06
{-- 
source: http://alwayspuzzling.blogspot.nl/2012/12/how-to-judge-difficulty-of-sudoku.html
NOT really using Difficulty Rating of Sudoku Puzzles - Palanek
--}

type Position = (Row,Column)
type Constrnt = [[Position]]

givenNumbers :: Sudoku -> Int
givenNumbers s = length (filledPositions s)

givensPerBlock :: Sudoku -> [[Position]]
givensPerBlock s = [[(r,c) | r <- b1, c <- b2, s (r,c) /=0] | b1 <- blocks, b2 <- blocks]

numGivensPerBlock :: Sudoku -> [Int]
numGivensPerBlock s =  [length [(r,c) | r <- b1, c <- b2, s (r,c) /=0] | b1 <- blocks, b2 <- blocks]

occurances :: Sudoku -> [Int]
occurances s = foldr (\p acc-> occurances' s p acc) (take 9 [0,0..]) (filledPositions s)
			
occurances' :: Sudoku -> Position -> [Int] -> [Int]
occurances' s p o = (increaseCounter ix o)
		where
			ix = (s p) - 1

-- "private" function, index starting from 1
increaseCounter :: Int -> [Int] -> [Int]
increaseCounter _ [] = []
increaseCounter 0 (x:xs) = (x+1):xs 
increaseCounter n (x:xs) = x:(increaseCounter (n-1) xs)   

{--
Easy sudoku generally have over 32 givens (out of the total of 81 numbers in the answer)
Medium sudoku have around 28–32 givens
Hard sudoku have less than 28 givens
--}
evalGivens :: Sudoku -> [Int] -> [Int]
evalGivens s pts 	| gns < 28 = increaseCounter hardIndex pts
					| gns >= 28 && gns < 32 = increaseCounter mediumIndex pts
					| gns >= 32 = increaseCounter easyIndex pts 
		where
			gns = givenNumbers s


{-- 
Easy sudoku have more than one given in every box
Medium sudoku may have a couple of boxes with only one given
Hard sudoku may have few of boxes with one given
--}
evalBlocks :: Sudoku -> [Int] -> [Int]
evalBlocks s pts = evalBlocks' [x | x <- (numGivensPerBlock s), x <= 1]  pts

evalBlocks' :: [Int] -> [Int] -> [Int]
evalBlocks' [] pts = increaseCounter easyIndex pts
evalBlocks' xs pts = if ((length xs) > ((length positions) `div` 2)) -- 4
						then increaseCounter hardIndex pts
						else increaseCounter mediumIndex pts


{-- 
In Easy sudoku, each digit from 1–9 appears as a given at least 3 times
In Medium sudoku, some digits may only appear twice as a given, the rest will appear at least 3 times each
In Hard sudoku, most digits appear only 2 or 3 times, as well as several single occurrences
--}
evalNumbers :: Sudoku -> [Int] -> [Int]
evalNumbers s pts = evalNumbers' [x | x <- (occurances s), x < 3]  pts

evalNumbers' :: [Int] -> [Int] -> [Int]
evalNumbers' [] pts = increaseCounter easyIndex pts
evalNumbers' xs pts = if ((length xs) > ((length positions) `div` 2)) -- 4
						then increaseCounter hardIndex pts
						else increaseCounter mediumIndex pts

easyIndex= 0
mediumIndex = 1
hardIndex = 2
data Difficulty = Easy | Medium | Hard deriving Show

--evalBlocks :: [Int] -> Difficulty
-- if same amount of points for each cathegory, more difficult level wins
evalPoints :: [Int] -> Difficulty
evalPoints ds@[x,y,z] 	| z == m = Hard
						| y == m = Medium
						| x == m = Easy
					where
						m = maximum ds 
evalPoints xs = Easy

-- returns sudoku difficulty
evalSud :: Sudoku -> IO Difficulty
evalSud s = do
		let r1 = evalGivens s [0,0,0]
		let r2 = evalBlocks s r1
		let r3 = evalNumbers s r2
		let result = evalPoints r3
		return result 

--print level of given sudoku
printSudDiff :: Sudoku -> IO String
printSudDiff sud = do
		diff <- evalSud sud
		let res = "Sudoku level: " ++(show diff)
		return res

-- print random sudoku with level
evalRandomSud :: IO String
evalRandomSud = do
	[r] <- rsolveNs [emptyN]
	s <- genProblem r
	let sud = fst s
	showNode s
	diff <- evalSud sud
	let res = "Sudoku level: " ++(show diff)
	return res