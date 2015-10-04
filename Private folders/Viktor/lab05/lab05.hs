module Lab05

where 

import Data.List
import System.Random
import Lecture5

-- ex 01 and ex05
blocks2 :: [[Int]]
blocks2 = [[2,3,4], [6,7,8]]


rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
blockConstrnt2 = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks2, b2 <- blocks2 ]

allConstr = rowConstrnt++columnConstrnt++blockConstrnt++blockConstrnt2

freeAtPos :: Sudoku -> Position -> [Value]
freeAtPos s (r,c) = freeAtPos' s (r,c) allConstr

-- ****
freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let 
   ys = filter (elem (r,c)) xs 
 in 
   foldl1 intersect (map ((values \\) . map s) ys)
-- **** ****

type Position = (Row,Column)
type Constrnt = [[Position]]

stillUnique :: Node -> Position -> Bool
stillUnique n (r,c) | uniqueSol n' = True
				  | otherwise = False
	where
		 n' = eraseN n (r,c)

-- ex03
testMin :: IO Bool
testMin = do 
			[r] <- rsolveNs [emptyN]
			s <- genProblem r
			let sud = fst s
			showNode s
			let xs = filledPositions sud
			let list = [x| x <- xs, stillUnique s x]
			let res = null list
			return res


--EX04
eraseNodeBlocks :: Node -> Int -> [[Position]] -> IO Node
eraseNodeBlocks n 0 _ = return n
eraseNodeBlocks n x bs = do
		[b] <- getRandomItem bs 
		n' <- eraseNodeBlocks (erasePositions n b) (x-1) (delete b bs)
		return n' 

erasePositions :: Node -> [Position] -> Node
erasePositions n [] = n
erasePositions n (x:xs) = erasePositions (eraseN n x) xs

genProblemEmptyBlocks :: Int -> IO ()
genProblemEmptyBlocks n = do 
			[r] <- rsolveNs [emptyN]
--			showNode r
			s <- eraseNodeBlocks r n [[(r,c) | r <- b1, c <- b2] | b1 <- blocks, b2 <- blocks]
			p <- genProblem s
			let res  = if (uniqueSol p) then p else emptyN
			showNode res

-- make functions to generate emptyBlock problem for subgridInjective

	
-- bonus 01
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
-- same points, more difficult level wins
evalPoints ds@[x,y,z] 	| z == m = Hard
						| y == m = Medium
						| x == m = Easy
					where
						m = maximum ds 
evalPoints xs = Easy

evalSud :: Sudoku -> IO Difficulty
evalSud s = do
		let r1 = evalGivens s [0,0,0]
		let r2 = evalBlocks s r1
		let r3 = evalNumbers s r2
		let result = evalPoints r3
		return result 

evalRandomSud :: IO String
evalRandomSud = do
	[r] <- rsolveNs [emptyN]
	s <- genProblem r
	let sud = fst s
	showNode s
	diff <- evalSud sud
	let res = "Soduku level: " ++(show diff)
	return res
