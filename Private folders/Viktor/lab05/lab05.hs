module Lab05

where 

import Data.List
import System.Random

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

type Position = (Row,Column)
type Constrnt = [[Position]]



rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
blockConstrnt2 = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks2, b2 <- blocks2 ]

allConstr = rowConstrnt++columnConstrnt++blockConstrnt

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

blocks2 :: [[Int]]
blocks2 = [[2,3,4], [6,7,8]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- bl r, c' <- bl c ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = 
  freeInSeq [ s (r,i) | i <- positions  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = 
  freeInSeq [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

{-- 
freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c)) 

--}
freeAtPos :: Sudoku -> Position -> [Value]
freeAtPos s (r,c) = freeAtPos' s (r,c) (rowConstrnt++columnConstrnt++blockConstrnt)

-- ****
freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let 
   ys = filter (elem (r,c)) xs 
 in 
   foldl1 intersect (map ((values \\) . map s) ys)
-- **** ****

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where 
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where 
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]

extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune (r,c,v) constraints) | v <- vs ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

prune :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | otherwise = (x,y,zs) : prune (r,c,v) rest

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd 
    [(r,c, freeAtPos s (r,c)) | 
                       (r,c) <- openPositions s ]

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node 
grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int 
count (T _ ts) = 1 + sum (map count ts)

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p 

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = sequence . fmap showNode . solveNs

example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example1sol :: Grid
example1sol = [[0, 3, 4, 6, 7, 8, 9, 1, 2],
               [6, 7, 2, 1, 9, 5, 3, 4, 8],
               [1, 9, 8, 3, 4, 2, 5 ,6, 7],
               [8, 5, 9, 7, 6, 1, 4, 2, 3],
               [4, 2, 6, 8, 5, 3, 7, 9, 1],
               [7, 1, 3, 9, 2, 4, 8, 5, 6],
               [ 9, 6, 1, 5, 3, 7, 2, 8, 4],
               [ 2, 8, 7, 4, 1, 9, 6, 3, 5],
               [ 3, 4, 5, 2, 8, 6, 1, 7, 9 ]]


example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs 
                        then return []
                        else return 
                          (extendNode (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node]) 
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
       then return []
       else 
         if goal (head xs) 
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys 
                      then return [head ys]
                      else if null (tail xs) then return []
                           else 
                             rsearch 
                               succ goal (return $ tail xs)

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

randomS = genRandomSudoku >>= showNode

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s) 
  where s = eraseS (fst n) (r,c) 

minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s

---------------------------------------------------------
-- #####################################################
--------------------------------------------------------
{-- --}
stillUnique :: Node -> Position -> Bool
stillUnique n (r,c) | uniqueSol n' = True
				  | otherwise = False
	where
		 n' = eraseN n (r,c)


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

{-- x
countOccurencesInBlock :: Sudoku -> [Position] -> [Int] -> [Int]
countOccurencesInBlock _ [] o = o
countOccurencesInBlock s (p:ps) o = countOccurencesInBlock s ps (increaseCounter (s p) o)
countOccurencesInBlock
--}

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
