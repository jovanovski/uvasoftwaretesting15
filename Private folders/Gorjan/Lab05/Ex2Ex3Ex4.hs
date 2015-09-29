  module Ex2
  
  where 
  
  import Data.List
  import System.Random
  import Test.QuickCheck
  import Data.Random.Extras

  type Row    = Int 
  type Column = Int 
  type Value  = Int
  type Grid   = [[Value]]
  type Position = (Row, Column)
  type Constrnt = [[Position]]
  
  positions :: [Int]
  positions = [1..9]

  values :: [Int]
  values    = [1..9] 

  blocks :: [[Int]]
  blocks = [[1..3],[4..6],[7..9]]
 
  rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
  columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
  blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
  
  constraints' :: [Constrnt]
  constraints' = [rowConstrnt, columnConstrnt, blockConstrnt]

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

  type Sudoku = Position -> Value

  sud2grid :: Sudoku -> Grid
  sud2grid s = 
    [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 
  
  grid2sud :: Grid -> Sudoku
  grid2sud gr = \ (r,c) -> pos gr (r,c) 
    where 
    pos :: [[a]] -> Position -> a 
    pos gr (r,c) = (gr !! (r-1)) !! (c-1)

  showSudoku :: Sudoku -> IO()
  showSudoku = showGrid . sud2grid

  bl :: Int -> [Int]
  bl x = concat $ filter (elem x) blocks 

  subGrid :: Sudoku -> Position -> [Value]
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

  freeInSubgrid :: Sudoku -> Position -> [Value]
  freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

  intersectAll :: [[Value]] -> [Value]
  intersectAll [x] = x
  intersectAll (x:xs) = x `intersect` (intersectAll xs)

  freeAtPos :: Sudoku -> Position -> [Value]
  freeAtPos s (r,c) = nub (intersectAll ([freeAtPos' s (r,c) cons | cons <- constraints']))

  freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
  freeAtPos' s (r,c) xs = let 
     ys = filter (elem (r,c)) xs 
   in 
    foldl1 intersect (map ((values \\) . map s) ys)

  injective :: Eq a => [a] -> Bool
  injective xs = nub xs == xs

  rowInjective :: Sudoku -> Row -> Bool
  rowInjective s r = injective vs where 
     vs = filter (/= 0) [ s (r,i) | i <- positions ]

  colInjective :: Sudoku -> Column -> Bool
  colInjective s c = injective vs where 
     vs = filter (/= 0) [ s (i,c) | i <- positions ]

  subgridInjective :: Sudoku -> Position -> Bool
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

  extend :: Sudoku -> (Position,Value) -> Sudoku
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
  
  sameblock :: Position -> Position -> Bool
  sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 

  initNode :: Grid -> [Node]
  initNode gr = let s = grid2sud gr in 
                if (not . consistent) s then [] 
                else [(s, constraints s)]

  openPositions :: Sudoku -> [Position]
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

  eraseS :: Sudoku -> Position -> Sudoku
  eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                       | otherwise      = s (x,y)

  eraseN :: Node -> Position -> Node
  eraseN n (r,c) = (s, constraints s) 
    where s = eraseS (fst n) (r,c) 

  minimalize :: Node -> [Position] -> Node
  minimalize n [] = n
  minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                           | otherwise    = minimalize n  rcs
    where n' = eraseN n (r,c)

  filledPositions :: Sudoku -> [Position]
  filledPositions s = [ (r,c) | r <- positions,  
                                c <- positions, s (r,c) /= 0 ]

  genProblem :: Node -> IO Node
  genProblem n = do 
                  ys <- randomize xs
                  return (minimalize n ys)
                  where xs = filledPositions (fst n)
  
  main :: IO ()
  main = do [r] <- rsolveNs [emptyN]
            showNode r
            s  <- genProblem r
            showNode s
            print (show (length (filledPositions (fst s))))
-- Ex3

  testUnique :: IO ()
  testUnique = do 
            print "Working..."
            [r] <- rsolveNs [emptyN]
            s  <- genProblem r
            let minNode = minimalize s (filledPositions (fst s))
            let pos = (filledPositions (fst s)) == (filledPositions (fst minNode))
            print pos
            
-- end Ex3

-- Ex4
  combinations :: Int -> [a] -> [[a]]
  combinations 0 _  = [ [] ]
  combinations n xs = [ y:ys | y:xs' <- tails xs
                             , ys <- combinations (n-1) xs']

  deleteBlock :: Node -> [Position] -> Node
  deleteBlock n [] = n
  deleteBlock n (x:xs) = deleteBlock (eraseN n x) xs

  deleteBlocks :: Node -> [[Position]] -> Node
  deleteBlocks n [] = n
  deleteBlocks n (x:xs) = deleteBlocks (deleteBlock n x) xs

  getSolvable :: Node -> [[[Position]]] -> Node
  getSolvable n [] = n
  getSolvable n (x:xs) = if uniqueSol (deleteBlocks n x) then (deleteBlocks n x) else getSolvable n xs


  -- testblocks
  -- args:
  -- - number of blocks to remove
  -- - number of tries to do
  -- - counter from 0 to count the tries
  testblocks :: Int -> Int -> Int -> IO ()
  testblocks x n o = do
                    [r] <- rsolveNs [emptyN]
                    let pos = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
                    let coms = combinations x pos
                    let solvable = getSolvable r coms 
                    if (filledPositions (fst solvable))==(filledPositions (fst r)) then do 
                      print ("Generated sudoku can't be solved in a unique way by removing " ++ (show x) ++ " blocks. "++(show n)++ " more tries remaining.") 
                      if n>0 then testblocks x (n-1) (o+1) else print("No solution found, usually meaning "++(show x)++" blocks can't be removed from a Sudoku and still keep it with one unique solution.")
                    else do
                      print("Solved on the "++(show o)++" try:")
                      s <- genProblem solvable 
                      showSudoku (fst s)
                      dif <- sudDifficulty s
                      print ("Sudoku difficulty: " ++ dif)


  callTest :: Int -> IO ()
  callTest n = if n<=0 || n>=9 then error ("Can't delete "++(show n)++" blocks, wanna just try generating a normal Sudoku using 'main'?") else testblocks n 5 1
-- end Ex4

-- Ex6
-- testing sudoku difficulty, no generation possible
-- calculated using the 3 guidelines from here: http://alwayspuzzling.blogspot.nl/2012/12/how-to-judge-difficulty-of-sudoku.html
  countHints :: Node -> Int
  countHints n = length (filledPositions (fst n))

  countingTest :: Node -> [Integer] -> [Integer]
  countingTest n xs = if (countHints n) > 32 then [((xs!!0)+1),(xs!!1),(xs!!2)]
                      else if (countHints n) > 28 then [(xs!!0),((xs!!1)+1),(xs!!2)]
                      else [(xs!!0),(xs!!1),((xs!!2)+1)]

  blockPos :: [Position] -> [Integer]
  blockPos [] = []
  blockPos (x:xs) = whatBlock x : blockPos xs

  whatBlock :: Position -> Integer
  whatBlock (x,y) = if x <= 3 && y <= 3 then 1
                    else if x <= 3 && y >=4 && y <= 6 then 4
                    else if x <= 3 && y >=7 then 7
                    else if x >= 4 && x <= 6 && y <= 3 then 2
                    else if x >= 4 && x <= 6 && y >=4 && y <= 6 then 5
                    else if x >= 4 && x <= 6 && y >= 7 then 8
                    else if x >= 7 && y <= 3 then 3
                    else if x >= 7 && y >= 4 && y <= 6 then 6
                    else 9

  numEmptyBoxes :: Node -> Integer
  numEmptyBoxes n = toInteger (9 - (length (nub (blockPos (filledPositions (fst n))))))

  emptyBoxesTest :: Node -> [Integer] -> [Integer]
  emptyBoxesTest n xs = emptyBoxesTest' n xs (numEmptyBoxes n)

  emptyBoxesTest' :: Node -> [Integer] -> Integer -> [Integer]
  emptyBoxesTest' n xs e = if e == 0 then [((xs!!0)+1),(xs!!1),(xs!!2)]
                          else if e < 3 then [(xs!!0),((xs!!1)+1),(xs!!2)]
                          else [(xs!!0),(xs!!1),((xs!!2)+1)]

  countDigits :: Node -> [Integer]
  countDigits n = (filter (/=0) (concat [[toInteger(((sud2grid (fst n))!!r)!!c)| c <- [0..8] ] | r <- [0..8] ]))

  digitRepeat :: Integer -> [Integer] -> Integer
  digitRepeat n [] = 0
  digitRepeat n (x:xs) = if x==n then 1+(digitRepeat n xs) else (digitRepeat n xs)

  seeDigitRepeat :: Node -> [Integer]
  seeDigitRepeat n = [digitRepeat i (countDigits n) | i <- [1..9]]

  digitRepeatTest :: Node -> [Integer] -> [Integer]
  digitRepeatTest n xs = if all (>2) (seeDigitRepeat n) then [((xs!!0)+1),(xs!!1),(xs!!2)]
                          else if length (filter (==2) (seeDigitRepeat n))<=4 && length (filter (==1) (seeDigitRepeat n))<=1 then [(xs!!0),((xs!!1)+1),(xs!!2)]
                          else [(xs!!0),(xs!!1),((xs!!2)+1)]

  sudDifficulty :: Node -> IO String
  sudDifficulty n = do
    let cTest = countingTest n [0,0,0]
    let digTest = digitRepeatTest n cTest
    let ebTest = emptyBoxesTest n digTest
    if ebTest!!0 == maximum ebTest then return "Easy"
      else if ebTest!!1 == maximum ebTest then return "Medium"
      else return "Hard"

--end Ex6

--Ex7 
-- hints range from 22 to 25 on a standard Sudoku, but take quicker to minimize
-- hints range from 16 to 19 on a NRC Sudoku, but take a lot longer to minimize on that aspect