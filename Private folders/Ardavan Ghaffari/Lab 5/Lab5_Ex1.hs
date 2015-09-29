module Lab5_Ex1 where

import Lecture5
import Data.List

blocks' :: [[Int]]
blocks' = [[2..4],[6..8]]

bl' :: Int -> [Int]
bl' x = concat $ filter (elem x) blocks'

subGrid' :: Sudoku -> (Row,Column) -> [Value]
subGrid' s (r,c) = [ s (r',c') | r' <- bl' r, c' <- bl' c ]

freeInSubgrid' :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid' s (r,c) = freeInSeq (subGrid' s (r,c))

freeAtPos' :: Sudoku -> (Row,Column) -> [Value]
freeAtPos' s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c))
   `intersect` (freeInSubgrid' s (r,c))

subgridInjective' :: Sudoku -> (Row,Column) -> Bool
subgridInjective' s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid' s (r,c))   

consistent' :: Sudoku -> Bool
consistent' s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subgridInjective' s (r,c) | r <- [2,6], c <- [2,6]] 

extendNode' :: Node -> Constraint -> [Node]
extendNode' (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune' (r,c,v) constraints) | v <- vs ]  

prune' :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune' _ [] = []
prune' (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune' (r,c,v) rest
  | sameblock' (r,c) (x,y) = 
		(x,y,zs\\[v]) : prune' (r,c,v) rest
  | otherwise = (x,y,zs) : prune' (r,c,v) rest

sameblock' :: (Row,Column) -> (Row,Column) -> Bool
sameblock' (r,c) (x,y) = bl' r == bl' x && bl' c == bl' y

initNode' :: Grid -> [Node]
initNode' gr = let s = grid2sud gr in 
              if (not . consistent') s then [] 
              else [(s, constraints' s)]

constraints' :: Sudoku -> [Constraint] 
constraints' s = sortBy length3rd 
    [(r,c, freeAtPos' s (r,c)) | 
                       (r,c) <- openPositions s ]

solveNs' :: [Node] -> [Node]
solveNs' = search succNode' solved 

succNode' :: Node -> [Node]
succNode' (s,[]) = []
succNode' (s,p:ps) = extendNode' (s,ps) p

solveAndShow' :: Grid -> IO[()]
solveAndShow' gr = solveShowNs' (initNode' gr)

solveShowNs' :: [Node] -> IO[()]
solveShowNs' = sequence . fmap showNode . solveNs'

sudokuNRC :: Grid
sudokuNRC = [
    [0,0,0,3,0,0,0,0,0],
    [0,0,0,7,0,0,3,0,0],
    [2,0,0,0,0,0,0,0,8],
    [0,0,6,0,0,5,0,0,0],
    [0,9,1,6,0,0,0,0,0],
    [3,0,0,0,7,1,2,0,0],
    [0,0,0,0,0,0,0,3,1],
    [0,8,0,0,4,0,0,0,0],
    [0,0,2,0,0,0,0,0,0]
  ]

  -- After refactoring the code from lecture in the
  -- manner above, this new code is now able to satisfy
  -- the new constraints imposed by NRC sudokus
  -- executing the above NRC sudoku with sloveAndShow'
  -- function solves the sudoku with the extra constraint.                                      
