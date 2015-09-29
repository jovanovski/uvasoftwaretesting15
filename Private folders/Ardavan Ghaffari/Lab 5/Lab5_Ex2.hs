module Lab5_Ex2 where

import Lecture5
import Data.List

type Position = (Row,Column)
type Constrnt = [[Position]]	

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

sudokuConstrnt :: Constrnt
sudokuConstrnt = rowConstrnt ++ columnConstrnt ++ blockConstrnt

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let 
   ys = filter (elem (r,c)) xs 
 in 
   foldl1 intersect (map ((values \\) . map s) ys)

initNode' :: Grid -> [Node]
initNode' gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints' s)]  

constraints' :: Sudoku -> [Constraint] 
constraints' s = sortBy length3rd 
    [(r,c, freeAtPos' s (r,c) sudokuConstrnt) | 
                       (r,c) <- openPositions s ] 

solveAndShow' :: Grid -> IO[()]
solveAndShow' gr = solveShowNs (initNode' gr)

