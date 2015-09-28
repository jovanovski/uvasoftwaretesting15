module Lab5Ex2 where

import Data.List
import Lecture5
import Lab5Ex1

type Position = (Row,Column)
type Constrnt = [[Position]]

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
block2Constrnt = [[(r,c)| r <- b1, c <- b2] | b1 <- blocks2, b2 <- blocks2]

constraints' :: Constrnt
constraints' = concat [
    rowConstrnt,
    columnConstrnt,
    blockConstrnt,
    block2Constrnt
  ]

intersectingConstraints :: Position -> Constrnt
intersectingConstraints p = filter (elem p) constraints'

freeAtPos' :: Sudoku -> Position -> [Value]
freeAtPos' s p =
  let ys = intersectingConstraints p
  in foldl1 intersect (map ((values \\) . map s) ys)

consistent3 :: Sudoku -> Bool
consistent3 s = and $ map (injective . (filter (/= 0))) [[s rc | rc <- c] | c <- constraints']

constraints3 :: Sudoku -> [Constraint]
constraints3 s = sortBy length3rd [(r,c, freeAtPos' s (r,c)) | (r,c) <- openPositions s ]

extendNode3 :: Node -> Constraint -> [Node]
extendNode3 (s,constraints) (r,c,vs) = [(extend s ((r,c),v), sortBy length3rd $ prune3 (r,c,v) constraints) | v <- vs ]

prune3 :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune3 _ [] = []
prune3 (r,c,v) ((x,y,zs):rest) =
  if any (\c -> (x,y) `elem` c) $ intersectingConstraints (r,c) then
    (x,y,zs\\[v]) : prune3 (r,c,v) rest
  else
    (x,y,zs) : prune3 (r,c,v) rest

initNode3 :: Grid -> [Node]
initNode3 gr = let s = grid2sud gr in if (not . consistent3) s then [] else [(s, constraints3 s)]

solveNs3 :: [Node] -> [Node]
solveNs3 = search succNode3 solved

succNode3 :: Node -> [Node]
succNode3 (s,[]) = []
succNode3 (s,p:ps) = extendNode3 (s,ps) p

solveShowNs3 :: [Node] -> IO[()]
solveShowNs3 = sequence . fmap showNode . solveNs3

solveAndShow3 :: Grid -> IO[()]
solveAndShow3 gr = solveShowNs3 (initNode3 gr)

-- time spent: 1h
-- test using 'solveAndShow3 ex1sudoku'
