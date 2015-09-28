module Lab5Ex1 where

import Data.List
import Lecture5

ex1sudoku :: Grid
ex1sudoku = [
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

blocks2 = [[2..4],[6..8]]

bl2 :: Int -> [Int]
bl2 x = concat $ filter (elem x) blocks2

subGrid2 :: Sudoku -> (Row,Column) -> [Value]
subGrid2 s (r,c) = [ s (r',c') | r' <- bl2 r, c' <- bl2 c ]

freeInSubgrid2 :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid2 s (r,c) = freeInSeq (subGrid2 s (r,c))

freeAtPos2 :: Sudoku -> (Row,Column) -> [Value]
freeAtPos2 s (r,c) = (freeInRow s r) `intersect` (freeInColumn s c) `intersect` (freeInSubgrid s (r,c)) `intersect` (freeInSubgrid2 s (r,c))

subgrid2Injective :: Sudoku -> (Row,Column) -> Bool
subgrid2Injective s (r,c) = injective vs where
  vs = filter (/= 0) (subGrid2 s (r,c))

consistent2 :: Sudoku -> Bool
consistent2 s = and $ [ rowInjective s r |  r <- positions ] ++ [ colInjective s c |  c <- positions ] ++ [ subgridInjective s (r,c) | r <- [1,4,7], c <- [1,4,7]] ++ [ subgrid2Injective s (r,c) | r <- [2,6], c <- [2,6]] ++ [ subgrid2Injective s (r,c) | r <- [2,6], c <- [2,6]]

constraints2 :: Sudoku -> [Constraint]
constraints2 s = sortBy length3rd [(r,c, freeAtPos2 s (r,c)) | (r,c) <- openPositions s ]

extendNode2 :: Node -> Constraint -> [Node]
extendNode2 (s,constraints) (r,c,vs) = [(extend s ((r,c),v), sortBy length3rd $ prune2 (r,c,v) constraints) | v <- vs ]

prune2 :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune2 _ [] = []
prune2 (r,c,v) ((x,y,zs):rest)
 | r == x = (x,y,zs\\[v]) : prune2 (r,c,v) rest
 | c == y = (x,y,zs\\[v]) : prune2 (r,c,v) rest
 | sameblock (r,c) (x,y) = (x,y,zs\\[v]) : prune2 (r,c,v) rest
 | sameblock2 (r,c) (x,y) = (x,y,zs\\[v]) : prune2 (r,c,v) rest
 | otherwise = (x,y,zs) : prune2 (r,c,v) rest

sameblock2 :: (Row,Column) -> (Row,Column) -> Bool
sameblock2 (r,c) (x,y) = bl2 r == bl2 x && bl2 c == bl2 y

initNode2 :: Grid -> [Node]
initNode2 gr = let s = grid2sud gr in if (not . consistent2) s then [] else [(s, constraints2 s)]

solveNs2 :: [Node] -> [Node]
solveNs2 = search succNode2 solved

succNode2 :: Node -> [Node]
succNode2 (s,[]) = []
succNode2 (s,p:ps) = extendNode2 (s,ps) p

solveShowNs2 :: [Node] -> IO[()]
solveShowNs2 = sequence . fmap showNode . solveNs2

solveAndShow2 :: Grid -> IO[()]
solveAndShow2 gr = solveShowNs2 (initNode2 gr)
