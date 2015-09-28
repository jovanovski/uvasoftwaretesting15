module Lab5Ex4 where

import Lecture5
import Data.List

erasePositions :: Node -> [(Row,Column)] -> Node
erasePositions n [] = n
erasePositions n (p:ps) = erasePositions (eraseN n p) ps

eraseRandomBlocks :: Node -> Int -> [[(Row, Column)]] -> IO Node
eraseRandomBlocks n 0 _ = return n
eraseRandomBlocks n m bs = do
  b:[] <- getRandomItem bs
  n' <- eraseRandomBlocks (erasePositions n b) (m-1) (delete b bs)
  return n'

-- m = number of empty blocks
genProblemWithEmptyBlocks :: Int -> IO ()
genProblemWithEmptyBlocks m = do
  [n] <- rsolveNs [emptyN]
  n' <- eraseRandomBlocks n m [ [(r,c) | r <- b1, c <- b2] | b1 <- blocks, b2 <- blocks]
  s <- genProblem n'
  if uniqueSol s then showNode s else genProblemWithEmptyBlocks m

-- time taken: 1h
-- tested with 'genProblemWithEmptyBlocks'
-- It is possible to generate problems with up to 4 empty blocks.
