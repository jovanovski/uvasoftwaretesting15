module Lab5Ex5 where

import Lecture5
import Data.List
import Lab5_Ex1

rsuccNode' :: Node -> IO [Node]
rsuccNode' (s,cs) = do
  xs <- getRandomCnstr cs
  if null xs then
    return []
  else return
    (extendNode' (s,cs\\xs) (head xs))

rsolveNs' :: [Node] -> IO [Node]
rsolveNs' ns = rsearch rsuccNode' solved (return ns)

genRandomSudoku' :: IO Node
genRandomSudoku' = do
  [r] <- rsolveNs' [emptyN]
  return r

randomS' = genRandomSudoku' >>= showNode

uniqueSol' :: Node -> Bool
uniqueSol' node = singleton (solveNs' [node]) where
 singleton [] = False
 singleton [x] = True
 singleton (x:y:zs) = False

minimalize' :: Node -> [(Row,Column)] -> Node
minimalize' n [] = n
minimalize' n ((r,c):rcs)
  | uniqueSol' n' = minimalize' n' rcs
  | otherwise    = minimalize' n  rcs
 where
   n' = eraseN n (r,c)

genProblem' :: Node -> IO Node
genProblem' n = do
  ys <- randomize xs
  return (minimalize' n ys)
  where
    xs = filledPositions (fst n)

main' :: IO ()
main' = do [r] <- rsolveNs' [emptyN]
           showNode r
           s  <- genProblem' r
           showNode s
