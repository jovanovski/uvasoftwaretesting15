module Lab5Ex5 where

import Lecture5
import Data.List
import Lab5Ex1

rsuccNode2 :: Node -> IO [Node]
rsuccNode2 (s,cs) = do
  xs <- getRandomCnstr cs
  if null xs then
    return []
  else return
    (extendNode2 (s,cs\\xs) (head xs))

rsolveNs2 :: [Node] -> IO [Node]
rsolveNs2 ns = rsearch rsuccNode2 solved (return ns)

genRandomSudoku2 :: IO Node
genRandomSudoku2 = do
  [r] <- rsolveNs2 [emptyN]
  return r

randomS2 = genRandomSudoku2 >>= showNode

uniqueSol2 :: Node -> Bool
uniqueSol2 node = singleton (solveNs2 [node]) where
 singleton [] = False
 singleton [x] = True
 singleton (x:y:zs) = False

minimalize2 :: Node -> [(Row,Column)] -> Node
minimalize2 n [] = n
minimalize2 n ((r,c):rcs)
  | uniqueSol2 n' = minimalize2 n' rcs
  | otherwise    = minimalize2 n  rcs
 where
   n' = eraseN n (r,c)

genProblem2 :: Node -> IO Node
genProblem2 n = do
  ys <- randomize xs
  return (minimalize2 n ys)
  where
    xs = filledPositions (fst n)

prop_IsConsistent :: Node -> Bool
prop_IsConsistent (s,_) = consistent2 s

testGenProblem2 :: Int -> IO ()
testGenProblem2 0 = print "All passed"
testGenProblem2 n = do
  [r] <- rsolveNs2 [emptyN]
  p  <- genProblem2 r
  if prop_IsConsistent p then do
    print "Passed on:"
    showNode r
    showNode p
    testGenProblem2 (n-1)
  else do
    showNode r
    showNode p
    error "Failed"
