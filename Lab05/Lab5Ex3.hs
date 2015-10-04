module Lab5Ex3 where

import Lecture5

prop_IsMinimal :: Node -> Bool
prop_IsMinimal p =
  let ps = filledPositions $ fst p
  in filledPositions (fst $ minimalize p ps) == ps

testGenProblem :: Int -> IO ()
testGenProblem 0 = print "All passed"
testGenProblem n = do
  [r] <- rsolveNs [emptyN]
  p  <- genProblem r
  if prop_IsMinimal p then do
    print "Passed on:"
    showSudoku (fst p)
    testGenProblem (n-1)
  else do
    showSudoku (fst p)
    error "Failed"

-- time spent 1h
-- tested using 'testGenProblem'
