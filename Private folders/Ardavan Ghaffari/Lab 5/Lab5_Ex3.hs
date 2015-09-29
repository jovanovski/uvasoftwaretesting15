module Lab5_Ex3 where

import Lecture5

isMinimal :: Node -> Bool
isMinimal n = uniqueSol n && not(minimalize' n (filledPositions (fst n)))

minimalize' :: Node -> [(Row,Column)] -> Bool
minimalize' n [] = False
minimalize' n ((r,c):rcs) = uniqueSol n' || minimalize' n rcs
  where n' = eraseN n (r,c) 

testSudokuGen :: Int -> Int -> IO ()  
testSudokuGen k n = if k == n then print ( show n ++ " minimal sudokus generated")
						else do
							[r] <- rsolveNs [emptyN]
							s <- genProblem r
							if isMinimal s then do
								print "problem below is minimal"
								showSudoku (fst s)
								testSudokuGen (k+1) n
							else do
								showSudoku (fst s)
								error "this problem is not minimal"
