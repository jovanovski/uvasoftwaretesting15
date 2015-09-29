module Lab5_Ex4 where

import Lecture5
import Lab5_Ex3

allBlocks = [[(r,c) | r <- b1 , c <- b2] | b1 <- blocks , b2 <- blocks]

-- n is the number of blocks to be removed
main' :: Int -> IO ()
main' n = do 
	[r] <- rsolveNs [emptyN]
	bs <- randomize allBlocks
	let r' = eraseBlocks r (take n bs)
	s <- genProblem r'
	if isMinimal s then showNode s else main' n


eraseBlocks :: Node -> [[(Row,Column)]] -> Node
eraseBlocks n [] = n
eraseBlocks n (b:bs) = eraseBlocks (eraseBlock n b) bs
 
eraseBlock :: Node -> [(Row,Column)] -> Node
eraseBlock n [] = n
eraseBlock n ((r,c):rcs) = eraseBlock (eraseN n (r,c)) rcs 