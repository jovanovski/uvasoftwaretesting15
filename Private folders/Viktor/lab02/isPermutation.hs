-- Ex 02
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs [] = False
isPermutation [] _ = False
isPermutation (x:xs) (ys) | x `elem` ys = isPermutation xs (delete x ys)
						  | otherwise = False
