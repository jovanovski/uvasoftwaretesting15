-- Ex 03
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] _ = True
isDerangement _ [] = True
isDerangement (x:xs) (y:ys) | x == y = False
							| otherwise  = isDerangement xs ys

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
		   	insrt x [] = [[x]]
 	  		insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)


deran :: Int -> [[Int]]
--deran n =  [0..n-1]
deran n = [x  | x <- perms $ createList n, isDerangement x]

createList :: Int -> [Int]
createList n = [0..n-1]


--testing
