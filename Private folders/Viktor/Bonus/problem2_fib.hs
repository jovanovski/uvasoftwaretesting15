-- problem 002
isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

problem2 = foldr (\x acc -> if (isEven x) && (x <= 4000000) then acc+x else acc) 0 (take 100 fibs)

