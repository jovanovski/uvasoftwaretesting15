module Lab6_Ex1

where 

expM' :: Integer -> Integer -> Integer -> Integer
expM' x 1 m = x `mod` m
expM' x 0 m = 1 `mod` m
expM' x y m = mod ((exMHelper x y' m) * (expM' x (y-y') m)) m
	where y' = hpo2 1 y

exMHelper :: Integer -> Integer -> Integer -> Integer
exMHelper x 1 m = x `mod` m
exMHelper x y m = ((exMHelper x (y `div` 2) m) ^ 2) `mod` m

-- this function calculates the highest power of 2 in a number and returns it
hpo2 :: Integer -> Integer -> Integer
hpo2 e x = if (e' <= x) then hpo2 e' x
						else e
				where
					e' = e*2				

			