module CSI where

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq, Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

matthew, peter, jack, arnold, carl :: Boy -> Bool
matthew = \x -> not(x == Matthew) && not(x == Carl)
peter = \x -> (x == Matthew) || (x == Jack)
jack = \x -> not(matthew x) && not(peter x)
arnold = \x -> matthew x /= peter x
carl = \x -> not(arnold x)

says :: Boy -> Boy -> Bool
says x y | x == Matthew = matthew y
		 | x == Peter = peter y
		 | x == Jack = jack y
		 | x == Arnold = arnold y
		 | x == Carl = carl y

accusers :: Boy -> [Boy]		 
accusers x = filter (\y -> says y x) boys

guilty, honest :: [Boy]
guilty = filter (\x -> length(accusers x) == 3) boys
honest = honestHelper boys guilty

honestHelper :: [Boy] -> [Boy] -> [Boy]
honestHelper _ [] = []
honestHelper b (g:gs) = filter (\b -> says b g) b ++ honestHelper b gs