module Lab4Ex6 where

import Data.List
import Data.Function -- using fix function definition from lecture
import Lab4Ex5

-- START trClos

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos rs = fix (\f rs -> let cs = sort $ nub (rs ++ (rs @@ rs)) in if rs == cs then rs else f cs) rs

-- END trClos
-- Time taken 1h
