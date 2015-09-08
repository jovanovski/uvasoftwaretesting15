module Triangles where

import Data.List

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle 0 _ _ = NoTriangle
triangle _ 0 _ = NoTriangle
triangle _ _ 0 = NoTriangle
triangle x y z 	| x<0 || y<0 || z<0 = NoTriangle
				| (sort [x,y,z])!!0 + (sort [x,y,z])!!1 < (sort [x,y,z])!!2 = NoTriangle
				| x == y && y == z = Equilateral
				| ((sort [x,y,z])!!0)^2 + ((sort [x,y,z])!!1)^2 == ((sort [x,y,z])!!2)^2 = Rectangular
				| x==y || x == z || y == z = Isosceles
				| otherwise = Other


notriangles = [[0,0,0], [0,0,1], [0,-1,20],[-3,-4,-5]]
equilateraltriangles = [[1,1,1],[22,22,22],[3,3,3],[43,43,43]]
isoscelestriangles = [[2,2,3],[4,2,4],[5,5,1],[2,6,6],[0,0,1]]
rectangulartirangles = [[3,4,5],[5,12,13],[6,8,10],[7,24,25]]

testTriangles :: [[Integer]] -> Shape -> IO Bool
testTriangles [] _ = do return True
testTriangles (x:xs) s = if triangle (x!!0) (x!!1) (x!!2) == s then testTriangles xs s
							else do
							print ("failed on " ++ show x)
							return False 

-- time ~ 10 min
-- tested using special function and pregenerated test cases
