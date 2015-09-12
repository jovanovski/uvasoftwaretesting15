module Triangles where

import Data.List
import Testing

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

-- TESTING

testTriangles :: ([Integer], Shape) -> Bool
testTriangles (xs, s) = triangle (xs!!0) (xs!!1) (xs!!2) == s

noTriangleTests :: [Test]
noTriangleTests = [ Test "testing notriangles " testTriangles
             [([0,0,0], NoTriangle), ([0,0,1], NoTriangle), ([0,-1,20], NoTriangle), ([-3,-4,-5], NoTriangle),([2,2,6],NoTriangle)]
           ]
equilateralTests :: [Test]
equilateralTests = [ Test "testing equilateral triangles " testTriangles
             [([1,1,1],Equilateral),([22,22,22],Equilateral),([3,3,3],Equilateral),([43,43,43],Equilateral)]
           ]
isoscelesTests :: [Test]
isoscelesTests = [ Test "testing isosceles triangles " testTriangles
             [([2,2,3],Isosceles),([2,4,2],Isosceles),([5,5,1],Isosceles)]
           ]

rectangularTests :: [Test]
rectangularTests = [ Test "testing rectangular triangles " testTriangles
             [([3,4,5],Rectangular),([5,12,13],Rectangular),([6,8,10],Rectangular),([7,24,25],Rectangular)]
           ]

allTests :: [Test]
allTests = concat [ noTriangleTests
                  , equilateralTests
                  , isoscelesTests
                  , rectangularTests

                  ]
-- time ~ 15 min
-- tested using special function and pregenerated test cases
--testreport:
--  *Triangles> runTests allTests
--[]