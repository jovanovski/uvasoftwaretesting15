module RecognizingTriangles where

import Data.List
import System.Random
import Testing

data Shape = NoTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)	

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = if ( a + b <= c || a + c <= b || b + c <= a) then NoTriangle
					else if ( a == b && b == c) then Equilateral
						else if ( a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2) then Rectangular
							else if ( a == b || a == c || b == c) then Isosceles
								else Other  

testTriangle :: (Integer,Integer,Integer,Shape) -> Bool
testTriangle (a,b,c,s) = triangle a b c == s

manualtriangleTests :: [Test]
manualtriangleTests = [Test "I am testing the triangle function manually" testTriangle
						[(4,4,8,NoTriangle),
						(5,5,5,Equilateral),
						(3,4,5,Rectangular),
						(8,10,10,Isosceles),
						(3,7,8,Other)]
					  ]