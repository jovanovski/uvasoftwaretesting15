data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | (a + b  < c) || (a + c < b) || (b + c < a)  = NoTriangle
				| a == b && a == c = Equilateral
				| (a == b && a /= c) = Isosceles
				| (a == c && a /= b) = Isosceles
				| (b == c && b/= a) = Isosceles			
				| otherwise = isRectangular a b c


isRectangular :: Integer -> Integer -> Integer -> Shape
isRectangular a b c | a*a + b*b == c*c = Rectangular
					| c*c + b*b == a*a = Rectangular
					| a*a + c*c == b*b = Rectangular
					| otherwise = Isosceles 


test = testEquilateral && testIsosceles && testNoTriangle && testRectangular

testEquilateral = testTriangles [(1,1,1), (5,5,5), (4,4,4)] Equilateral
testNoTriangle = testTriangles [(1,2,50), (100,4,5), (5,100,4)] NoTriangle
testIsosceles = testTriangles [(2,2,3), (5,5,3), (4,5,4)] Isosceles
testRectangular = testTriangles [(3,4,5), (3,5,4), (5,4,3)] Rectangular



testTriangles :: [(Integer, Integer, Integer)] -> Shape -> Bool
testTriangles [] _ = True
testTriangles (x:xs) s = (testTriangle x s) && testTriangles xs s

testTriangle :: (Integer, Integer, Integer) -> Shape -> Bool
testTriangle (a,b,c) s = triangle a b c == s


{--
a) all are equal -> Equilateral
b} there are two equal numbers -> IsoSceles, rectangular
c) there are no equal numbers -> NotTriangel, Other


--}