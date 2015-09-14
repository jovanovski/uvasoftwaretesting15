module Ex03 where

import Lecture3
import Testing

cnf :: Form -> Form
cnf f = applyCnj $ nnf $ arrowfree f


-- function converts form into the CNF
applyCnj :: Form -> Form
applyCnj (Prop a) = Prop a
applyCnj (Neg p) = Neg p
applyCnj (Cnj fs) = Cnj (map applyCnj fs)
--applyCnj (Dsj [Prop a, Prop b]) = (Dsj [Prop a,Prop b])
applyCnj (Dsj [(Prop a), (Cnj fs)]) = (Cnj (map applyCnj [(Dsj [Prop a,b]) | b <- fs]))
applyCnj (Dsj [aa@(Neg (Prop a)), (Cnj fs)]) = (Cnj (map applyCnj [(Dsj [aa,b]) | b <- fs]))
applyCnj (Dsj [(Cnj fs), Prop b]) = (Cnj(map applyCnj [(Dsj [a,Prop b]) | a <- fs]))
applyCnj (Dsj [(Cnj fs), Neg (Prop b)]) = (Cnj(map applyCnj [(Dsj [a, Neg (Prop b)]) | a <- fs]))
applyCnj (Dsj [(Cnj fs), (Cnj gs)]) = (Cnj (map applyCnj [(Dsj [a,b]) | a <- fs, b <- gs]))
-- the rest of cases: literals and negative literals
applyCnj (Dsj [a, b]) = (Dsj [a,b])

-- maximum 2 forms, not used
deMorgan :: Form -> Form
deMorgan (Prop p)= (Prop p)
deMorgan (Neg (Neg (Prop p))) = (Prop p)
deMorgan (Neg f) = (Neg f)
deMorgan (Dsj [p,q]) = Cnj [Neg p, Neg q]
deMorgan (Cnj [p,q]) = Dsj [Neg p, Neg q]

-- TESTING
a = Prop 1
b = Prop 2
c = Prop 3 


myForm1 = Neg a
myForm2 = Cnj [a,b]
myForm3 = Dsj [a,b]
--myForm3Correct = Cnj [Neg a, Neg b]
--myForm4 = Dsj [a,Cnj [b,c]]
myForm4 = Dsj [Cnj [a,Cnj [b,c]], a]
myForm4Correct = Cnj []
myForm5 = Cnj [a,Cnj[b,c]]
myForm6 = Dsj [a,Cnj [a,Cnj [b,c]]]
myForm6Correct = Cnj [Dsj [a,a], Cnj [Dsj [a,b], Dsj [a,c]]]

assertEqual :: (Form ,Form) -> Bool
assertEqual (f,g) = cnf f == g


test :: [Test]
test = [Test "testing cnj" assertEqual [
			(Prop 1, Prop 1),
			(myForm1, myForm1),
			(myForm2, myForm2),
			(myForm3, myForm3),
			(myForm5, myForm5),
			(myForm6, myForm6Correct)
--			(myForm4, myForm4Correct)
			]

		]
{----}		
