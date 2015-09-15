module Ex2 where

import Lecture3
import Testing


testParse :: (String, [Form]) -> Bool
testParse (s, f) = parse s == f

parseTests :: [Test]
parseTests = [ Test "testing parse " testParse
             [
	            ("*(1  2)))))))", [Cnj [Prop 1, Prop 2]]),
	            ("*(1 -2)", [Cnj [Prop 1, Neg(Prop 2)]]),
	            ("(1)", [Prop 1]),
	            ("2==>1", [Impl (Prop 2) (Prop 1)]),
	           	("2<=>1", [Equiv (Prop 2) (Prop 1)]),
	           	("1", [Prop 1]),
	           	("(2==>1)", [Impl (Prop 2) (Prop 1)]),
	           	("((2==>1)", [Impl (Prop 2) (Prop 1)])
	           	]
            ]

-- | parse fails for single prop in parenthesis
-- | parse fails for implication without parenthesis
-- | parse fails for equivilance without parenthesis
-- | parse fails for extra heading parenthesis