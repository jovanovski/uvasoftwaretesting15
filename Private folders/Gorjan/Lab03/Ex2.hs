module Ex2 where

import Lecture3
import Testing


testParse :: (String, [Form]) -> Bool
testParse (s, f) = parse s == f

parseTests :: [Test]
parseTests = [ Test "testing parse " testParse
             [("*(1 2)", [Cnj [Prop 1, Prop 2]]), ("*(1 -2)", [Cnj [Prop 1, Neg(Prop 2)]])]
           ]

testParse2 :: (String, String) -> Bool
testParse2 (s, e) = show(parse s) == e

parseTests2 :: [Test]
parseTests2 = [ Test "testing2 parse " testParse2
             [("*(1 2)", "[*(1 2)]"), ("  *(1 2)", "[*(1 2)]"), ("*(1   2)", "[*(1 2)]"), ("*(1 2)   ", "[*(1 2)]"), ("*(1 -2)", "[*(1 -2)]"), ("*(*1 -2)", "[]")]]