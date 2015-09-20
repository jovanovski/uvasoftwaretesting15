module ParseTest where 

import Lecture3
import Testing

-- START test parse

testParse :: (String, String) -> Bool
testParse (s,e) = show (parse s) == e

parseTests :: [Test]
parseTests = [ Test "parse tests" testParse
    [
      ("1", "[1]"),
      ("(1)", "[1]"),
      ("*(1 2)", "[*(1 2)]"),
      ("*((1 2))", "[*(1 2)]"),
      ("*(1 +(2 -3))", "[*(1 +(2 -3))]"),
      ("*(1 +(2 -3)", "[]"),
      ("*(1 +(2 -3))))", "[*(1 +(2 -3))]"),
      ("+(1 2)", "[+(1 2)]"),
      ("1==>2", "[1==>2]"),
      ("(1==>2)", "[1==>2]"),
      ("((1==>2))", "[1==>2]"),
      ("1<=>2", "[1<=>2]"),
      ("(1<=>2)", "[1<=>2]"),
      ("((1<=>2))", "[1<=>2]")
    ]
  ]

-- END test parse 
-- Time spent: 1h, tested using `runTests parseTests`. 
-- Test result are not so good:  
--    * Implications and equivalences only work in parentheses
--    * Multiple parentheses are not supported
