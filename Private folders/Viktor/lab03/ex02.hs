import Data.List
import System.Random
import Lecture3
import Testing


testParser :: String -> [Form] -> Bool
testParser "" [] = True
testParser s (x:xs) = False


assertEqual :: (String , [Form]) -> Bool
assertEqual (s, xs) = parse s == xs

assertEqualString :: (String , String) -> Bool
assertEqualString (s, f) = show (parse s) == f



test :: [Test]
test = [Test "testing parse" assertEqual 
			[
				("", []),
				("*(1 +(2 -3))", [Cnj [Prop 1, Dsj [Prop 2, Neg (Prop 3)]]])
			]

		]++
		[Test "testing parse 2" assertEqualString
			[("*(1 2)", "[*(1 2)]"), ("  *(1 2)", "[*(1 2)]"), ("*(1   2)", "[*(1 2)]"), ("*(1 2)   ", "[*(1 2)]"), ("*(1 -2)", "[*(1 -2)]"), ("*(*1 -2)", "[]")]
		]

{----}

delexer :: [Token] -> String
delexer [] = ""
delexer ((TokenInt v):ts) = '!':(delexer ts)
delexer (TokenNeg:ts) = '-':(delexer ts)
delexer (TokenOP:ts) = '(':(delexer ts)
delexer (TokenCP:ts) = ')':(delexer ts)
delexer (TokenImpl:ts) = '=':'=':'>':(delexer ts)
delexer (TokenDsj:ts) = '*':(delexer ts)
delexer (TokenCnj:ts) = '+':(delexer ts)
delexer (TokenEquiv:ts) = '<':'=':'>':(delexer ts)
--delexer (x:ts) | x == TokenInt v = '#':(delexer ts)
--				| otherwise = '?':(delexer ts)

