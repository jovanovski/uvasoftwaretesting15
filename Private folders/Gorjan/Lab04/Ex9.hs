module Ex9 where

import Lecture4
import Data.List


fib' :: Statement
fib' = Seq [Ass "x" (I 0), Ass "y" (I 1), Cond (Cj [(Ng (Lt (V "x") (V "y"))), (Gt (V "y") (V "x"))]) (Ass "y" (Add (V "x") (V "y"))) (Ass "x" (Add (V "x") (V "y"))), While (Gt (V "n") (I 0))
              (Seq [Ass "z" (V "x"), 
                    Ass "x" (V "y"),
                    Ass "y" (Add (V "z") (V "y")), 
                    Ass "n" (Subtr (V "n") (I 1))])]



showCond :: Condition -> String
showCond (Prp prop) = prop
showCond (Eq ex1 ex2) = (showExpr ex1) ++ "==" ++ (showExpr ex2)
showCond (Lt ex1 ex2) = (showExpr ex1) ++ "<" ++ (showExpr ex2)
showCond (Gt ex1 ex2) = (showExpr ex1) ++ ">" ++ (showExpr ex2)
showCond (Ng cond) = "!(" ++ (showCond cond) ++ ")"
showCond (Cj conds) = intercalate " && " (map showCond conds)
showCond (Dj conds) = intercalate " || " (map showCond conds)


showExpr :: Expr -> String
showExpr (I i) = show i
showExpr (V var) = var
showExpr (Add x y) = (showExpr x) ++ "+" ++ (showExpr y)
showExpr (Subtr  x y) = (showExpr x) ++ "-" ++ (showExpr y)
showExpr (Mult x y) = (showExpr x) ++ "*" ++ (showExpr y)

showStatement :: Statement -> String
showStatement (Seq sq) = showSeq sq
showStatement (Ass var expr) = (var ++ ":="++(showExpr expr)++"; ")
showStatement (Cond cond st1 st2) = "if("++(showCond cond)++") then { " ++ (showStatement st1) ++ "} else { " ++ (showStatement st2)++"}; "
showStatement (While cond st) = "while ("++(showCond cond) ++ ") { " ++ (showStatement st) ++ "};"

showSeq :: [Statement] -> String
showSeq [] = ""
showSeq (x:xs) = (showStatement x)++(showSeq xs)