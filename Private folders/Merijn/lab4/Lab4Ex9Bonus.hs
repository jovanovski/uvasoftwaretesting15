module Lab4Ex9Bonus where

import Test.QuickCheck
import Control.Monad
import Data.List
import Data.Char
import Lecture4

-- START Bonus - show Function

instance Show Expr where
  show e = case e of
    (I i) -> show i
    (V v) -> v
    (Add a b) -> "(" ++ (show a) ++ " + " ++ (show b) ++ ")"
    (Subtr a b) -> "(" ++ (show a) ++ " - " ++ (show b) ++ ")"
    (Mult a b) -> "(" ++ (show a) ++ " * " ++ (show b) ++ ")"

instance Show Condition where
  show c = case c of
    (Prp v) -> v
    (Eq e1 e2) -> "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
    (Lt e1 e2) -> "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
    (Gt e1 e2) -> "(" ++ show e1 ++ " > " ++ show e2 ++ ")"
    (Ng c) -> "not(" ++ show c ++ ")"
    (Cj cs) -> "and(" ++ (intercalate ", " (map show cs)) ++ ")"
    (Dj cs) -> "or(" ++ (intercalate ", " (map show cs)) ++ ")"

showIndent :: Int -> Statement -> String
showIndent i s =
  let p1 = replicate i ' '
      i2 = i + 4
      p2 = replicate i2 ' '
  in  case s of
        (Ass v e) ->  p1 ++ v ++ " = " ++ (show e) ++ ";\n"
        (Cond c s1 s2) -> p1 ++ "if (" ++ (show c) ++ ") {\n" ++ (showIndent i2 s1) ++ p1 ++ "} else {\n" ++ (showIndent i2 s2) ++ p1 ++ "}\n"
        (Seq sts) ->  p1 ++ "{\n" ++ concat (map (showIndent i2) sts) ++ p1 ++ "}\n"
        (While c s) -> p1 ++ "while (" ++ (show c) ++") {\n" ++ (showIndent i2 s) ++ p1 ++ "}\n"

instance Show Statement where
  show s = showIndent 4 s

arbVarname :: Gen String
arbVarname = elements ["a", "b", "c", "d"]

arbI :: Gen Expr
arbI = liftM I (resize 5 arbitrary)

arbV :: Gen Expr
arbV = liftM V arbVarname

arbExpr :: Int -> Gen Expr
arbExpr n =
  case n of
    0 -> oneof [arbI, arbV]
    n -> oneof [
        arbI,
        arbV,
        liftM2 Add subExpr subExpr,
        liftM2 Subtr subExpr subExpr,
        liftM2 Mult subExpr subExpr
      ]
  where
    subExpr = arbExpr (n `div` 2)

instance Arbitrary Expr where
  arbitrary = sized arbExpr

arbCondition :: Int -> Gen Condition
arbCondition n = case n of
  0 -> oneof [
      liftM Prp arbVarname,
      liftM2 Eq subExpr subExpr,
      liftM2 Lt subExpr subExpr,
      liftM2 Gt subExpr subExpr
    ]
  n -> oneof [
      liftM Prp arbVarname,
      liftM2 Eq subExpr subExpr,
      liftM2 Lt subExpr subExpr,
      liftM2 Gt subExpr subExpr,
      liftM Ng subCondition,
      liftM Cj subConditionList,
      liftM Dj subConditionList
    ]
  where
    subExpr = arbExpr (n `div` 2)
    subCondition = arbCondition (n `div` 2)
    subConditionList = (resize 5 (listOf subCondition))

instance Arbitrary Condition where
  arbitrary = sized arbCondition

arbAss :: Int -> Gen Statement
arbAss n = liftM2 Ass arbVarname (arbExpr n)

arbStatement :: Int -> Gen Statement
arbStatement n = case n of
  0 -> subAss
  n -> oneof [
      subAss,
      liftM3 Cond subCondition subStatement subStatement,
      liftM Seq (resize 5 (listOf subStatement)),
      liftM2 While subCondition subStatement
    ]
  where
    subAss = (arbAss (n `div` 2))
    subStatement = arbStatement (n `div` 2)
    subCondition = arbCondition (n `div` 2)

instance Arbitrary Statement where
  arbitrary = sized arbStatement

filterWhitespace :: String -> String
filterWhitespace s = filter (\c -> not $ isSpace c) s

readVar :: String -> (String,String)
readVar s = (takeWhile isAlpha s, dropWhile isAlpha s)

read2Close :: Read a => Read b => (a -> b -> String -> [(c, String)]) -> Int -> String -> [(c, String)]
read2Close f d r = case readsPrec d r of
  (a,r1):[] -> case r1 of
    ' ':r2 -> case readsPrec d r2 of
      (b,r3):[] -> case r3 of
        ')':r4 -> f a b r4

readArgClose :: Read a => (a -> String -> [(b, String)]) -> Int -> String -> [(b, String)]
readArgClose f d r = case readsPrec d r of
  [] -> []
  (a,r1):[] -> case r1 of
    ')':r2 -> f a r2
    otherwise -> []

instance Read Expr where
  readsPrec d r =
    let rs = filterWhitespace r
    in case rs of
    '(':r1 ->
      case readsPrec d r1 of
        [] -> []
        (e1,r2):[] -> case r2 of
          '+':r3 -> readArgClose (\e2 r -> [(Add e1 e2, r)]) d r3
          '-':r3 -> readArgClose (\e2 r -> [(Subtr e1 e2, r)]) d r3
          '*':r3 -> readArgClose (\e2 r -> [(Mult e1 e2, r)]) d r3
          otherwise -> []
        otherwise -> []
    (x:_) ->
      if isAlpha x then
        let (v,r1) = readVar r in [(V v, r1)]
      else if isDigit x || x == '-' then
        let (i,r1):[] = readsPrec d r in [(I i, r1)]
      else []
    otherwise -> []

prop_ExprReadShowIsEqual :: Expr -> Bool
prop_ExprReadShowIsEqual e = e == (read $ show e)

instance Read Condition where
  readsPrec d r =
    let rs = filterWhitespace r
    in case rs of
      '(':r1 ->
        case readsPrec d r1 of
          [] -> []
          (e1,r2):[] -> case r2 of
            '=':'=':r3 -> readArgClose (\e2 r -> [(Eq e1 e2, r)]) d r3
            '<':r3 -> readArgClose (\e2 r -> [(Lt e1 e2, r)]) d r3
            '>':r3 -> readArgClose (\e2 r -> [(Gt e1 e2, r)]) d r3
            otherwise -> []
          otherwise -> []
      'n':'o':'t':'(':r1 -> readArgClose (\c r -> [(Ng c, r)]) d r1
      'a':'n':'d':'(':r1 -> readConditionsClose (\cs r -> [(Cj cs, r)]) r1
      'o':'r':'(':r1 -> readConditionsClose (\cs r -> [(Dj cs, r)]) r1
      x:_ ->
        if isAlpha x then
          let (v,r1) = readVar r in [(Prp v, r1)]
        else []
      otherwise -> []
    where
      readConditionsClose f r =
        let (cs, r1) = readConditions r
        in case r1 of
          ')':r2 -> f cs r2
          otherwise -> []
      readConditions r = case readsPrec d r of
        [] -> ([], r)
        (c,r1):[] ->
          case r1 of
            ',':r2 -> let (cs,r3) = readConditions r2 in (c:cs,r3)
            otherwise -> ([c], r1)

prop_ConditionReadShowIsEqual :: Condition -> Bool
prop_ConditionReadShowIsEqual e = e == (read $ show e)

instance Read Statement where
  readsPrec d r =
    let rs = filterWhitespace r
    in case rs of
      'i':'f':'(':r1 -> case readsPrec d r1 of
        [] -> []
        (c,r2):[] -> case r2 of
          ')':'{':r3 -> case readsPrec d r3 of
            [] -> []
            (s1,r4):[] -> case r4 of
              '}':'e':'l':'s':'e':'{':r5 -> case readsPrec d r5 of
                [] -> []
                (s2,r6):[] -> case r6 of
                  '}':r7 -> [(Cond c s1 s2, r7)]
              otherwise -> []
          otherwise -> []
      '{':r1 ->
        let (sts,r2) = readStatements r1
        in case r2 of
          '}':r3 -> [(Seq sts, r3)]
          otherwise -> []
      'w':'h':'i':'l':'e':'(':r1 -> case readsPrec d r1 of
        [] -> []
        (c,r2):[] -> case r2 of
          ')':'{':r3 -> case readsPrec d r3 of
            [] -> []
            (s,r4):[] -> case r4 of
              '}':r5 -> [(While c s, r5)]
              otherwise -> []
          otherwise -> []
      otherwise ->
        let (v,r1) = readVar rs
        in case r1 of
          '=':r2 -> case readsPrec d r2 of
            [] -> []
            (e,r3):[] -> case r3 of
              ';':r4 -> [(Ass v e, r4)]
              otherwise -> []
          otherwise -> []
    where
      readStatements r = case readsPrec d r of
        [] -> ([], r)
        (s,r1):[] ->
          let (sts,r2) = readStatements r1
          in (s:sts, r2)

prop_StatementReadShowIsEqual :: Statement -> Bool
prop_StatementReadShowIsEqual s = s == (read $ show s)

-- END Bonus - show function
-- Time taken 6h - this was too much time for a bonus exercise to be fun...
-- Tested using quickcheck and prop_*ReadShowIsEqual
