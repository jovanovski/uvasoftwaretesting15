module Lab4 where

import Lab2
import SetOrd
import System.Random
import Test.QuickCheck
import Control.Monad
import Data.List
import Data.Char
import Data.Function (on)
-- import Data.Function -- using fix function definition from lecture
import Lecture4

-- START random set generator

-- n = number of test cases to generate
genIntSet :: IO (Set Int)
genIntSet = do
  l <- genIntList 15 15  
  return $ list2set l

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where 
  arbitrary = liftM list2set arbitrary

-- END random set generator 
-- Time taken 30m, tested using `genIntSet 50 50` and `sample $ (arbitrary :: Gen (Set Int))`

-- START set operations

-- unionSet already implemented in SetOrd

testSetProp2 :: (Set Int -> Set Int -> Bool) -> Int -> IO()
testSetProp2 _ 0 = print "All passed"
testSetProp2 p n = do
  f <- genIntSet
  g <- genIntSet 
  let s = (show f) ++ " and " ++ (show g)
  if p f g then do
    print $ "Pass on " ++ s
    testSetProp2 p (n-1)
  else error $ "Failed on " ++ s

instance Foldable (Set) where
  foldr f s (Set l) = foldr f s l

prop_UnionHasAllElements :: Set Int -> Set Int -> Bool
prop_UnionHasAllElements f g = 
  let u = unionSet f g
  in (all (\x -> inSet x u) f) && (all (\x -> inSet x u) g)

prop_UnionHasOnlyElements :: Set Int -> Set Int -> Bool
prop_UnionHasOnlyElements f g = 
  let u = unionSet f g
  in all (\x -> (x `elem` f) || (x `elem` g)) u

-- test using own set generator
testUnionSet :: Int -> IO ()
testUnionSet n = testSetProp2 (\f g -> (prop_UnionHasOnlyElements f g) && (prop_UnionHasAllElements f g)) n

intersectionSet :: Ord a => Set a -> Set a -> Set a
intersectionSet (Set xs) (Set ys) = list2set $ xs `intersect` ys

prop_IntersectionElementsInBoth :: Set Int -> Set Int -> Bool
prop_IntersectionElementsInBoth f g = 
  let i = intersectionSet f g
  in all (\x -> (inSet x f) && (inSet x g)) i

prop_IntersectionHasAllElements :: Set Int -> Set Int -> Bool
prop_IntersectionHasAllElements f g = 
  let i = intersectionSet f g
  in all (\x -> if x `elem` g then inSet x i else True) f

testIntersectionSet :: Int -> IO () 
testIntersectionSet n = testSetProp2 (\f g -> (prop_IntersectionElementsInBoth f g) && (prop_IntersectionHasAllElements f g)) n

differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet (Set xs) (Set ys) = list2set $ xs \\ ys

prop_DifferenceElementsNotInOther :: Set Int -> Set Int -> Bool
prop_DifferenceElementsNotInOther f g = 
  let d = differenceSet f g
  in all (\x -> not (inSet x g)) d

prop_DifferenceHasAllElements :: Set Int -> Set Int -> Bool
prop_DifferenceHasAllElements f g = 
  let d = differenceSet f g
  in all (\x -> if not (x `elem` g) then inSet x d else True) f

testDifferenceSet :: Int -> IO ()
testDifferenceSet n = testSetProp2 (\f g -> (prop_DifferenceElementsNotInOther f g) && (prop_DifferenceHasAllElements f g)) n

-- END set operations
-- Time taken 1.5h. 
-- Tested with own generator and `testUnionSet`, `testIntersectionSet` & `testDifferenceSet`
-- Tested with QuickCheck using properties `prop_UnionHasAllElements`, `prop_UnionHasOnlyElements`, `prop_IntersectionElementsInBoth`, `prop_IntersectionHasAllElements`, `prop_DifferenceElementsNotInOther` & `prop_DifferenceHasAllElements`

-- START symmetric closure

type Rel a = [(a, a)]

symClos :: Ord a => Rel a -> Rel a
symClos rs = sort $ nub $ symClos' rs

symClos' :: Ord a => Rel a -> Rel a
symClos' [] = []
symClos' ((a,b):rs) = (a,b):(b,a):(symClos' rs)

-- END symClos
-- Time taken 15m

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos rs = fix (\f rs -> let cs = sort $ nub (rs ++ (rs @@ rs)) in if rs == cs then rs else f cs) rs

-- END trClos,
-- Time taken 1h

-- START Testing symClos & trClos

prop_SymClosIsSymmetric :: Rel Int -> Bool
prop_SymClosIsSymmetric r = let c = symClos r in all (\(a,b) -> (b,a) `elem` c) c

prop_SymClosContainsOriginal :: Rel Int -> Bool
prop_SymClosContainsOriginal r = let c = symClos r in all (\x -> x `elem` c) r

prop_TrClosIsTransitive :: Rel Int -> Bool
prop_TrClosIsTransitive r = let c = trClos r in all (\v -> v `elem` c) [((x,u)) | (x,y) <- c, (z,u) <- c, y == z]

prop_TrClosContainsOriginal :: Rel Int -> Bool
prop_TrClosContainsOriginal r = let c = trClos r in all (\x -> x `elem` c) r

-- END Testing symClos & trClos
-- Time taken 20m - tested uing quickcheck

-- START sym tr clos is tr sym clos 

prop_SymTrClosIsTrSymClos :: Rel Int -> Bool
prop_SymTrClosIsTrSymClos r = let stc = symClos $ trClos r; tsc = trClos $ symClos r in stc == tsc

{-
They are NOT the same - the transitive closure of a symmetric closure is also reflexive, while a symmetric 
closure of a transitive closure does not need to be so (because of isolated points). An example would be: 

  [(1,0)]

When first taking the transitive closure and then the symmetric closure, this would lead to:

  [(1,0)] -> [(1,0)] -> [(0,1),(1,0)]

While first taking the symmetric closure and then the transitive closure leads to:

  [(1,0)] -> [(0,1),(1,0)] -> [(0,0),(0,1),(1,0),(1,1)]
-}

-- END sym tr clos is tr sym clos
-- Time taken 30m 

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

-- START Extra Bonus - Euler 50

primesbelow :: Integer -> [Integer]
primesbelow n = sievebelow [2..] where 
  sievebelow (x:xs) = if x^2 > n then x:takeWhile (< n) xs else x:(sievebelow [y|y <- xs, y `mod` x /= 0])

longestprimeseqsumprimebelow :: Integer -> [Integer]
longestprimeseqsumprimebelow m = let (c,cl) = helper1 (primesbelow m) [] 0 in c
  where
    -- ps = remaining primes
    -- c = longest sequence
    -- cl = longest sequence length 
    helper1 [] c cl = (c,cl)
    helper1 ps c cl = 
      let (c2,cl2) = (helper2 ps [] 0 0 c cl)
      in helper1 (tail ps) c2 cl2
      where 
        -- ps = remaining primes
        -- r = current sequence
        -- rl = current sequence length
        -- rs = current sequence sum
        -- c = longest sequence 
        -- cl = longest sequence length
        helper2 [] _ _ _ c cl = (c,cl)
        helper2 (p:ps) r rl rs c cl = 
          let rs2 = rs + p
          in if rs2 >= m then (c, cl) else
            let r2 = r ++ [p]
                rl2 = (rl + 1)
            in if (rl2 > cl) && (rs2 `elem` ps) then helper2 ps r2 rl2 rs2 r2 rl2 else helper2 ps r2 rl2 rs2 c cl

sol50 :: Integer
sol50 = sum $ longestprimeseqsumprimebelow 1000000

-- END Extra Bonus - Euler 50
-- Time taken 1h

