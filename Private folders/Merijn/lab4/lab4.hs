module Lab4 where

import Lab2
import SetOrd
import System.Random
import Test.QuickCheck
import Control.Monad
import Data.List
import Data.Char
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
    (Add a b) -> "+(" ++ (show a) ++ " " ++ (show b) ++ ")"
    (Subtr a b) -> "-(" ++ (show a) ++ " " ++ (show b) ++ ")"
    (Mult a b) -> "*(" ++ (show a) ++ " " ++ (show b) ++ ")"

instance Show Condition where 
  show c = case c of 
    (Prp v) -> v
    (Eq a b) -> "==(" ++ show a ++ " " ++ show b ++ ")"
    (Lt a b) -> "<(" ++ show a ++ " " ++ show b ++ ")"
    (Gt a b) -> ">(" ++ show a ++ " " ++ show b ++ ")"
    (Ng a) -> "!(" ++ show a ++ ")"
    (Cj cs) -> "&&(" ++ unwords (map show cs) ++ ")"
    (Dj cs) -> "||(" ++ unwords (map show cs) ++ ")"

instance Show Statement where 
  show s = case s of 
    (Ass v e) ->  "=(" ++ v ++ " " ++ (show e) ++ ")"
    (Cond c t f) -> "if(" ++ (show c) ++ " " ++ (show t) ++ " " ++ (show f) ++ ")"
    (Seq sts) -> "{" ++ unwords (map show sts) ++ "}"
    (While c b) -> "while(" ++ (show c) ++ " " ++ (show b) ++ ")"

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

instance Read Expr where 
  readsPrec d r = case r of 
    '+':'(':r1 -> read2Close (\e1 e2 r -> [(Add e1 e2, r)]) d r1
    '-':'(':r1 -> read2Close (\e1 e2 r -> [(Subtr e1 e2, r)]) d r1
    '*':'(':r1 -> read2Close (\e1 e2 r -> [(Mult e1 e2, r)]) d r1
    (x:_) -> 
      if isAlpha x then 
        let (v,r1) = readVar r in [(V v, r1)]
      else 
        let (i,r1):[] = readsPrec d r in [(I i, r1)]

prop_ExprReadShowIsEqual :: Expr -> Bool
prop_ExprReadShowIsEqual e = e == (read $ show e)

instance Read Condition where 
  readsPrec d r =  case r of 
    '=':'=':'(':r1 -> read2Close (\e1 e2 r -> [(Eq e1 e2, r)]) d r1
    '<':'(':r1 -> read2Close (\e1 e2 r -> [(Lt e1 e2, r)]) d r1
    '>':'(':r1 -> read2Close (\e1 e2 r -> [(Gt e1 e2, r)]) d r1
    '!':'(':r1 -> case readsPrec d r1 of 
      (c,r2):[] -> case r2 of 
        ')':r3 -> [(Ng c, r3)] 
    '&':'&':'(':r1 -> 
      case readConditions r1 [] of 
        [] -> case r1 of 
          ')':r2 -> [(Cj [], r2)]
        (cs,r2):[] -> case r2 of 
          ')':r3 -> [(Cj cs, r3)]
    '|':'|':'(':r1 -> 
      case readConditions r1 [] of 
        [] -> case r1 of 
          ')':r2 -> [(Dj [], r2)]
        (cs,r2):[] -> case r2 of 
          ')':r3 -> [(Dj cs, r3)]
    x:_ -> 
      if isAlpha x then 
        let (v,r1) = readVar r in [(Prp v, r1)]
      else []
    where
      readConditions r cs = case readsPrec d r of 
        [] -> []
        (c,r1):[] -> 
          let cs2 = cs ++ [c]
          in case r1 of 
            ' ':r2 -> readConditions r2 cs2
            otherwise -> [(cs2, r1)]

prop_ConditionReadShowIsEqual :: Condition -> Bool
prop_ConditionReadShowIsEqual e = e == (read $ show e)

instance Read Statement where 
  readsPrec d r = case r of 
    '=':'(':r1 -> let (v,r2) = readVar r1 in case r2 of 
      ' ':r3 -> case readsPrec d r3 of 
        (e,r4):[] -> case r4 of
          ')':r5 -> [(Ass v e, r5)]
    'i':'f':'(':r1 -> case readsPrec d r1 of 
      (c,r2):[] -> case r2 of 
        ' ':r3 -> case readsPrec d r3 of 
          (s1,r4):[] -> case r4 of 
            ' ':r5 -> case readsPrec d r5 of 
              (s2,r6):[] -> case r6 of 
                ')':r7 -> [(Cond c s1 s2, r7)]
    '{':r1 -> case r1 of 
        '}':r2 -> [(Seq [], r2)] 
        otherwise -> let (ss, r2):[] = readStatements r1 [] in case r2 of 
          '}':r3 -> [(Seq ss, r3)]
    'w':'h':'i':'l':'e':'(':r1 -> read2Close (\c s r -> [(While c s,r)]) d r1
    where 
      readStatements r ss = case readsPrec d r of 
        (s,r1):[] -> 
          let ss2 = ss ++ [s]
          in case r1 of 
            ' ':r2 -> readStatements r2 ss2
            otherwise -> [(ss2, r1)]

prop_StatementReadShowIsEqual :: Statement -> Bool
prop_StatementReadShowIsEqual s = s == (read $ show s)

-- END Bonus - show function
-- Time taken 6h - this was too much time for a bonus exercise to be fun...
-- Tested using quickcheck and prop_*ReadShowIsEqual