module Cnf2Cls where 

import Data.List
import Lecture3
import Test.QuickCheck
import ToCnf
import ToCnfTest

-- START bonus - resolutions style theorem proving

type Clause = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Cnj fs) = map cnfClause2cls fs
cnf2cls f = [cnfClause2cls f]

cnfClause2cls :: Form -> Clause
cnfClause2cls (Dsj fs) = map cnfNeg2Cls fs
cnfClause2cls f = [cnfNeg2Cls f]

cnfNeg2Cls :: Form -> Int 
cnfNeg2Cls (Neg f) = - (cnfLiteral2Cls f)
cnfNeg2Cls f = cnfLiteral2Cls f

cnfLiteral2Cls :: Form -> Int
cnfLiteral2Cls (Prop p) 
  | p == 0 = error "properties cannot have name 0"
  | otherwise = p
cnfLiteral2Cls _ = error "not cnf"

-- dual of evl, but for Clauses
evlClauses :: Valuation -> Clauses -> Bool
evlClauses v cs = all (evlClause v) cs

evlClause :: Valuation -> Clause -> Bool
evlClause v c = any (evlLiteral v) c 

evlLiteral :: Valuation -> Int -> Bool
evlLiteral [] l = error ("no info for " ++ show l)
evlLiteral ((i,b):xs) l
  | abs l == i = if l > 0 then b else not b
  | otherwise = evlLiteral xs l

propNamesClauses :: Clauses ->  [Int]
propNamesClauses cs = nub (foldr (++) [] (map propNamesClause cs))

propNamesClause :: Clause -> [Int]
propNamesClause c = map abs c

containsProp0 :: Form -> Bool
containsProp0 (Prop p) = p == 0
containsProp0 (Neg f) = containsProp0 f
containsProp0 (Cnj fs) = any containsProp0 fs
containsProp0 (Dsj fs) = any containsProp0 fs
containsProp0 (Impl f g) = containsProp0 f || containsProp0 g
containsProp0 (Equiv f g) = containsProp0 f || containsProp0 g

-- Test property - CNF forms returned by cnf2cls should be logically equivalent to original
prop_Cnf2ClsFromCnfIsEquiv f = not (containsProp0 f) ==> all (\v -> evl v f == evlClauses v (cnf2cls (toCnf f))) (allVals f)

-- Test property - cnf2cls retains same property names as cnf formula
prop_Cnf2ClsSameProps f = not (containsProp0 f) ==> 
  let fcnf = (toCnf f)
  in sort (propNames fcnf) == sort (propNamesClauses (cnf2cls fcnf))

-- END bonus 
-- Time spent: 3 h, tested using quickCheck for properties `prop_Cnf2ClsFromCnfIsEquiv` & `prop_Cnf2ClsSameProps`