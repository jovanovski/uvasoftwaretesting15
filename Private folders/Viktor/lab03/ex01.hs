module Lab3 where
 
import Data.List
import System.Random
import Lecture3


{--
data Form = Prop Name
>           | Neg  Form
>           | Cnj [Form]
>           | Dsj [Form]
>           | Impl Form Form 
>           | Equiv Form Form 
>           deriving Eq

--}

-- doesnt apply at least for one item (value)
contradiction :: Form -> Bool
contradiction f =  any (\ v -> not (evl v f)) (allVals f)

-- must be always true for all elements (values)
-- tautology (Impl (Prop 1) (Impl (Prop 2) (Prop 1)))
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- do cartesian product of all values of firt and second form
allVals2 :: Form -> Form -> [Valuation]
allVals2 p q = [pv++qv | pv <- allVals p, qv <- allVals q]

-- in a sense of logic, we can rewrite impl into disjunction as:
-- p -> q == not q || p
entails :: Form -> Form -> Bool
entails p q = all (\ v -> not (evl v q) || (evl v p)) (allVals2 p q)

-- p <=> q == p -> q && q -> p
-- the entails function is used
equiv :: Form -> Form -> Bool
equiv p q =  (entails p q) && (entails q p) 

{--
-- REPORT
estimation time 15min, real time 30 min
takes longer to understand what evl does

--}