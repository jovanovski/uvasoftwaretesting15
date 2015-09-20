module ToCnfTest where 

import Data.List
import Lecture3
import Test.QuickCheck
import Control.Monad
import LogicalFunctions
import ToCnf

-- START test toCnf

instance Arbitrary Form where
  arbitrary = sized arbForm

arbForm :: Int -> Gen Form
arbForm 0 = arbProp
arbForm n = oneof [
    arbProp, 
    liftM Neg subform, 
    liftM Cnj (listOf (resize 5 subform)),
    liftM Dsj (listOf (resize 5 subform)),
    liftM2 Impl subform subform,
    liftM2 Equiv subform subform
  ] where 
  subform = arbForm (n `div` 2)

-- generator for properties 1 - 5
arbProp :: Gen Form
arbProp = do
  a <- resize 4 arbitrary
  return $ Prop ((abs a) + 1)

isCnf :: Form -> Bool 
isCnf (Cnj fs) = all isCnfClause fs
isCnf f = isCnfClause f

isCnfClause :: Form -> Bool
isCnfClause (Dsj fs) = all isCnfLiteral fs
isCnfClause f = isCnfLiteral f

isCnfLiteral :: Form -> Bool
isCnfLiteral (Prop _) = True
isCnfLiteral (Neg (Prop _)) = True
isCnfLiteral _ = False

-- Test property - Forms returned by toCnf should be logically equivalent 
prop_ToCnfIsEquiv :: Form -> Bool
prop_ToCnfIsEquiv f = equiv f (toCnf f)

-- Test property - Forms returned by toCnf should be in Cnf form
prop_ToCnfIsCnf :: Form -> Bool
prop_ToCnfIsCnf f = isCnf (toCnf f)

-- END test toCnf 
-- Time spent: 2h, tested using quickCheck for properties `prop_ToCnfIsEquiv` & `prop_ToCnfIsCnf`
