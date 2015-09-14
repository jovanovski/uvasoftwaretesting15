module Ex3 where

import Lecture3
import Test.QuickCheck

cnf :: String -> Form
cnf xs = Cnj (cnfForm(flipVals (falseVals (allVals(parse xs!!0)) (parse xs!!0))))

cnfForm :: [Valuation] -> [Form]
cnfForm [] = []
cnfForm (x:xs) = getDsj(mDOV x) : cnfForm xs

falseVals :: [Valuation] -> Form -> [Valuation]
falseVals [] _ = []
falseVals (x:xs) f = if (evl x f) then falseVals xs f else x: falseVals xs f

flipVals :: [Valuation] -> [Valuation]
flipVals [] = []
flipVals (x:xs) = flipVal x : flipVals xs

flipVal :: Valuation -> Valuation
flipVal [] = []
flipVal ((i, b):xs) = (i, not b) : flipVal xs

getDsj :: [Form] -> Form
getDsj xs = Dsj xs

mDOV :: Valuation -> [Form]
mDOV [] = []
mDOV ((i, b):xs) = if b then (Prop i) : mDOV xs else Neg (Prop i) :mDOV xs

	-- | flipVals (falseVals (allVals(parse "*(1 2)"!!0)) (parse "*(1 2)"!!0))
	-- | take forumula
	-- | parse formula
	-- | find truth values
	-- | find pairs for which eval is false
	-- | create NOT p or NOT q or NOT r ...
	-- | create conjuction of them