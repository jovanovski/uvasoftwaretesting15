module Ex8 where

import Ex5
import Ex6


import Test.QuickCheck

testProp :: Rel Integer -> Bool
testProp x = trClos ( symClose x ) == symClose ( trClos x)

-- TEST concludes that there is a difference when symmetric closure is applied over a transitive closure, and the other way aroud
-- falsifiable test case Rel [(0,1)]