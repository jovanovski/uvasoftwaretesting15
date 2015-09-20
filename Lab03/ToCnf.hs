module ToCnf where 

import Lecture3
import Testing

-- START convert to CNF

toCnf :: Form -> Form
toCnf f = Cnj [ Dsj [if v then Neg (Prop n) else Prop n | (n,v) <- vs] | vs <- allVals f, evl vs f == False]

-- END convert to CNF 
-- Time spent: 3h, of which 2,5h trying a method without truth table conversion, which turned out to be way too hard...
