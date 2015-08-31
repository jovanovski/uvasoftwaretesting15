{-# OPTIONS_GHC -Wall #-}
module Stolen where

data Boy = Matthew | Peter | Jack | Arnold | Carl
	deriving (Eq, Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

--SM = not Carl and not Matthew
--SP = Matthew or Jack
--SJ = not SM and not SP
--SA = (SM and not SP) or (not SM and SP)
--SC = not SA

--says :: Boy -> Boy -> Bool


infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

infix 1 ==>
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y


m = True
p = False
j = False
a = False
c = False

ms = (not c) && (not m)
ps = m || j
js = (not ms) && (not ps)
as = (ms && (not ps)) || ((not ms) && ps)
cs = (not as)