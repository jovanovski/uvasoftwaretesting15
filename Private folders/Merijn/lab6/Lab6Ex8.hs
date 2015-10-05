module Lab6Ex8 where

import Lecture6
import System.Random
import Lab6Ex1
import Lab6Ex6
import Control.Monad

randomofbitlength :: Int -> IO (Integer)
randomofbitlength l = randomRIO (2^(l-1)+1, 2^l)

primeofbitlength :: Int -> Int -> IO (Integer)
primeofbitlength k l = do
  c <- randomofbitlength l
  b <- primeMR2 k c
  if b then return c else primeofbitlength k l

primepairsofbitlength :: Int -> Int -> IO (Integer, Integer)
primepairsofbitlength k l = liftM2 (\p q -> (p,q)) (primeofbitlength k l) (primeofbitlength k l)

keysofbitlength :: Int -> Int -> IO ((Integer, Integer),(Integer, Integer))
keysofbitlength k l = liftM (\(p,q) -> (rsa_public p q, rsa_private p q)) (primepairsofbitlength k l)

rsa_encode2 :: (Integer,Integer) -> Integer -> Integer
rsa_encode2 (e,n) =  \ m -> exM2 m e n

rsa_decode2 :: (Integer,Integer) -> Integer -> Integer
rsa_decode2 = rsa_encode2

prop_encodeDecodeIdentity :: (Integer,Integer) -> (Integer,Integer) -> Integer -> Bool
prop_encodeDecodeIdentity pub pri m = rsa_decode2 pri (rsa_encode2 pub m) == m

 -- k to use in testKeys, can be set higher to
testK :: Int
testK = 10
-- prime bit length to use in key generation
testBitLength :: Int
testBitLength = 24

testKeys :: Integer -> IO ()
testKeys 0 = print "All passed"
testKeys n = do
  (pub,pri) <- keysofbitlength testK testBitLength
  m <- randomRIO (0, 1000000)
  let s = (show pub) ++ " " ++ (show pri) ++ " " ++ (show m)
  if prop_encodeDecodeIdentity pub pri m
  then do
    print $ "Passed on " ++ s
    testKeys (n-1)
  else error $ "Failed on " ++ s

-- Tested using 'testKeys n' where n is the number of tests to run, important
-- test inputs can be configured with testK and testBitLength.
