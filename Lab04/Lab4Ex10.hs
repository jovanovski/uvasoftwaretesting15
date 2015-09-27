module Lab4Ex10ExtraBonus where

-- START Extra Bonus - Euler 50

primesbelow :: Integer -> [Integer]
primesbelow n = sievebelow [2..] where
  sievebelow (x:xs) = if x^2 > n then x:takeWhile (< n) xs else x:(sievebelow [y | y <- xs, y `mod` x /= 0])

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
