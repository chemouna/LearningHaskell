module Sierpinski where

import Data.Bits
import Data.Bool
import Data.Numbers.Primes

-- plotting set {{i OR j, i AND j) | i <- [0..n], j <- [0..n]}}
f n = [(i .|. j, i .&. j) | i <- [0..n], j <- [0..n]]
plot n = unlines [[bool ' ' '*' $ (r, c) `elem` f n |
  c <- [0..n]] | r <- [0..n]]

main = putStr $ plot (20 :: Int)

-- using pascal triangle

choose n 0 = 1
choose n k
  | n < k     = 0
  | otherwise = choose (n - 1) (k - 1) + choose (n - 1) k

plot' n = unlines [[bool ' ' '*' $ odd $ choose r c |
   c <- [0..n]] | r <- [0..n]]

main' = putStr $  plot (20 :: Int)


-- A more efficient solution from https://crypto.stanford.edu/~blynn/pr/choose.html
f p n 0 = 0
f p n k | k > n - k = f p n (n - k)
        | p > n - k = 1
        | 2 * p > n = 0
        | p * p > n = fromEnum $ cn < ck
        | cn >= ck  =     f p n'       k'
        | bn /= bk  = 1 + f p n'       k'
        | bn /= 0   = 1 + f p (n' - 1) k'
        | otherwise = 1 + f p n'       (k' + 1)
        where
          (n', cn) = n  `divMod` p
          bn       = n' `mod` p
          (k', ck) = k  `divMod` p
          bk       = k' `mod` p

choose' n k = product $ map (\p -> p^f p n k) $ takeWhile (<= n) primes

main'' = print $ choose' 1000000 353000

