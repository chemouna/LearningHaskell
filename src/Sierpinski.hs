
module Sierpinski where

import Data.Bits
import Data.Bool


-- plotting set {{i OR j, i AND j) | i <- [0..n], j <- [0..n]}}
f n = [(i .|. j, i .&. j) | i <- [0..n], j <- [0..n]]
plot n = unlines [[bool ' ' '*' $ (r, c) `elem` f n |
  c <- [0..n]] | r <- [0..n]]

main = putStr $ plot (100 :: Int)
