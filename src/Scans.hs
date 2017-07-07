module Scans where

import Data.List

-- experiment with multiple scans
sumList' :: Num a => [a] -> [a]
sumList' xs = scanl1 (+) xs

sumList'' :: Num a => [a] -> [a]
sumList'' xs = tail $ scanl (+) 0 xs

-- and same thing but using foldl
sumListfoldl :: Num a => [a] -> [a]
sumListfoldl xs = tail . reverse $ foldl acc [0] xs where
  acc (y:ys) x = (x+y):y:ys

-- and in pointfree
sumListfoldl_pf :: Num a => [a] -> [a]
sumListfoldl_pf = tail.reverse.foldl acc [0] where
  acc (y:ys) x = (x+y):y:ys

-- a less performant but more elegant
sumList_eleg :: Num a => [a] -> [a]
sumList_eleg xs = tail $ map sum $ inits xs

-- & in pointfree
sumList_eleg_pf :: Num a => [a] -> [a]
sumList_eleg_pf = tail.map sum.inits
