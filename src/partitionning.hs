module Partitionning where

import Data.List.Split

-- splitPlaces: Splits a list into chunks of the given lengths. For example:



{--
 Partitioning a list fairly :
 group a list into a list of lists of N elements each, with the final sublist being smaller than N
 if the length of the list is not evenly divisible by N
--}

-- Using splitPlaces from split package 
fairPartition n xs = case length xs `quotRem` n of
    (q, r) -> splitPlaces (replicate r (q+1) ++ replicate (n-r) q) xs
