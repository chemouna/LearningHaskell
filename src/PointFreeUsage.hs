
module PointFreeUsage where

import Control.Monad
import Control.Monad.Fix


-- | Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- to convert it to pointfree pointFree version
repli_pf xs n = replicate n =<< xs
repli_pf' = (. replicate) . (>>=)


-- | Problem:
-- given an array of integers, the goal is to efficiently find the subarray that has the greatest value when all of its elements are summed together.
-- has this solution
mss [] = 0
mss xs =  maximum [foldr1 (+) xs, mss (tail xs), mss (init xs)]

-- let's try to convert this to point free
mss' = fix ((maximum .) . ap ((:) . foldr (+) 0) . ap (ap . ((:) .) . (. tail)) (flip flip ([]) . ((:) .) . (. init)))

