
module PointFreeUsage where

import Control.Monad
import Control.Monad.Fix

-- | Problem:
-- given an array of integers, the goal is to efficiently find the subarray that has the greatest value when all of its elements are summed together.
-- has this solution
mss [] = 0
mss xs =  maximum [foldr1 (+) xs, mss (tail xs), mss (init xs)]

-- let's try to convert this to point free
mss' = fix ((maximum .) . ap ((:) . foldr (+) 0) . ap (ap . ((:) .) . (. tail)) (flip flip ([]) . ((:) .) . (. init)))

