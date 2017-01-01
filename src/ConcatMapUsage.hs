
module ConcatMapUsage where

import Data.List

-- | Problem #1 :  Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs


-- | Problem #2
-- given an array of integers, the goal is to efficiently find the subarray that has the greatest value when all of its elements are summed together.
-- Note that because some elements of the array may be negative, the problem is not solved by simply picking the start and end elements of the array to
-- be the subarrray, and summing the entire array. For example, given the array
-- {1, 2, -5, 4, -3, 2}
-- The maximum sum of a subarray is 4. It is possible for the subarray to be zero elements in length (if every element of the array were negative).

find_subarray_greatest_sum = maximum . concatMap (scanl1 (+)) . scanr (:) []

-- find_subarray_greatest_sum [1, 2, -5, 4, -3, 2] == 4

-- | Project Euler #38
-- Take the number 192 and multiply it by each of 1, 2 and 3:
-- 192 × 1 = 192
-- 192 × 2 = 384
-- 192 × 3 = 576
-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 --   and (1,2,3)
-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with
-- (1,2, ... , n) where n > 1?

solve = maximum $ concatMap largestPandigital [2..9]

largestPandigital n = filter isPandigital $ map (flip cp [1..n]) [1..10^(9 `div` n + 1)]

isPandigital = (== "123456789") . sort
cp x = concatMap (show . (*x))

