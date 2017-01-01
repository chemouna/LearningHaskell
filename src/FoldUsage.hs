
module FoldUsage where



-- | Problem #2
-- given an array of integers, the goal is to efficiently find the subarray that has the greatest value when all of its elements are summed together.
-- Note that because some elements of the array may be negative, the problem is not solved by simply picking the start and end elements of the array to
-- be the subarrray, and summing the entire array. For example, given the array
-- {1, 2, -5, 4, -3, 2}
-- The maximum sum of a subarray is 4. It is possible for the subarray to be zero elements in length (if every element of the array were negative).
mss [] = 0
mss xs =  maximum [foldr1 (+) xs, mss (tail xs), mss (init xs)]

mss' = maximum [foldr (+), mss $ tail, mss $ init]

