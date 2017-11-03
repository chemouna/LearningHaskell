
module ProductOfOtherNumbers where

{--
Given an array of numbers, nums, return an array of numbers products, where products[i] is the product of all nums[j], j != i.

Input : [1, 2, 3, 4, 5]
Output: [(2*3*4*5), (1*3*4*5), (1*2*4*5), (1*2*3*5), (1*2*3*4)]
      = [120, 60, 40, 30, 24]
You must do this in O(N) without using division.
--}

otherProducts xs = zipWith (*) above below
  where
    above = scanl (*) 1 $ init xs
    below = tail $ scanr (*) 1 xs
