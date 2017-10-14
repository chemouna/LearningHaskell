module CollatzConjecture where

import Data.List

threePlus1 :: Int -> Int -> Int
threePlus1 i j = maximum $ map (length . collatzSequence) [i..j]

collatzSequence :: Int -> [Int]
collatzSequence = terminate . iterate collatz
  where
    terminate (1:_) = [1]
    terminate (x:xs) = x : terminate xs

collatz :: Int -> Int
collatz n
  | n == 1 = 1
  | n `mod` 2 == 0 = (n `div` 2)
  | otherwise =  (3 * n + 1)


-- Solution 2

threePlus1_sol2 i j = maximum $ map (length . collatzSequence2) [i..j]

collatzSequence2 :: Int -> [Int]
collatzSequence2 = unfoldr collatz2 
  where collatz2 0 = Nothing
        collatz2 1 = Just (1, 0)
        collatz2 x
          | odd x = Just (x, 3 * x + 1)
          | otherwise = Just (x, x `div` 2)
