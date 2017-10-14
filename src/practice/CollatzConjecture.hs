module CollatzConjecture where

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


