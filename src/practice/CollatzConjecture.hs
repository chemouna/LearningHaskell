module CollatzConjecture where

import Data.List (map, unfoldr)
import Test.QuickCheck.All
import Test.QuickCheck
import Data.Array
import qualified Data.MemoTrie as Mt
import qualified Data.MemoCombinators as Memo

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


collatzProp :: Int -> Bool
collatzProp(1) = True
collatzProp(n) = collatzProp(collatz n)

-- generator that only yields positive values so
-- that we don't generate useless negative values

positives :: Gen Int
positives = do
  x <- arbitrary -- pick an arbitrary int
  -- make it positive
  if(x == 0)
    then return 1
  else if (x < 0)
    then return (-x)
  else
    return x

-- verboseCheck (forAll positives collatzProp)


-- Haskell solution with caching
cacheSize :: Int
cacheSize = 65536

table :: Array Int Int
table = listArray (1, cacheSize) (1 : map go [2..cacheSize])
  where
    go n
      | even n = 1 + lookupCollatz (n `div` 2)
      | otherwise = 1 + lookupCollatz (3 * n + 1)

lookupCollatz :: Int -> Int
lookupCollatz n
  | n < cacheSize = table ! n
  | even n = 1 + lookupCollatz (n `div` 2)
  | otherwise = 1 + lookupCollatz (3 * n + 1)


collatzProp2 :: Int -> Bool
collatzProp2 1 = True
collatzProp2 n = collatzProp (lookupCollatz n)

--  verboseCheck (forAll positives collatzProp2)


-- Using MemoCombinators for caching

collatzMemo :: Int -> Int
collatzMemo = Memo.integral go
  where
    go 1 = 1
    go n
      | even n = 1 + collatzMemo (n `div` 2)
      | otherwise = 1 + collatzMemo (3 * n + 1)

collatzProp3 :: Int -> Bool
collatzProp3 1 = True
collatzProp3 n = collatzProp (collatzMemo n)

-- verboseCheck (forAll positives collatzProp3)
 
